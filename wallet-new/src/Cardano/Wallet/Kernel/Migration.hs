module Cardano.Wallet.Kernel.Migration (migrateLegacyDataLayer) where

import           Universum

import           Control.Lens (review)
import           Control.Lens.TH
import qualified Data.HashMap.Strict as HM
import           Data.Text (pack)
import           Data.Time (defaultTimeLocale, formatTime, getCurrentTime,
                     iso8601DateFormat)
import           System.Directory (doesDirectoryExist, makeAbsolute, renamePath)

import           Formatting ((%))
import qualified Formatting as F

import qualified Pos.Core as Core
import           Pos.Util.Wlog (Severity (..))
import qualified Pos.Wallet.Web.ClientTypes as WebTypes
import qualified Pos.Wallet.Web.State.Storage as WS

import           Pos.Wallet.Web.State.Acidic (closeState, openState)
import           Pos.Wallet.Web.State.State (getWalletSnapshot)

import qualified Cardano.Wallet.Kernel as Kernel
import qualified Cardano.Wallet.Kernel.DB.HdWallet as HD
import           Cardano.Wallet.Kernel.DB.InDb (InDb (InDb))
import           Cardano.Wallet.Kernel.Keystore as Keystore
import           Cardano.Wallet.Kernel.Restore (restoreWallet)
import           Cardano.Wallet.Kernel.Types
import           Cardano.Wallet.WalletLayer.Kernel.Wallets (prefilter)

{-------------------------------------------------------------------------------
  Pure helper functions for migration. This include only data that are not
  derivable from the blockchain.
-------------------------------------------------------------------------------}

data ConfidenceScore =
    LowConfidence
  | HighConfidence

data MigrationMetadata = MigrationMetadata {
    _mmHdRoot                       :: HD.HdRoot
  , _mmDefaultAddress               :: Core.Address
  -- ^ A default Address which will be used during restoration. It's a trick
  -- to avoid having the user insert the @spending password@ during migration,
  -- which would complicate things sensibly.
  , _mmHasSpendingPasswordHeuristic :: ConfidenceScore
  -- ^ An heuristic measure of whether or not a spending password was set for
  -- this wallet. Note how is not possible to reconstruct this information
  -- reliably, because the legacy wallet never stored if this wallet was
  -- spending-password-protected, it only stored the creation time and the
  -- last time the password was changed. However, when creating a wallet, they
  -- did set the lastUpdate == dateCreated, so it's not possible to distinguish
  -- between the case where the user set the password at creation time and
  -- never changed it or the case where the user never set the password at all.
  }

makeLenses ''MigrationMetadata

data MetadataUnavailable = NotEnoughData

-- | Obtain any interesting metadata necessary to migrate a wallet from
-- the legacy 'WS.WalletStorage'.
metadataFromWalletStorage :: WS.WalletStorage
                          -> [Either MetadataUnavailable MigrationMetadata]
metadataFromWalletStorage ws =
    let allWalletInfos = HM.toList (WS._wsWalletInfos ws)
    in map extract allWalletInfos
  where
      extract :: (WebTypes.CId (WebTypes.Wal), WS.WalletInfo)
              -> Either MetadataUnavailable MigrationMetadata
      extract (wId, wInfo) = do
          hdRoot <- extractHdRoot  (wId, wInfo)
          addr   <- extractAddress (wId, wInfo)
          let hasSpendingPassword = extractHasSpendingPassword wInfo
          return $ MigrationMetadata hdRoot addr hasSpendingPassword

      extractHdRoot :: (WebTypes.CId (WebTypes.Wal), WS.WalletInfo)
                    -> Either MetadataUnavailable HD.HdRoot
      extractHdRoot (cwalId, wi) = do
          let wMeta = WS._wiMeta wi :: WebTypes.CWalletMeta
              (WebTypes.CId (WebTypes.CHash t)) = cwalId
          rootAddr <- bimap (const NotEnoughData) identity (Core.decodeTextAddress t)
          pure $ HD.HdRoot {
                 HD._hdRootId = HD.HdRootId . InDb $ rootAddr
               , HD._hdRootName = HD.WalletName (WebTypes.cwName wMeta)
               , HD._hdRootHasPassword =
                   HD.HasSpendingPassword $
                       InDb (review Core.timestampSeconds (WS._wiPassphraseLU wi))
               , HD._hdRootAssurance = case WebTypes.cwAssurance wMeta of
                     WebTypes.CWAStrict -> HD.AssuranceLevelStrict
                     WebTypes.CWANormal -> HD.AssuranceLevelNormal
               , HD._hdRootCreatedAt =
                   InDb (review Core.timestampSeconds (WS._wiCreationTime wi))
               }

      extractAddress :: (WebTypes.CId (WebTypes.Wal), WS.WalletInfo)
                     -> Either MetadataUnavailable Core.Address
      extractAddress (cwalId, _) =
        findSuitableAddress cwalId (HM.toList (WS._wsAccountInfos ws))

      findSuitableAddress _ [] = Left NotEnoughData
      findSuitableAddress cwalId ((WebTypes.AccountId aiWId _index, acc) : xs)
          | aiWId /= cwalId = findSuitableAddress cwalId xs
          | otherwise =
              -- Optimisation: we might want to prefer using unused addresses
              -- rather than any address. However, since the pool of addresses
              -- can be extremely big, especially for exchanges, it's better to
              -- do as little work as possible.
              case HM.keys . WS._aiAddresses $ acc of
                   []    -> findSuitableAddress cwalId xs
                   (a:_) -> Right a

      extractHasSpendingPassword :: WS.WalletInfo -> ConfidenceScore
      extractHasSpendingPassword wi
          | WS._wiPassphraseLU wi /= WS._wiCreationTime wi = HighConfidence
          | otherwise = LowConfidence


-- | Tries to check the existence of the DB at 'FilePath'. It first tries to
-- check for the input 'FilePath' as-it-is. If this fails, it tries to check
-- for absolute path.
resolveDbPath :: FilePath -> IO (Maybe FilePath)
resolveDbPath fp = do
    exists <- doesDirectoryExist fp
    case exists of
         True  -> return (Just fp)
         False -> do
             absPath  <- makeAbsolute fp
             absExist <- doesDirectoryExist absPath
             return $ if absExist then Just absPath else Nothing


-- | Migrates the wallet database created with the legacy data layer onto the
-- new format. It does that by extract the metadata not deriveable from the
-- blockchain first, and then kicking-off the async restoration process.
migrateLegacyDataLayer :: (Severity -> Text -> IO ())
                       -> Kernel.PassiveWallet
                       -> FilePath
                       -> Keystore
                       -> IO ()
migrateLegacyDataLayer logMsg pw unresolvedDbPath keystore = do
    logMsg Info "Starting acid state migration"
    resolved <- resolveDbPath unresolvedDbPath
    case resolved of
       Nothing -> -- We assume no migration is needed and we move along
           logMsg Info $ "No legacy DB at " <> pack unresolvedDbPath <> " , migration is not needed."
       Just legacyDbPath -> do
           bracketLegacyDB legacyDbPath $ \st -> do
                let  (unavailable, available) = partitionEithers (metadataFromWalletStorage st)
                     unavailableLen = length unavailable
                     availableLen   = length available

                when (unavailableLen > 0) $ do
                    logMsg Error $ show unavailableLen
                        <> " out of "
                        <> show unavailableLen
                        <> " rootAddress(es) failed to decode"

                when (availableLen > 0) $ do
                    logMsg Info $ "Found "
                        <> show availableLen
                        <> " rootAddress(es) to migrate."

                mapM_ (restore logMsg pw keystore) available

           -- Now that we have closed the DB, we can move the directory
           backupPath <- moveLegacyDB legacyDbPath

           -- asynchronous restoration still runs at this point.
           logMsg Info $ "acid state migration succeeded. Old db backup can be found at " <> pack backupPath


restore :: (Severity -> Text -> IO ())
        -> Kernel.PassiveWallet
        -> Keystore
        -> MigrationMetadata
        -> IO ()
restore logMsg pw keystore metadata = do
    let wId = WalletIdHdRnd (metadata ^. mmHdRoot . HD.hdRootId)
        -- Our best guess whether or not this wallet has the spending password.
        -- currently we always yield True, but we can tweak these values if
        -- our assumptions reveal not to be correct (or if we can fine tune
        -- our heuristic).
        bestGuess = case metadata ^. mmHasSpendingPasswordHeuristic of
                         LowConfidence  -> True
                         HighConfidence -> True
    mEsk <- Keystore.lookup wId keystore
    case mEsk of
        Just esk -> do
            res <- restoreWallet pw
                                 bestGuess
                                 (metadata ^. mmDefaultAddress)
                                 (metadata ^. mmHdRoot . HD.hdRootName)
                                 (metadata ^. mmHdRoot . HD.hdRootAssurance)
                                 esk
                                 (prefilter esk pw wId)
            case res of
                 Right (restoredRoot, balance) -> do
                     let errMsg = "Migrating " % F.build
                                % " with balance " % F.build
                     logMsg Info (F.sformat errMsg restoredRoot balance)
                 Left err -> do
                     let errMsg = "Couldn't migrate " % F.build
                                % " due to : " % F.build
                     logMsg Error (F.sformat errMsg wId err)
        Nothing -> do
            let errMsg = "Couldn't migrate " % F.build
                       % " : the key was not found in the keystore."
            logMsg Error (F.sformat errMsg wId)

-- PRECONDITION: The 'FilePath' should exist. This is checked at the call site.
bracketLegacyDB :: FilePath -> (WS.WalletStorage -> IO a) -> IO a
bracketLegacyDB legacyDbPath withWalletStorage =
    bracket (openState False legacyDbPath)
            closeState
            (\db -> getWalletSnapshot db >>= withWalletStorage)

-- | Move the legacy database into a backup directory.
moveLegacyDB :: FilePath -> IO FilePath
moveLegacyDB filepath = do
    now <- getCurrentTime
    let backupPath =  filepath
                   <> "-backup-"
                   <> formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S")) now
    renamePath filepath backupPath
    return backupPath

