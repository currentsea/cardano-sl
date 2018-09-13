module Cardano.Wallet.Kernel.Migration (migrateAcid) where

import           Universum

import           Control.Lens (review)
import           Control.Lens.TH
import qualified Data.HashMap.Strict as HM
import           Data.Text (pack)
import           System.Directory (doesDirectoryExist, renamePath)

import qualified Pos.Core as Core
import           Pos.Util.Wlog (Severity (..))
import qualified Pos.Wallet.Web.ClientTypes as WebTypes
import qualified Pos.Wallet.Web.State.Storage as WS

import           Pos.Wallet.Web.State.Acidic (openState)
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

data MigrationMetadata = MigrationMetadata {
    _mmHdRoot         :: HD.HdRoot
  , _mmDefaultAddress :: Core.Address
  -- ^ A default Address which will be used during restoration. It's a trick
  -- to avoid having the user insert the @spending password@ during migration,
  -- which would complicate things sensibly.
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
          return $ MigrationMetadata hdRoot addr

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


migrateAcid :: (Severity -> Text -> IO ())
            -> Kernel.PassiveWallet
            -> FilePath
            -> Keystore
            -> IO ()
migrateAcid logMsg pw filepath keystore = do
    logMsg Info "starting acid state migration"
    storageM <- openLegacydb filepath
    case storageM of
        Nothing ->
            logMsg Warning $ "couldn`t find legacy db at " <> pack filepath
        Just st -> do
            let  (unavailable, available) = partitionEithers (metadataFromWalletStorage st)
                 unavailableLen = length unavailable
            when (unavailableLen > 0) $ do
                logMsg Error $ show unavailableLen
                    <> " out of "
                    <> show unavailableLen
                    <> " rootAddress(es) failed to decode"

            mapM_ (restore pw keystore) available

            backupPath <- moveLegacyDB filepath

            -- asynchronous restoration still runs at this point.
            logMsg Info $ "acid state migration succeeded. Old db backup can be found at " <> pack backupPath

restore :: Kernel.PassiveWallet -> Keystore -> MigrationMetadata -> IO ()
restore pw keystore metadata = do
    let wId = WalletIdHdRnd (metadata ^. mmHdRoot . HD.hdRootId)
    mEsk <- Keystore.lookup wId keystore
    case mEsk of
        Just esk -> do
            _ <- restoreWallet pw
                               False
                               (metadata ^. mmDefaultAddress)
                               (metadata ^. mmHdRoot . HD.hdRootName)
                               (metadata ^. mmHdRoot . HD.hdRootAssurance)
                               esk
                               (prefilter esk pw wId)
            return ()
        Nothing ->
            return ()

openLegacydb :: FilePath -> IO (Maybe WS.WalletStorage)
openLegacydb filepath = do
    exists <- doesDirectoryExist filepath
    case exists of
        False -> return Nothing
        True  -> do
            -- here we assume that the old db file won`t be deleted in the meantime.
            db <- liftIO $ openState False filepath
            Just <$> liftIO (getWalletSnapshot db)

moveLegacyDB :: FilePath -> IO FilePath
moveLegacyDB filepath = go 0
  where
    go :: Int -> IO FilePath
    go n = do
        let backupPath = filepath <> "-backup-" <> show n
        exists <- doesDirectoryExist backupPath
        case exists of
            True  -> go $ n + 1
            False -> do
                renamePath filepath backupPath
                return backupPath

