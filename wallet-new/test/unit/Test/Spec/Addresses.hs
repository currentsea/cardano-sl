{-# LANGUAGE TypeApplications #-}
module Test.Spec.Addresses (spec, withFixture, Fixture(..)) where

import           Universum

import           Control.Lens (to)
import           Control.Monad.Except (runExceptT)
import           Data.Acid (update)
import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import           Formatting (build, sformat)
import           Servant.Server

import           Test.Hspec (Spec, describe, runIO, shouldBe, shouldSatisfy)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (arbitrary, choose, generate, withMaxSuccess)
import           Test.QuickCheck.Monadic (PropertyM, monadicIO, pick)

import           Pos.Core (Address)
import           Pos.Core.NetworkMagic (NetworkMagic, RequiresNetworkMagic (..),
                     makeNetworkMagic)
import           Pos.Crypto (EncryptedSecretKey, safeDeterministicKeyGen)

import           Cardano.Wallet.API.V1.Handlers.Addresses as Handlers
import qualified Cardano.Wallet.API.V1.Types as V1
import qualified Cardano.Wallet.Kernel.Addresses as Kernel
import           Cardano.Wallet.Kernel.DB.AcidState
import           Cardano.Wallet.Kernel.DB.HdWallet (AssuranceLevel (..),
                     HasSpendingPassword (..), HdAccountId (..),
                     HdAccountIx (..), HdRootId (..), WalletName (..),
                     eskToHdRootId, hdAccountIdIx)
import           Cardano.Wallet.Kernel.DB.HdWallet.Create (initHdRoot)
import           Cardano.Wallet.Kernel.DB.HdWallet.Derivation
                     (HardeningMode (..), deriveIndex)
import           Cardano.Wallet.Kernel.DB.InDb (InDb (..), fromDb)
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet
import           Cardano.Wallet.Kernel.Internal (PassiveWallet, wallets)
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import qualified Cardano.Wallet.Kernel.Read as Kernel
import           Cardano.Wallet.Kernel.Types (AccountId (..), WalletId (..))
import           Cardano.Wallet.WalletLayer (PassiveWalletLayer)
import qualified Cardano.Wallet.WalletLayer as WalletLayer
import qualified Cardano.Wallet.WalletLayer.Kernel.Accounts as Accounts
import qualified Cardano.Wallet.WalletLayer.Kernel.Addresses as Addresses
import qualified Cardano.Wallet.WalletLayer.Kernel.Wallets as Wallets

import qualified Test.Spec.Fixture as Fixture
import qualified Test.Spec.Wallets as Wallets

import           Util.Buildable (ShowThroughBuild (..))

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

data Fixture = Fixture {
      fixtureHdRootId  :: HdRootId
    , fixtureESK       :: EncryptedSecretKey
    , fixtureAccountId :: AccountId
    , fixturePw        :: PassiveWallet
    }

data WithAddressFixture = WithAddressFixture {
    fixtureAddress :: V1.WalletAddress
    }

-- | Prepare some fixtures using the 'PropertyM' context to prepare the data,
-- and execute the 'acid-state' update once the 'PassiveWallet' gets into
-- scope (after the bracket initialisation).
prepareFixtures
    :: NetworkMagic
    -> Fixture.GenPassiveWalletFixture Fixture
prepareFixtures nm = do
    let (_, esk) = safeDeterministicKeyGen (B.pack $ replicate 32 0x42) mempty
    let newRootId = eskToHdRootId nm esk
    newRoot <- initHdRoot <$> pure newRootId
                          <*> pure (WalletName "A wallet")
                          <*> pure NoSpendingPassword
                          <*> pure AssuranceLevelNormal
                          <*> (InDb <$> pick arbitrary)
    newAccountId <- HdAccountId newRootId <$> deriveIndex (pick . choose) HdAccountIx HardDerivation
    let accounts = M.singleton newAccountId mempty
    return $ \pw -> do
        void $ liftIO $ update (pw ^. wallets) (CreateHdWallet newRoot accounts)
        return $ Fixture {
                           fixtureHdRootId = newRootId
                         , fixtureAccountId = AccountIdHdRnd newAccountId
                         , fixtureESK = esk
                         , fixturePw  = pw
                         }

prepareAddressFixture
    :: NetworkMagic
    -> Fixture.GenPassiveWalletFixture WithAddressFixture
prepareAddressFixture nm = do
    spendingPassword <- Fixture.genSpendingPassword
    newWalletRq <- Wallets.genNewWalletRq spendingPassword
    return $ \pw -> do
        Right v1Wallet <- Wallets.createWallet nm pw newWalletRq
        -- Get all the available accounts
        db <- Kernel.getWalletSnapshot pw
        let Right accs = Accounts.getAccounts (V1.walId v1Wallet) db
        let (acc : _) = IxSet.toList accs
        let newAddressRq = V1.NewAddress spendingPassword (V1.accIndex acc) (V1.walId v1Wallet)
        res <- Addresses.createAddress nm pw newAddressRq
        case res of
             Left e     -> error (show e)
             Right addr -> return (WithAddressFixture addr)

withFixture :: NetworkMagic
            -> (  Keystore.Keystore
               -> PassiveWalletLayer IO
               -> PassiveWallet
               -> Fixture
               -> IO a
               )
            -> PropertyM IO a
withFixture nm = Fixture.withPassiveWalletFixture nm (prepareFixtures nm)

withAddressFixture :: NetworkMagic
                   -> (  Keystore.Keystore
                      -> PassiveWalletLayer IO
                      -> PassiveWallet
                      -> WithAddressFixture
                      -> IO a
                      )
                   -> PropertyM IO a
withAddressFixture nm = Fixture.withPassiveWalletFixture nm (prepareAddressFixture nm)

spec :: Spec
spec = do
    go NMMustBeNothing
    go NMMustBeJust
  where
    go rnm = describe "Addresses" $ do
        pm <- runIO (generate arbitrary)
        let nm = makeNetworkMagic rnm pm
        describe "CreateAddress" $ do
            describe "Address creation (wallet layer)" $ do

                prop "works as expected in the happy path scenario" $ withMaxSuccess 200 $
                    monadicIO $ do
                        withFixture nm $ \keystore layer _ Fixture{..} -> do
                            Keystore.insert (WalletIdHdRnd fixtureHdRootId) fixtureESK keystore
                            let (HdRootId hdRoot) = fixtureHdRootId
                                (AccountIdHdRnd myAccountId) = fixtureAccountId
                                wId = sformat build (view fromDb hdRoot)
                                accIdx = myAccountId ^. hdAccountIdIx . to getHdAccountIx
                            res <- (WalletLayer._pwlCreateAddress layer) (V1.NewAddress Nothing accIdx (V1.WalletId wId))
                            (bimap STB STB res) `shouldSatisfy` isRight

            describe "Address creation (kernel)" $ do
                prop "works as expected in the happy path scenario" $ withMaxSuccess 200 $
                    monadicIO $ do
                        withFixture nm $ \keystore _ _ Fixture{..} -> do
                            Keystore.insert (WalletIdHdRnd fixtureHdRootId) fixtureESK keystore
                            res <- Kernel.createAddress nm mempty fixtureAccountId fixturePw
                            (bimap STB STB res) `shouldSatisfy` isRight

                prop "fails if the account has no associated key in the keystore" $ do
                    monadicIO $ do
                        withFixture nm $ \_ _ _ Fixture{..} -> do
                            res <- Kernel.createAddress nm mempty fixtureAccountId fixturePw
                            case res of
                                (Left (Kernel.CreateAddressKeystoreNotFound acc)) | acc == fixtureAccountId -> return ()
                                x -> fail (show (bimap STB STB x))

                prop "fails if the parent account doesn't exist" $ do
                    monadicIO $ do
                        withFixture nm $ \keystore _ _ Fixture{..} -> do
                            Keystore.insert (WalletIdHdRnd fixtureHdRootId) fixtureESK keystore
                            let (AccountIdHdRnd hdAccountId) = fixtureAccountId
                            void $ update (fixturePw ^. wallets) (DeleteHdAccount hdAccountId)
                            res <- Kernel.createAddress nm mempty fixtureAccountId fixturePw
                            case res of
                                Left (Kernel.CreateAddressUnknownHdAccount _) -> return ()
                                x -> fail (show (bimap STB STB x))

            describe "Address creation (Servant)" $ do
                prop "works as expected in the happy path scenario" $ do
                    monadicIO $
                        withFixture nm $ \keystore layer _ Fixture{..} -> do
                            Keystore.insert (WalletIdHdRnd fixtureHdRootId) fixtureESK keystore
                            let (HdRootId hdRoot) = fixtureHdRootId
                                (AccountIdHdRnd myAccountId) = fixtureAccountId
                                wId = sformat build (view fromDb hdRoot)
                                accIdx = myAccountId ^. hdAccountIdIx . to getHdAccountIx
                                req = V1.NewAddress Nothing accIdx (V1.WalletId wId)
                            res <- runExceptT . runHandler' $ Handlers.newAddress layer req
                            (bimap identity STB res) `shouldSatisfy` isRight

            describe "Address creation (wallet layer & kernel consistency)" $ do
                prop "layer & kernel agrees on the result" $ do
                    monadicIO $ do
                        res1 <- withFixture nm $ \keystore _ _ Fixture{..} -> do
                            Keystore.insert (WalletIdHdRnd fixtureHdRootId) fixtureESK keystore
                            Kernel.createAddress nm mempty fixtureAccountId fixturePw
                        res2 <- withFixture nm $ \keystore layer _ Fixture{..} -> do
                            Keystore.insert (WalletIdHdRnd fixtureHdRootId) fixtureESK keystore
                            let (HdRootId hdRoot) = fixtureHdRootId
                                (AccountIdHdRnd myAccountId) = fixtureAccountId
                                wId = sformat build (view fromDb hdRoot)
                                accIdx = myAccountId ^. hdAccountIdIx . to getHdAccountIx
                            (WalletLayer._pwlCreateAddress layer) (V1.NewAddress Nothing accIdx (V1.WalletId wId))
                        case res2 of
                            Left (WalletLayer.CreateAddressError err) ->
                                return $ (bimap STB STB res1) `shouldBe` (bimap STB STB (Left err))
                            Left (WalletLayer.CreateAddressAddressDecodingFailed _) ->
                                fail "Layer & Kernel mismatch: impossible error, CreateAddressAddressDecodingFailed"
                            Right _ -> do
                                -- If we get and 'Address', let's check that this is the case also for
                                -- the kernel run. Unfortunately we cannot compare the two addresses for equality
                                -- because the random index will be generated with a seed which changes every time
                                -- as we uses random, IO-based generation deep down the guts.
                                return $ (bimap STB STB res1) `shouldSatisfy` isRight

        describe "ValidateAddress" $ do
            describe "Address validation (wallet layer)" $ do

                prop "works as expected in the happy path scenario (valid address, ours)" $ withMaxSuccess 25 $
                    monadicIO $ do
                        withAddressFixture nm $ \_ layer _ WithAddressFixture{..} -> do
                            res <- WalletLayer.validateAddress layer (sformat build (V1.unV1 . V1.addrId $ fixtureAddress))
                            bimap STB STB res `shouldSatisfy` isRight

                prop "rejects a malformed address" $ withMaxSuccess 1 $
                    monadicIO $ do
                        withAddressFixture nm $ \_ layer _ WithAddressFixture{..} -> do
                            res <- WalletLayer.validateAddress layer "foobar"
                            case res of
                                Left (WalletLayer.ValidateAddressDecodingFailed "foobar") -> return ()
                                Left err -> fail $ "Got different error than expected: " <> show err
                                Right _ -> fail "I was expecting a failure, but it didn't happen."

                prop "rejects an address which is not ours" $ withMaxSuccess 1 $ do
                    monadicIO $ do
                        (randomAddr :: Address) <- pick arbitrary
                        withAddressFixture nm $ \_ layer _ WithAddressFixture{..} -> do
                            res <- WalletLayer.validateAddress layer (sformat build randomAddr)
                            case res of
                                Left (WalletLayer.ValidateAddressNotOurs _) -> return ()
                                Left err -> fail $ "Got different error than expected: " <> show err
                                Right _ -> fail "I was expecting a failure, but it didn't happen."