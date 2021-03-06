{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}
module Util where

import           Universum hiding ((^?))

import           Cardano.Wallet.Client.Http
import           Control.Lens
import           System.IO.Unsafe (unsafePerformIO)
import           Test.Hspec
import           Test.QuickCheck (arbitrary, generate)


type WalletRef = MVar Wallet

data PaginationTest ixs a = PaginationTest
    { page         :: Maybe Page
    , perPage      :: Maybe PerPage
    , filters      :: FilterOperations ixs a
    , sorts        :: SortOperations a
    , expectations :: [a] -> IO ()
    }

randomWallet :: WalletOperation -> IO NewWallet
randomWallet walletOp =
    generate $
        NewWallet
            <$> arbitrary
            <*> pure Nothing
            <*> arbitrary
            <*> pure "Wallet"
            <*> pure walletOp

randomCreateWallet :: IO NewWallet
randomCreateWallet = randomWallet CreateWallet

randomRestoreWallet :: IO NewWallet
randomRestoreWallet = randomWallet RestoreWallet

randomAccount :: WalletClient IO -> IO (Wallet, Account)
randomAccount wc = do
    newWallet <- randomWallet CreateWallet
    wallet@Wallet{..} <- createWalletCheck wc newWallet
    (\(account, _) -> (wallet, account)) <$> firstAccountAndId wc wallet

createWalletCheck :: WalletClient IO -> NewWallet -> IO Wallet
createWalletCheck wc newWallet = do
    result <- fmap wrData <$> postWallet wc newWallet
    result `shouldPrism` _Right

firstAccountAndId :: WalletClient IO -> Wallet -> IO (Account, WalletAddress)
firstAccountAndId wc wallet = do
    etoAccts <- getAccounts wc (walId wallet)
    toAccts <- fmap wrData etoAccts `shouldPrism` _Right

    toAccts `shouldSatisfy` (not . null)
    let (toAcct : _) = toAccts

    accAddresses toAcct `shouldSatisfy` (not . null)
    let (toAddr : _) = accAddresses toAcct

    pure (toAcct, toAddr)

createAddress :: WalletClient IO -> (Wallet, Account) -> IO WalletAddress
createAddress wc (Wallet{..}, Account{..}) = do
    eresp <- postAddress wc (NewAddress Nothing accIndex walId)
    wrData <$> eresp `shouldPrism` _Right

createAddresses :: WalletClient IO -> Int -> (Wallet, Account) -> IO [WalletAddress]
createAddresses wc n src =
    replicateM n (createAddress wc src)

newWalletRef :: IO WalletRef
newWalletRef = newEmptyMVar

sampleWallet :: WalletRef -> WalletClient IO -> IO Wallet
sampleWallet wRef wc = do
    mwallet <- tryTakeMVar wRef
    case mwallet of
        Just wallet -> do
            putMVar wRef wallet
            pure wallet
        Nothing -> do
            w <- randomWallet CreateWallet
            w' <- createWalletCheck wc w
            didWrite <- tryPutMVar wRef w'
            if didWrite
                then pure w'
                else readMVar wRef

genesisWallet :: WalletClient IO -> IO Wallet
genesisWallet wc = do
    Right allWallets <- fmap wrData <$> getWallets wc
    maybe
        (fail "Genesis wallet is missing; did you import it prior to executing the test-suite?")
        return
        (find isUnlockedGenesisWallet allWallets)
  where isUnlockedGenesisWallet w = isGenesisWallet w && not (isLockedWallet w)

-- Hard code the genesis wallet that's asset locked
genesisAssetLockedWallet :: WalletClient IO -> IO Wallet
genesisAssetLockedWallet wc = do
    Right allWallets <- fmap wrData <$> getWallets wc
    maybe
        (fail "Genesis wallet is missing; did you import it prior to executing the test-suite?")
        return
        (find isLockedGenesisWallet allWallets)
  where isLockedGenesisWallet w = isGenesisWallet w && isLockedWallet w

isGenesisWallet :: Wallet -> Bool
isGenesisWallet = (== "Imported Wallet") . walName

lockedWallet :: WalletId
lockedWallet =
    WalletId "Ae2tdPwUPEZ5YjF9WuDoWfCZLPQ56MdQC6CZa2VKwMVRVqBBfTLPNcPvET4"

isLockedWallet :: Wallet -> Bool
isLockedWallet = (== lockedWallet) . walId

genesisRef :: WalletRef
genesisRef = unsafePerformIO newEmptyMVar
{-# NOINLINE genesisRef #-}

shouldPrism :: Show s => s -> Prism' s a -> IO a
shouldPrism a b = do
    a `shouldSatisfy` has b
    let Just x = a ^? b
    pure x

infixr 8 `shouldPrism`

shouldPrism_ :: Show s => s -> Prism' s a -> IO ()
shouldPrism_ a b =
    a `shouldSatisfy` has b

infixr 8 `shouldPrism_`
