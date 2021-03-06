{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE RecordWildCards #-}

-- | Arbitrary instances for client types

module Test.Pos.Wallet.Arbitrary.Web.ClientTypes
       (
       ) where

import qualified Data.ByteString.Char8 as B8
import qualified Serokell.Util.Base64 as B64
import           Test.QuickCheck (Arbitrary (..), vectorOf)

import           Pos.Wallet.Web.ClientTypes.Types (CHash (..), CId (..),
                     CWAddressMeta (..))
import           Pos.Wallet.Web.State (WAddressMeta (..))

import           Test.Pos.Core.Arbitrary ()
import           Universum

instance Arbitrary CHash where
    arbitrary = CHash . B64.encode . B8.pack <$> vectorOf 64 arbitrary

instance Arbitrary (CId w) where
    arbitrary = CId <$> arbitrary

-- TODO it's generate invalid CWAddressMeta
-- @deriveLvl2KeyPair@ should be used for this instance
-- but it's extremely slow
instance Arbitrary CWAddressMeta where
    arbitrary = do
        cwamWId <- arbitrary
        cwamAccountIndex <- arbitrary
        cwamAddressIndex <- arbitrary
        cwamId <- arbitrary
        pure CWAddressMeta {..}

instance Arbitrary WAddressMeta where
  arbitrary = WAddressMeta
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
