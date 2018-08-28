{-# LANGUAGE ExistentialQuantification #-}

module Cardano.Wallet.WalletLayer.Exception
    ( WalletException(..)
    , walletExceptionToException
    , walletExceptionFromException
    ) where

import           Universum

import           Cardano.Wallet.API.Response.JSend (HasDiagnostic)
import           Cardano.Wallet.API.V1.Errors (ToHttpErrorStatus)

import           Data.Typeable (cast)

import qualified Prelude

-- | 'WalletException' is a root exception type meant to be used for all
-- exceptions in the WalletLayer. It allows us to catch them all in one handler,
-- provided they have instances for the required classes. It is not
-- in use as of #3464 or 13-09-2019.
--
-- Information about exception hierarchies:
-- http://hackage.haskell.org/package/base-4.11.1.0/docs/src/GHC.Exception.html#Exception
--
-- == Usage
-- To make e.g. 'CreateAccountError' a subexception of 'WalletException', the
-- following instances are needed:
--
-- * Exception
--
--     * Show
--
-- * HasDiagnostics
-- * ToHttpErrorStatus (has default implementation)
--
--     * ToServantError
--
--         * ToJSON
--     * Buildable
--
-- Also, the Exception instance should use 'walletExceptionToException' and
-- 'walletExceptionFromException':
--
-- > instance Exception CreateAccountError where
-- >     toException   = walletExceptionToException
-- >     fromException = walletExceptionFromException
--
-- Something like this should now work:
-- > _ `catch` \(_ :: (WalletException e)) -> toServantError e
data WalletException = forall e.
    ( Exception e
    , HasDiagnostic e
    , ToHttpErrorStatus e
    ) => WalletException { getWalletException :: e }


instance Prelude.Show WalletException where
    show (WalletException e) = Prelude.show e

instance Exception WalletException

-- | For implementing 'Control.Exception.toException'
walletExceptionToException :: forall e.
    ( Exception e
    , HasDiagnostic e
    , ToHttpErrorStatus e
    ) => e -> SomeException
walletExceptionToException = toException . WalletException

-- | For implementing 'Control.Exception.fromException'
walletExceptionFromException :: Exception e => SomeException -> Maybe e
walletExceptionFromException x = do
    WalletException a <- fromException x
    cast a
