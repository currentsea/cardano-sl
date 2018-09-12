{-# LANGUAGE LambdaCase #-}
-- | React to BListener events
module Cardano.Wallet.Kernel.BListener (
    -- * Respond to block chain events
    applyBlock
  , switchToFork
    -- * Testing
  , observableRollbackUseInTestsOnly
  ) where

import           Universum hiding (State)

import           Control.Concurrent.MVar (modifyMVar_)
import           Control.Lens (_Just)
import           Control.Monad.Except (throwError, withExceptT)
import           Data.Acid.Advanced (update')
import qualified Data.Map.Strict as Map
import           Data.SafeCopy (base, deriveSafeCopy)
import           Formatting (bprint, build, (%))
import qualified Formatting.Buildable

import           Pos.Chain.Block (HeaderHash)
import           Pos.Core.Chrono (OldestFirst (..))
import           Pos.Core.Txp (TxId)
import           Pos.Crypto (EncryptedSecretKey)
import           Pos.DB.Block (getBlund)

import           Cardano.Wallet.Kernel.DB.AcidState (ApplyBlock (..),
                     ObservableRollbackUseInTestsOnly (..), SwitchToFork (..),
                     SwitchToForkError (..), SwitchToForkInternalError (..))
import           Cardano.Wallet.Kernel.DB.BlockContext
import           Cardano.Wallet.Kernel.DB.HdWallet
import           Cardano.Wallet.Kernel.DB.InDb
import           Cardano.Wallet.Kernel.DB.Resolved (ResolvedBlock, rbContext)
import           Cardano.Wallet.Kernel.DB.Spec.Pending (Pending)
import           Cardano.Wallet.Kernel.DB.Spec.Update (ApplyBlockFailed (..))
import           Cardano.Wallet.Kernel.DB.TxMeta.Types
import           Cardano.Wallet.Kernel.Internal
import qualified Cardano.Wallet.Kernel.NodeStateAdaptor as Node
import           Cardano.Wallet.Kernel.PrefilterTx (PrefilteredBlock (..),
                     prefilterBlock)
import           Cardano.Wallet.Kernel.Read (getWalletCredentials)
import qualified Cardano.Wallet.Kernel.Submission as Submission
import           Cardano.Wallet.Kernel.Types (WalletId (..))
import qualified Cardano.Wallet.Kernel.Util.NonEmptyMap as NEM
import           Cardano.Wallet.WalletLayer.Kernel.Wallets
                     (blundToResolvedBlock)

{-------------------------------------------------------------------------------
  Passive Wallet API implementation
-------------------------------------------------------------------------------}

-- | Prefilter the block for each account.
--
-- TODO: Improve performance (CBR-379)
prefilterBlock' :: PassiveWallet
                -> ResolvedBlock
                -> IO ((BlockContext, Map HdAccountId PrefilteredBlock), [TxMeta])
prefilterBlock' pw b = do
    aux <$> getWalletCredentials pw
  where
    aux :: [(WalletId, EncryptedSecretKey)]
        -> ((BlockContext, Map HdAccountId PrefilteredBlock), [TxMeta])
    aux ws =
      let (conMap, conMeta) = mconcat $ map (uncurry (prefilterBlock b)) ws
      in ((b ^. rbContext, conMap), conMeta)

data BackfillFailed
    = SuccessorChanged BlockContext (Maybe BlockContext)
      -- ^ Although we already checked that it should fit, the block we're trying
      -- to apply does not fit onto the previous block.
    | CouldNotReachCheckpoint BlockContext
      -- ^ While trying to backfill missing blocks, we found that the last known
      -- block was not an ancestor of the block to apply.
    | CouldNotFindBlockForHeader HeaderHash
      -- ^ While trying to backfill missing blocks, we got a header that did not
      -- correspond to a known block.
    | NotAMainBlock HeaderHash
      -- ^ While trying to backfill missing blocks, we got a header that did not
      -- correspond to a main block.

deriveSafeCopy 1 'base ''BackfillFailed

instance Buildable BackfillFailed where
    build (SuccessorChanged context checkpoint) = bprint
        ("SuccessorChanged "
        % "{ context:    " % build
        % ", checkpoint: " % build
        % " }"
        )
        context
        checkpoint
    build (CouldNotReachCheckpoint context) = bprint
        ("CouldNotReachCheckpoint "
        % "{ context: " % build
        % " }"
        )
        context
    build (CouldNotFindBlockForHeader hh) = bprint
        ("CouldNotFindBlockForHeader "
        % "{ header hash: " % build
        % " }"
        )
        hh
    build (NotAMainBlock hh) = bprint
        ("NotAMainBlock "
        % "{ header hash: " % build
        % " }"
        )
        hh

-- | Notify all the wallets in the PassiveWallet of a new block
--
-- NOTE: Multiple concurrent or parallel calls to 'applyBlock' are not allowed.
-- Without this constraint, two concurrent backfill operations could conflict
-- with each other and cause both to fail.
-- The serialization of calls to 'applyBlock' is handled by the wallet worker,
-- which should carry the sole responsibility for applying blocks to a wallet.
applyBlock :: PassiveWallet
           -> ResolvedBlock
           -> IO (Either BackfillFailed ())
applyBlock pw@PassiveWallet{..} b = do
    k <- Node.getSecurityParameter _walletNode
    runExceptT (applyOneBlock k Nothing b) >>= \case
        Right () -> return (Right ())
        Left  (ApplyBlockNotSuccessor curCtx cpCtx) -> runExceptT $ do
          -- If we could not apply this block, there are three possibilities:
          --   1. The wallet worker has fallen behind the node and is missing blocks.
          --      In this case, the wallet worker's tip is an ancestor of the block to
          --      apply, and we should try to find and apply each of the missing blocks
          --      so that the wallet catches up to the node.
          --   2. An account's checkpoint is actually *ahead* of the wallet worker's tip.
          --      This can happen if a restoration begins after the node sees a block,
          --      but before the wallet worker has applied it. In this case, do nothing.
          --   3. An account's checkpoint is incomparable with the wallet worker's tip.
          --      This could happen because the account in on a different fork. In this
          --      case, start a restoration on the account's wallet.
          blocks <- findMissing (Just curCtx) cpCtx []
          for_ (getOldestFirst blocks) (withExceptT convertError . applyOneBlock k Nothing)

  where

      -- Try to apply a single block, failing if it does not fit onto the most recent checkpoint.
      applyOneBlock :: Node.SecurityParameter
                    -> Maybe (Set HdAccountId)
                    -> ResolvedBlock
                    -> ExceptT ApplyBlockFailed IO ()
      applyOneBlock k accts b' = ExceptT $ do
          ((ctxt, blocksByAccount), metas) <- prefilterBlock' pw b'
          -- apply block to all Accounts in all Wallets
          mConfirmed <- update' _wallets $ ApplyBlock k ctxt accts blocksByAccount
          case mConfirmed of
              Left  errs -> return . Left . snd . NEM.findMin $ errs
              Right confirmed -> do
                  modifyMVar_ _walletSubmission (return . Submission.remPending confirmed)
                  mapM_ (putTxMeta _walletMeta) metas
                  return $ Right ()

      -- Interpret an ApplyBlockFailed error during backfilling as a BackfillFailed.
      convertError :: ApplyBlockFailed -> BackfillFailed
      convertError = \case
          ApplyBlockNotSuccessor curCtx cpCtx -> SuccessorChanged curCtx cpCtx

      -- Find all blocks that were missing between the given block and the wallet's most recent
      -- checkpoint. 'Nothing' is used to represent the genesis block.
      findMissing :: Maybe BlockContext
                  -> Maybe BlockContext
                  -> [ResolvedBlock]
                  -> ExceptT BackfillFailed IO (OldestFirst [] ResolvedBlock)
      findMissing Nothing    Nothing    acc = return (OldestFirst acc)
      findMissing Nothing    (Just cp) _acc = throwError (CouldNotReachCheckpoint cp)
      findMissing (Just cur) tgt        acc =
        if (Just (cur ^. bcHash)) == (tgt ^? _Just . bcHash) then
            return (OldestFirst acc)
        else do
            rb   <- hashToBlock (cur ^. bcHash . fromDb)
            prev <- traverse hashToBlock (rb ^? rbContext . bcPrevMain . _Just . fromDb)
            findMissing (prev ^? _Just . rbContext) tgt (rb : acc)

      -- Find and resolve the block with a given hash.
      hashToBlock :: HeaderHash -> ExceptT BackfillFailed IO ResolvedBlock
      hashToBlock hh = ExceptT $
          Node.withNodeState (pw ^. walletNode) (\_lock -> getBlund hh) >>= \case
              Nothing    -> return $ Left (CouldNotFindBlockForHeader hh)
              Just blund ->
                  blundToResolvedBlock (pw ^. walletNode) blund <&> \case
                      Nothing -> Left  (NotAMainBlock hh)
                      Just rb -> Right rb

-- | Switch to a new fork
--
-- NOTE: The Ouroboros protocol says that this is only valid if the number of
-- resolved blocks exceeds the length of blocks to roll back.
switchToFork :: PassiveWallet
             -> BlockContext    -- ^ The tip that we are switching away from.
             -> Int             -- ^ Number of blocks to roll back
             -> [ResolvedBlock] -- ^ Blocks in the new fork
             -> IO (Either SwitchToForkError ())
switchToFork pw@PassiveWallet{..} oldTip n bs = do
    k <- Node.getSecurityParameter _walletNode
    blocksAndMeta <- mapM (prefilterBlock' pw) bs
    let (blockssByAccount, metas) = unzip blocksAndMeta

    trySwitchingToFork k blockssByAccount >>= \case
        Left  err     -> return $ Left err
        Right changes -> do
            mapM_ (putTxMeta _walletMeta) $ concat metas
            modifyMVar_ _walletSubmission $
                return . Submission.addPendings (fst <$> changes)
            modifyMVar_ _walletSubmission $
                return . Submission.remPending (snd <$> changes)
            return $ Right ()
  where

    trySwitchingToFork :: Node.SecurityParameter
                       -> [(BlockContext, Map HdAccountId PrefilteredBlock)]
                       -> IO (Either SwitchToForkError (Map HdAccountId (Pending, Set TxId)))
    trySwitchingToFork k blockssByAccount = do
        -- Find any new restorations that we didn't know about.
        restorations <- Map.elems <$> currentRestorations pw
        -- Stop the restorations and get the re-started account states that should be used.
        newAccts <- Map.unions <$> mapM prepareForRestoration restorations
        -- Switch to the fork, retrying if another restoration begins in the meantime.
        update' _wallets (SwitchToFork k oldTip n blockssByAccount newAccts) >>= \case
            Left RollbackDuringRestoration      -> trySwitchingToFork k blockssByAccount
              -- ^ Some more accounts started restoring, try again.
            Left (ApplyBlockFailedInternal err) -> return $ Left (ApplyBlockFailed err)
            Left NotEnoughBlocksInternal        -> return $ Left NotEnoughBlocks
            Right changes                       -> do
                -- Restart the restorations, and return the changes.
                mapM_ restartRestoration restorations
                return $ Right changes

-- | Observable rollback
--
-- Only used for tests. See 'switchToFork'.
-- TODO(kde): Do we want tests to deal with metadata?
observableRollbackUseInTestsOnly :: PassiveWallet
                                 -> IO (Either SwitchToForkInternalError ())
observableRollbackUseInTestsOnly PassiveWallet{..} = do
    res <- update' _wallets $ ObservableRollbackUseInTestsOnly
    case res of
      Left err           -> return $ Left err
      Right reintroduced -> do modifyMVar_ _walletSubmission $
                                 return . Submission.addPendings reintroduced
                               return $ Right ()
