{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

-- | Applications of runners to scenarios.

module Pos.Launcher.Launcher
       ( -- * Node launcher.
       launchNode
       ) where

import           Universum

import           Ntp.Client (NtpConfiguration)
import           Pos.Chain.Ssc (SscParams)
import           Pos.Chain.Txp (TxpConfiguration)
import           Pos.Client.CLI.NodeOptions (CommonNodeArgs (..), NodeArgs (..))
import           Pos.Client.CLI.Options (configurationOptions)
import           Pos.Client.CLI.Params (getNodeParams)
import           Pos.Core as Core (Config (..))
import           Pos.DB.DB (initNodeDBs)
import           Pos.DB.Txp.Logic (txpGlobalSettings)
import           Pos.Launcher.Configuration (AssetLockPath (..),
                     HasConfigurations, WalletConfiguration,
                     withConfigurations)
import           Pos.Launcher.Param (LoggingParams (..), NodeParams (..))
import           Pos.Launcher.Resource (NodeResources, bracketNodeResources,
                     loggerBracket)
import           Pos.Util.Util (logException)
import           Pos.WorkMode (EmptyMempoolExt)

-- import           Pos.Util.CompileInfo (HasCompileInfo, withCompileInfo)


-- | Run a given action from a bunch of static arguments
launchNode
    :: NodeArgs
    -> CommonNodeArgs
    -> LoggingParams
    -> (  HasConfigurations
       => Core.Config
       -> WalletConfiguration
       -> TxpConfiguration
       -> NtpConfiguration
       -> NodeParams
       -> SscParams
       -> NodeResources EmptyMempoolExt
       -> IO ()
       )
    -> IO ()
launchNode nArgs cArgs lArgs action = do
    let withLogger' = loggerBracket lArgs . logException (lpDefaultName lArgs)
    let withConfigurations' = withConfigurations
            (AssetLockPath <$> cnaAssetLockPath cArgs)
            (cnaDumpGenesisDataPath cArgs)
            (cnaDumpConfiguration cArgs)
            (configurationOptions (commonArgs cArgs))

    withLogger' $ withConfigurations' $ \coreConfig walletConfig txpConfig ntpConfig -> do
        (nodeParams, Just sscParams) <- getNodeParams
            (lpDefaultName lArgs)
            cArgs
            nArgs
            (configGeneratedSecrets coreConfig)

        let action' = action
                coreConfig
                walletConfig
                txpConfig
                ntpConfig
                nodeParams
                sscParams

        bracketNodeResources
            coreConfig
            nodeParams
            sscParams
            (txpGlobalSettings coreConfig txpConfig)
            (initNodeDBs coreConfig)
            action'
