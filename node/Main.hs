{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeOperators       #-}

module Main
       ( main
       ) where

import           Universum

import           Ntp.Client (NtpConfiguration)

import           Pos.Binary ()
import           Pos.Chain.Ssc (SscParams)
import           Pos.Chain.Txp (TxpConfiguration)
import           Pos.Client.CLI (CommonNodeArgs (..), NodeArgs (..),
                     SimpleNodeArgs (..))
import qualified Pos.Client.CLI as CLI
import           Pos.Core as Core (Config (..))
import           Pos.Launcher (HasConfigurations, NodeParams (..),
                     loggerBracket, runNodeRealSimple, withConfigurations)
import           Pos.Launcher.Configuration (AssetLockPath (..),
                     ConfigurationOptions (cfoKey))
import           Pos.Util (logException)
import           Pos.Util.CompileInfo (HasCompileInfo, withCompileInfo)
import           Pos.Util.Wlog (LoggerName, logInfo)
import           Pos.Worker.Update (updateTriggerWorker)

loggerName :: LoggerName
loggerName = "node"

actionWithoutWallet
    :: ( HasConfigurations
       , HasCompileInfo
       )
    => Core.Config
    -> TxpConfiguration
    -> SscParams
    -> NodeParams
    -> IO ()
actionWithoutWallet coreConfig txpConfig sscParams nodeParams =
    runNodeRealSimple coreConfig txpConfig nodeParams sscParams [updateTriggerWorker]

action
    :: ( HasConfigurations
       , HasCompileInfo
       )
    => SimpleNodeArgs
    -> Core.Config
    -> TxpConfiguration
    -> NtpConfiguration
    -> IO ()
action (SimpleNodeArgs (cArgs@CommonNodeArgs {..}) (nArgs@NodeArgs {..})) coreConfig txpConfig _ntpConfig = do
    logInfo "Wallet is disabled, because software is built w/o it"
    (currentParams, Just sscParams) <- CLI.getNodeParams
       loggerName
       cArgs
       nArgs
       (configGeneratedSecrets coreConfig)
    actionWithoutWallet coreConfig txpConfig sscParams currentParams

main :: IO ()
main = withCompileInfo $ do
    args@(CLI.SimpleNodeArgs commonNodeArgs _) <- CLI.getSimpleNodeOptions
    let loggingParams = CLI.loggingParams loggerName commonNodeArgs
    let conf          = CLI.configurationOptions (CLI.commonArgs commonNodeArgs)
    let getKey        = cfoKey conf
    let blPath        = AssetLockPath <$> cnaAssetLockPath commonNodeArgs
    loggerBracket getKey loggingParams
        . logException "node"
        $ withConfigurations blPath
                             (cnaDumpGenesisDataPath commonNodeArgs)
                             (cnaDumpConfiguration commonNodeArgs)
                             conf
        $ action args
