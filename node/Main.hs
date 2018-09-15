module Main
    ( main
    ) where

import           Universum

import           Pos.Client.CLI (SimpleNodeArgs (..), getSimpleNodeOptions,
                     loggingParams)
import           Pos.Launcher (launchNode, runNode, runRealMode)
import           Pos.Util.CompileInfo (withCompileInfo)
import           Pos.Util.Wlog (logInfo)
import           Pos.Worker.Update (updateTriggerWorker)


main :: IO ()
main = withCompileInfo $ do
    SimpleNodeArgs cArgs nArgs <- getSimpleNodeOptions
    let lArgs = loggingParams "node" cArgs

    launchNode nArgs cArgs lArgs $ \coreConfig _ txpConfig _ _ _ nodeRes -> do
        let plugins = [ updateTriggerWorker ]

        logInfo "Wallet is disabled, because software is built w/o it"

        runRealMode
            coreConfig
            txpConfig
            nodeRes
            (runNode coreConfig txpConfig nodeRes plugins)
