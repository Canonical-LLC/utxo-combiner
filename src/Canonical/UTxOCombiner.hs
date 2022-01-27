module Canonical.UTxOCombiner where
import Options.Applicative
import Control.Monad
import Cardano.Transaction
import Data.Function
import Data.Maybe
import Control.Concurrent

data Options = Options
  { oAddress :: String
  , oSigningKeyPath :: FilePath
  , oBatchCount :: Int
  , oTestnet :: Maybe Integer
  , oOneShot :: Bool
  , oSendToAddress :: Maybe String
  , oWaitTime :: Int
  } deriving (Show, Eq)

pOptions :: Parser Options
pOptions
   =  Options
  <$> strOption
        ( long "address"
        <> short 'a'
        <> metavar "ADDRESS"
        )
  <*> strOption
        ( long "signing-key"
        <> short 's'
        <> metavar "FILE"
        )
  <*> option auto
        ( long "count"
        <> short 'c'
        <> metavar "BATCH_COUNT"
        )
  <*> (
        ( Just <$> option auto
          ( long "testnet-magic"
          <> short 't'
          <> metavar "TESTNET_MAGIC_NUMBER"
          )
        )
      <|>
        ( Nothing <$ switch
          ( long "mainnet"
          )
        )
      )
  <*> switch
        (  long "one-shot"
        <> short 'o'
        )
  <*> optional
        ( strOption
          ( long "output-address"
          <> metavar "ADDRESS"
          )
        )
  <*> option auto
        ( long "wait"
        <> short 'w'
        <> metavar "WAIT_SECONDS"
        <> value (45 * 60)
        )

parse :: IO Options
parse = execParser $ info (pOptions <**> helper) mempty

run :: Options -> IO ()
run Options {..} = do
  let senderAddress = fromMaybe oAddress oSendToAddress
  fix $ \next -> do
    utxos <- queryUtxos oAddress oTestnet

    when (length utxos > oBatchCount) $ eval oTestnet $ do
      let
        inputsToUse = take oBatchCount utxos

      forM_ inputsToUse input
      void $ balanceNonAdaAssets senderAddress
      changeAddress senderAddress
      sign oSigningKeyPath

    unless oOneShot $ do
      threadDelay $ 1_000_000 * oWaitTime
      next

main :: IO ()
main = run =<< parse
