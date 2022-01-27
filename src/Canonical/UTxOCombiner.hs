module Canonical.UTxOCombiner where
import Options.Applicative
import Control.Monad
import Cardano.Transaction
import Data.Function

data Options = Options
  { oAddress :: String
  , oSigningKeyPath :: FilePath
  , oBatchCount :: Int
  , oTestnet :: Maybe Integer
  , oOneShot :: Bool
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

parse :: IO Options
parse = execParser $ info (pOptions <**> helper) mempty

run :: Options -> IO ()
run Options {..} = do
  fix $ \next -> do
    utxos <- queryUtxos oAddress oTestnet

    when (length utxos > oBatchCount) $ eval oTestnet $ do
      let
        inputsToUse = take oBatchCount utxos

      forM_ inputsToUse input
      void $ balanceNonAdaAssets oAddress
      changeAddress oAddress
      sign oSigningKeyPath

    unless oOneShot $ do
      waitForNextBlock oTestnet
      next

main :: IO ()
main = run =<< parse
