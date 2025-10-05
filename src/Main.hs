module Main where

import           Options.Applicative

import qualified Problems
import           Problems.Class


main :: IO ()
main = run =<< execParser opts
  where
    opts = info (options <**> helper)
      ( fullDesc
     <> progDesc "Run a Codeforces problem"
     <> header "codeforces-in-haskell - solving Codeforces problems in Haskell"
      )

run :: Options -> IO ()
run opts =
  case lookup (problemCode opts) problems of
    Just problem -> (solve problem <$> getContents) >>= putStrLn
    _ -> error $ "No registered solution for problem: " <> problemCode opts

problems :: [(String, Problem_)]
problems =
  [ ("231A", Problem_ Problems.P_231A)
  , ("1982A", Problem_ Problems.P_1982A)
  ]

data Options = MkOptions
  { problemCode :: !String
  }
  deriving Show

options :: Parser Options
options = MkOptions
  <$> strOption
    ( long "code"
    <> short 'c'
    <> metavar "CODE"
    <> help "The code for the problem to solve"
    )
