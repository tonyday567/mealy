{-# LANGUAGE OverloadedStrings #-}

-- | performance measurement
module Main where

import Control.Category (id)
import Control.Monad
import Data.List (intercalate)
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Mealy
import Data.Mealy.Quantiles
import Data.Mealy.Simulate
import Data.TDigest hiding (median)
import Data.Text (Text)
import Options.Applicative
import Perf
import Prelude hiding (id)

data Run = RunStats | RunQuantiles [Double] deriving (Eq, Show)

data AppConfig = AppConfig
  { appReportOptions :: ReportOptions,
    appRun :: Run
  }
  deriving (Eq, Show)

parseRun :: Parser Run
parseRun =
  flag' RunStats (long "stats" <> help "run stats test")
    <|> flag' (RunQuantiles [0.1, 0.5, 0.9]) (long "quantiles" <> help "run quantiles test")
    <|> pure RunStats

parseAppConfig :: Parser AppConfig
parseAppConfig =
  AppConfig
    <$> parseReportOptions defaultReportOptions
    <*> parseRun

appInfo :: ParserInfo AppConfig
appInfo =
  info
    (parseAppConfig <**> helper)
    (fullDesc <> progDesc "mealy performance measurement")

main :: IO ()
main = do
  o <- execParser appInfo
  let repOptions = appReportOptions o
  let n = reportN repOptions
  let s = reportStatDType repOptions
  let mt = reportMeasureType repOptions
  let run = appRun o
  let l = reportLength repOptions
  gen <- create
  _ <- warmup 100
  case run of
    RunStats -> do
      xs <- rvs gen l
      reportMain ExampleSum repOptions (intercalate "-" [show run, show n, show l, show s, show mt]) (void . stats xs)
    RunQuantiles qs -> do
      xs <- rvs gen l
      reportMain ExampleSum repOptions (intercalate "-" [show run, show n, show l, show s, show mt]) (void . perfQuantiles xs qs)

reportRaw :: ReportOptions -> PerfT IO [[Double]] a -> IO (a, Map Text [[Double]])
reportRaw o t = do
  let !n = reportN o
  let c = reportClock o
  let mt = reportMeasureType o
  runPerfT (measureDs mt c n) t

stats :: (Semigroup t) => [Double] -> Int -> PerfT IO t [[Double]]
stats xs l = do
  r1 <- ffap "stats" (scan ((,) <$> ma 0.99 <*> std 0.99)) (take l xs)
  r2 <- ffap "ma" (scan (ma 0.99)) xs
  r3 <- ffap "std" (scan (std 0.99)) xs
  pure [fst <$> r1, snd <$> r1, r2, r3]

perfQuantiles :: (Semigroup t) => [Double] -> [Double] -> Int -> PerfT IO t [Int]
perfQuantiles qs xs l = do
  r1 <- ffap "digitize" (scan (digitize 0.99 qs)) (take l xs)
  r2 <- ffap "digitize'" (scan (digitize' 0.99 qs)) (take l xs)
  pure (r1 <> r2)

digitize' :: Double -> [Double] -> Mealy Double Int
digitize' r qs = M inject step' extract
  where
    step' (x, _) a = (onlineInsert a x, a)
    inject a = (onlineInsert a (emptyOnlineTDigest r), a)
    extract (x, l) = bucket' qs' l
      where
        qs' = fromMaybe (0 / 0) . (`quantile` t) <$> qs
        (OnlineTDigest t _ _) = onlineCompress x
        bucket' xs l' =
          fold (M id (+) id) $
            ( \x' ->
                if x' > l'
                  then 0
                  else 1
            )
              <$> xs
