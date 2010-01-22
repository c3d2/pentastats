module Main (main) where

import qualified Data.ByteString.Lazy.Char8 as C
import Data.Attoparsec
import Prelude hiding (takeWhile)
import Data.Char (chr, ord, isDigit)
import Control.Monad (liftM, forM_, forM)
import Data.Time.Format (parseTime)
import System.Locale (defaultTimeLocale)
import Data.Time.Clock
import Data.Time.Calendar (Day, addDays)
import Data.Maybe (fromMaybe)
import Data.List (foldl', intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO
import System (system)
import System.Directory (removeFile)

-- For elder GHC, base, time without instance Ix Date
dateRange (begin, end) | begin < end
                           = begin : dateRange (addDays 1 begin, end)
                       | otherwise
                           = [end]
    


data Request = Get Day C.ByteString Integer
             | Unknown
             deriving (Show)
reqIsGet (Get _ _ _) = True
reqIsGet _ = False

parseLine :: C.ByteString -> Request
parseLine = getResult . parse line
    where getResult (_, Right a) = a
          line = do ip <- word
                    space
                    ident <- word
                    space
                    user <- word
                    space
                    char '['
                    date <- parseDate `liftM` takeWhile ((/= ']') . chr . fromIntegral)
                    char ']'
                    space
                    char '"'
                    method <- word
                    space
                    path <- word
                    space
                    ver <- word
                    space
                    code <- num
                    space
                    size <- num
                    return $ if code >= 200 && code < 300
                             then Get date path size
                             else Unknown
          char = word8 . fromIntegral . ord
          space = char ' '
          word = takeWhile $ (/= ' ') . chr . fromIntegral
          num = (maybe 0 fst . C.readInteger) `liftM` takeWhile (isDigit . chr . fromIntegral)
          -- Here be dragons
          parseDate = utctDay .
                      fromMaybe undefined .
                      parseTime defaultTimeLocale "%d/%b/%Y:%H:%M:%S %z" .
                      C.unpack
{-
          utcToUnix utc = truncate $ diffUTCTime utc utc1970
          utc1970 = fromMaybe undefined $ parseTime defaultTimeLocale "" ""
-}

type Stats k = Map k FileStats
type FileStats = Map Day Integer

collectRequest :: Stats C.ByteString -> Request -> Stats C.ByteString
collectRequest stats (Get day file size)
    = Map.alter (Just .
             Map.alter (Just .
                        (+ size) .
                        fromMaybe 0
                       ) day .
             fromMaybe Map.empty
            ) file stats


reducePentaMedia :: Stats C.ByteString -> Stats (String, String)
reducePentaMedia
    = Map.mapKeys (\fn ->
                       let fn' = last $ split '/' fn
                           fn'' = split '.' fn'
                           ext = last fn''
                           fn''' = intercalate "." $
                                   take (length fn'' - 1) fn''
                       in (fn''', ext)
                  ) .
      Map.filterWithKey (\fn _ ->
                             fn /= "" &&
                             isValid fn &&
                             oneDot fn &&
                             (fn `startsWith` "/pentaradio/" ||
                              fn `startsWith` "/pentacast/")
                        ) .
      -- TODO: URL normalization & URI decoding
      Map.mapKeys C.unpack
    where startsWith a b = take (length b) a == b
          split :: Char -> String -> [String]
          split c s = case break (== c) s of
                        (s', c:s'') -> s' : split c s''
                        (s, "") -> [s]
          oneDot ('.':s) = oneDot' s
          oneDot (_:s) = oneDot s
          oneDot "" = False
          oneDot' ('.':_) = False
          oneDot' (_:s) = oneDot' s
          oneDot' "" = True
          isValid ('?':_) = False
          isValid ('&':_) = False
          isValid (_:s) = isValid s
          isValid "" = True

groupByExt :: Stats (String, String) -> Map String (Stats String)
groupByExt
    = Map.foldWithKey (\(fn, ext) stats ->
                           Map.alter (Just . Map.insert ext stats . fromMaybe Map.empty
                                     ) fn
                      ) Map.empty

createFiles :: Map String (Stats String) -> IO ()
createFiles fnStats
    = do forM_ (Map.toList fnStats) $ \(fn, extStats) ->
             do putStrLn $ fn ++ " " ++ (show $ Map.keys extStats)
                render fn extStats
         writeFile "index.html" indexSource
    where indexSource = "<h1>Pentamedia Stats</h1>" ++
                        concatMap (\fn -> "<h2>" ++ fn ++ "</h2><img src=\"" ++ fn ++ ".png\"/>") (Map.keys fnStats)
          render fn extStats
              = do dataSources <- forM (zip [0..] $ Map.toList extStats) $ \(i, (ext, stats)) ->
                                  do writeData i stats
                                     return $ (ext, dataFile i)
                   writeFile plotSourceFile $ plotSource (fn ++ ".png") dataSources
                   system $ "gnuplot " ++ plotSourceFile
                   removeFile plotSourceFile
          plotSource :: String -> [(String, String)] -> String
          plotSource outfile dataSources
              = "set terminal png\n" ++
                "set output '" ++ outfile ++ "'\n" ++
                "set xdata time\n" ++
                "set timefmt \"%Y-%m-%d\"\n" ++
                "set xlabel \"Date\"\n" ++
                "set ylabel \"Bytes\"\n" ++
                "plot" ++
                concatMap (\(i, (ext, dataFile)) ->
                               (if i == 0
                                then " "
                                else ", ") ++
                               "'" ++ dataFile ++  "' using 1:2 title '" ++ ext ++ "' with lines"
                          ) (zip [0..] dataSources)
          plotSourceFile = "graph.gnuplot"
          dataFile :: Int -> String
          dataFile i = "data-" ++ show i
          writeData :: Int -> FileStats -> IO ()
          writeData i stats
              = withFile (dataFile i) WriteMode $ \f ->
                forM_ (fillDayStats stats) $ \(day, size) ->
                hPutStrLn f $ show day ++ " " ++ show size
          fillDayStats :: FileStats -> [(Day, Integer)]
          fillDayStats stats
              = do day <- dateRange (fst $ Map.findMin stats, fst $ Map.findMax stats)
                   return (day, fromMaybe 0 $ Map.lookup day stats)

main = C.getContents >>=
       createFiles .
       groupByExt .
       reducePentaMedia .
       foldl' collectRequest Map.empty .
       filter reqIsGet .
       map parseLine .
       C.lines
