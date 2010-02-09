module Main (main, getFileSize) where

import qualified Data.ByteString.Lazy.Char8 as C
import Data.Attoparsec
import Prelude hiding (takeWhile)
import Data.Char (chr, ord, isDigit)
import Control.Monad (liftM, forM_, forM)
import Data.Time.Format (parseTime)
import System.Locale (defaultTimeLocale)
import Data.Time.Clock
import Data.Time.Calendar (Day, addDays, toModifiedJulianDay)
import Data.Maybe (fromMaybe)
import Data.List (foldl', intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO
import System (system)
import System.Directory (removeFile)
import qualified Network.HTTP as HTTP
import Network.URI (parseURI)
import Text.Printf (printf)


getFileSize :: C.ByteString -> IO Integer
getFileSize path
    = do putStrLn $ "HEAD " ++ C.unpack path
         getSize `liftM` HTTP.simpleHTTP headRequest
    where uri = fromMaybe undefined $
                parseURI $
                "http://ftp.c3d2.de" ++ C.unpack path
          headRequest :: HTTP.Request C.ByteString
          headRequest = HTTP.mkRequest HTTP.HEAD uri
          getSize (Right rsp) = read $
                                fromMaybe "0" $
                                HTTP.findHeader HTTP.HdrContentLength rsp

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
reqPath (Get _ path _) = path
reqPath Unknown = error "No path for Unknown request"

parseLine :: C.ByteString -> Request
parseLine = getResult . parse line
    where getResult (_, Right a) = a
          getResult _ = Unknown
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
                    return $ if C.unpack method == "GET" &&
                                code >= 200 &&
                                code < 300
                             then toModifiedJulianDay date `seq`
                                  C.unpack path `seq`
                                  size `seq`
                                  Get date path size
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
    = let stats' = Map.alter (Just .
                              Map.insertWith' (+) day size .
                              fromMaybe Map.empty
                             ) file stats
      in stats'

isPentaMedia :: C.ByteString -> Bool
isPentaMedia fn = not (C.null fn) &&
                  isValid fn &&
                  oneDot fn &&
                  (fn `startsWith` "/pentaradio/" ||
                   fn `startsWith` "/pentacast/")
    -- TODO: URL normalization & URI decoding
    where startsWith a b = C.unpack (C.take (fromIntegral $ length b) a) == b
          oneDot s = case C.uncons s of
                       Just ('.', s') -> oneDot' s'
                       Just (_, s') -> oneDot s'
                       Nothing -> False
          oneDot' s = case C.uncons s of
                        Just ('.', s') -> False
                        Just (_, s') -> oneDot' s'
                        Nothing -> True
          isValid = C.all (`notElem` "?&")

getFileSizes :: Stats C.ByteString -> IO (Stats (C.ByteString, Integer))
getFileSizes
    = liftM Map.fromList .
      mapM (\(fn, stats) ->
                do size <- getFileSize fn
                   return ((fn, size), stats)
           ) .
      Map.toList

reduceFilenames :: Stats (C.ByteString, Integer) -> Stats (C.ByteString, C.ByteString, Integer)
reduceFilenames
    = Map.mapKeys (\(fn, size) ->
                       let fn' = last $ split '/' fn
                           fn'' = split '.' fn'
                           ext = last fn''
                           fn''' = C.intercalate (C.singleton '.') $
                                   take (length fn'' - 1) fn''
                       in (fn''', ext, size)
                  )
    where split :: Char -> C.ByteString -> [C.ByteString]
          split c s = case C.break (== c) s of
                        (s, s'') | C.null s'' -> [s]
                        (s', s'') -> s' : split c (C.tail s'')

groupByExt :: Stats (C.ByteString, C.ByteString, Integer) -> Map C.ByteString (Stats (C.ByteString, Integer))
groupByExt
    = Map.foldWithKey (\(fn, ext, size) stats ->
                           Map.alter (Just . Map.insert (ext, size) stats . fromMaybe Map.empty
                                     ) fn
                      ) Map.empty

createOutput :: Map C.ByteString (Stats (C.ByteString, Integer)) -> IO ()
createOutput fnStats
    = do forM_ (Map.toList fnStats) $ \(fn, extStats) ->
             do putStrLn $ C.unpack fn ++ " " ++ (show $ Map.keys extStats)
                render fn extStats
         writeFile "index.html" indexSource
    where indexSource = "<h1>Pentamedia Stats</h1>" ++
                        concatMap (\(fn, extStats) ->
                                       "<h2>" ++ C.unpack fn ++ "</h2>" ++
                                       "<p>" ++ printf "%.1f" (downloads extStats) ++ " downloads</p>" ++
                                       "<img src=\"" ++ C.unpack fn ++ ".png\"/>"
                                  ) (Map.toList fnStats)
          downloads :: Stats (C.ByteString, Integer) -> Double
          downloads
              = foldl' (\sum ((ext, fileSize), stats) ->
                            sum + (fromIntegral (Map.fold (+) 0 stats) / fromIntegral fileSize)
                       ) 0 .
                Map.toList
          render fn extStats
              = do dataSources <- forM (zip [0..] $ Map.toList extStats) $ \(i, ((ext, size), stats)) ->
                                  do writeData i size stats
                                     return $ (ext, dataFile i)
                   writeFile plotSourceFile $ plotSource (C.unpack fn ++ ".png") dataSources
                   system $ "gnuplot " ++ plotSourceFile
                   removeFile plotSourceFile
                   mapM_ removeFile $ map (C.unpack . snd) dataSources
          plotSource :: String -> [(C.ByteString, C.ByteString)] -> String
          plotSource outfile dataSources
              = "set terminal png tiny\n" ++
                "set output '" ++ outfile ++ "'\n" ++
                "set xdata time\n" ++
                "set timefmt \"%Y-%m-%d\"\n" ++
                "set xtics autofreq\n" ++
                "set format x \"%Y-%m-%d\"\n" ++
                "set xlabel \"Date\"\n" ++
                "set ylabel \"Transmitted / Size\"\n" ++
                "plot" ++
                concatMap (\(i, (ext, dataFile)) ->
                               (if i == 0
                                then " "
                                else ", ") ++
                               "'" ++ C.unpack dataFile ++  "' using 1:2 title '" ++ C.unpack ext ++ "' with boxes"
                          ) (zip [0..] dataSources)
          plotSourceFile = "graph.gnuplot"
          dataFile :: Int -> C.ByteString
          dataFile i = C.pack $ "data-" ++ show i
          writeData :: Int -> Integer -> FileStats -> IO ()
          writeData i fileSize stats
              = withFile (C.unpack $ dataFile i) WriteMode $ \f ->
                forM_ (fillDayStats stats) $ \(day, size) ->
                hPutStrLn f $ show day ++ " " ++ show (fromIntegral size / fromIntegral fileSize)
          fillDayStats :: FileStats -> [(Day, Integer)]
          fillDayStats stats
              = do day <- dateRange (fst $ Map.findMin stats, fst $ Map.findMax stats)
                   return (day, fromMaybe 0 $ Map.lookup day stats)

main = C.getContents >>=
       return .
       foldl collectRequest Map.empty .
       filter (isPentaMedia . reqPath) .
       filter reqIsGet .
       map parseLine .
       C.lines >>=
       getFileSizes >>=
       createOutput .
       groupByExt .
       reduceFilenames
