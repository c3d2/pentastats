{-# LANGUAGE BangPatterns, TupleSections, FlexibleInstances #-}
module Main (main, getFileSize) where

import Control.Applicative
import qualified Data.ByteString.Char8 as SC
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Attoparsec.Lazy hiding (take)
import Prelude hiding (takeWhile)
import Data.Char (isDigit, isAlpha)
import Control.Monad (liftM, forM_, forM, when)
import Data.Time.Clock
import Data.Time.Calendar (Day, addDays, fromGregorian)
import Data.Maybe (fromMaybe)
import Data.List (foldl', intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO
import System.Cmd (system)
import System.Directory (removeFile)
import qualified Network.HTTP as HTTP
import Network.URI (parseURI)
import Text.Printf (printf)
import Data.ByteString.Internal (w2c, c2w)
import qualified Text.JSON as JSON
import GHC.Real (Ratio((:%)))
import Data.Network.Address


force a = a `seq` a

-- |in case getFileSize encounters 404
fallbackSize = 100 * 1024 * 1024  -- 100 MB

getFileSize :: SC.ByteString -> IO Integer
getFileSize path
    = do putStrLn $ "HEAD " ++ SC.unpack path
         getSize `liftM` HTTP.simpleHTTP headRequest
    where uri = fromMaybe undefined $
                parseURI $
                "http://ftp.c3d2.de" ++ SC.unpack path
          headRequest :: HTTP.Request C.ByteString
          headRequest = HTTP.mkRequest HTTP.HEAD uri
          getSize (Right rsp) = read $
                                fromMaybe (show fallbackSize) $
                                HTTP.findHeader HTTP.HdrContentLength rsp

-- For elder GHC, base, time without instance Ix Date
dateRange (begin, end) | begin < end
                           = begin : dateRange (addDays 1 begin, end)
                       | otherwise
                           = [end]

data Host = Host4 !IPv4
          | Host6 !IPv6
            deriving (Show, Eq, Ord)
data Request = Get !Day !SC.ByteString !Host !Integer
             | Unknown
             deriving (Show)
reqIsGet (Get _ _ _ _) = True
reqIsGet _ = False
reqPath (Get _ path _ _) = path
reqPath Unknown = error "No path for Unknown request"

parseLine :: C.ByteString -> Request
parseLine = {-# SCC "getResult" #-} getResult . {-# SCC "parse" #-} parse line
    where getResult (Done _ !a) = a
          getResult _ = Unknown
          line = do host <- {-# SCC "wordHost" #-} host
                    ident <- {-# SCC "wordIdent" #-} word
                    space
                    user <- {-# SCC "wordUser" #-} word
                    space
                    char '['
                    date <- {-# SCC "date" #-} date
                    takeWhile ((/= '"') . w2c)
                    char '"'
                    method <- {-# SCC "wordMethod" #-} word
                    when (SC.unpack method /= "GET") $
                      fail "Not GET"
                    space
                    path <- {-# SCC "wordPath" #-} word
                    space
                    ver <- {-# SCC "wordVer" #-} word
                    space
                    code <- {-# SCC "wordCode" #-} num'
                    when (code < 200 || code >= 300) $
                      fail "Wrong response code"
                    space
                    size <- {-# SCC "wordSize" #-} num
                    return $ {-# SCC "Get" #-} Get date path host size
          char = word8 . c2w
          space = char ' '
          word = takeWhile $ (/= ' ') . w2c
          num = (maybe 0 fst . SC.readInteger) `liftM` takeWhile (isDigit . w2c)
          num' = (maybe 0 fst . SC.readInt) `liftM` takeWhile (isDigit . w2c)
          host = (do h <- takeWhile ((\c -> isDigit c || c == '.') . w2c)
                     space
                     return $ Host4 $ readAddress $ SC.unpack h)
                 <|>
                 (do h <- takeWhile ((\c -> isDigit c || c `elem` "abcdef" || c == ':') . w2c)
                     space
                     return $ Host6 $ readAddress $ SC.unpack h)

          date = do day <- num'
                    char '/'
                    month <- month
                    char '/'
                    year <- num
                    char ':'
                    return $ fromGregorian year month day
          month = let m name num = takeWhile (isAlpha . w2c) >>= \name' ->
                                   if SC.unpack name' == name
                                   then return num
                                   else fail $ "No such month: " ++ SC.unpack name'
                  in choice [m "Jan" 1,
                             m "Feb" 2,
                             m "Mar" 3,
                             m "Apr" 4,
                             m "May" 5,
                             m "Jun" 6,
                             m "Jul" 7,
                             m "Aug" 8,
                             m "Sep" 9,
                             m "Oct" 10,
                             m "Nov" 11,
                             m "Dec" 12]

type Stats k k' = Map k (FileStats k')
type FileStats k = Map k Integer

collectRequest :: Request -> Stats SC.ByteString (Day, Host) -> Stats SC.ByteString (Day, Host)
collectRequest (Get day file host size) = Map.alter (Just .
                                                     Map.insertWith' (\a b -> a + b) (day, host) size .
                                                     fromMaybe Map.empty
                                                    ) file
collectRequest Unknown = id


isPentaMedia :: SC.ByteString -> Bool
isPentaMedia fn = not (SC.null fn) &&
                  isValid fn &&
                  oneDot fn &&
                  (fn `startsWith` "/pentaradio/" ||
                   fn `startsWith` "/pentacast/" ||
                   fn `startsWith` "/datenspuren/" ||
                   fn `startsWith` "/themenabend/" ||
                   fn `startsWith` "/video/")
    -- TODO: URL normalization & URI decoding
    where startsWith a b = take (fromIntegral $ length b) (SC.unpack a) == b
          oneDot s = case SC.uncons s of
                       Just ('.', s') -> oneDot' s'
                       Just (_, s') -> oneDot s'
                       Nothing -> False
          oneDot' s = case SC.uncons s of
                        Just ('.', s') -> False
                        Just (_, s') -> oneDot' s'
                        Nothing -> True
          isValid = SC.all (`notElem` "?&")

getFileSizes :: Stats SC.ByteString a -> IO (Stats (SC.ByteString, Integer) a)
getFileSizes stats = do sizes <- catch loadSizes (const $ return Map.empty)
                        sizes' <- getMissingSizes sizes stats
                        putStrLn $ "sizes: " ++ JSON.encode (toJSON sizes')
                        saveSizes sizes'
                        fillIn sizes' stats
  where sizesFile = "sizes.json"
        fromJSON :: JSON.JSObject Integer -> Map SC.ByteString Integer
        fromJSON = Map.mapKeys SC.pack . Map.fromList . JSON.fromJSObject
        toJSON :: Map SC.ByteString Integer -> JSON.JSObject Integer
        toJSON = JSON.toJSObject . Map.toList . Map.mapKeys SC.unpack
        loadSizes :: IO (Map SC.ByteString Integer)
        loadSizes = do JSON.Ok json <- JSON.decode <$> readFile sizesFile
                       return $ fromJSON json
        getMissingSizes :: Map SC.ByteString Integer 
                           -> Stats SC.ByteString a 
                           -> IO (Map SC.ByteString Integer)
        getMissingSizes sizes stats
          = Map.fromList <$>
            mapM (\(fn, _) ->
                   case Map.lookup fn sizes of
                     Nothing ->
                       (fn, ) <$> getFileSize fn
                     Just size ->
                       return (fn, size)
                 ) (Map.toList stats)
        saveSizes :: Map SC.ByteString Integer -> IO ()
        saveSizes = writeFile sizesFile . JSON.encode . toJSON
        fillIn :: Map SC.ByteString Integer -> Stats SC.ByteString a -> IO (Stats (SC.ByteString, Integer) a)
        fillIn sizes stats
          = Map.fromList <$>
            mapM (\(fn, stats) ->
                   case Map.lookup fn sizes of
                        Just size ->
                          return ((fn, size), stats)
                        Nothing ->
                          do putStrLn $ "No file size for " ++ show fn
                             return $ ((fn, 100 * 1024 * 1024), stats)
                ) (Map.toList stats)

reduceFilenames :: Stats (SC.ByteString, Integer) Day -> Stats (SC.ByteString, SC.ByteString, Integer) Day
reduceFilenames
    = Map.mapKeys (\(fn, size) ->
                       let fn' = last $ split '/' fn
                           fn'' = split '.' fn'
                           ext = last fn''
                           fn''' = SC.intercalate (SC.singleton '.') $
                                   take (length fn'' - 1) fn''
                       in (fn''', ext, size)
                  )
    where split :: Char -> SC.ByteString -> [SC.ByteString]
          split c s = case SC.break (== c) s of
                        (s, s'') | SC.null s'' -> [s]
                        (s', s'') -> s' : split c (SC.tail s'')
                        
limitByHost :: Stats (SC.ByteString, Integer) (Day, Host) -> Stats (SC.ByteString, Integer) (Day, Host)
limitByHost = Map.mapWithKey $ \(fn, size) ->
              Map.map $ \transmitted ->
              min size transmitted
              
forgetHosts :: Stats (SC.ByteString, Integer) (Day, Host) -> Stats (SC.ByteString, Integer) Day
forgetHosts = Map.map $ 
              Map.foldrWithKey (\(day, host) transmitted ->
                                 Map.alter (Just . (+ transmitted) . fromMaybe 0) day
                               ) Map.empty

groupByExt :: Stats (SC.ByteString, SC.ByteString, Integer) Day -> Map SC.ByteString (Stats (SC.ByteString, Integer) Day)
groupByExt
    = Map.foldrWithKey (\(fn, ext, size) stats ->
                         Map.alter (Just . Map.insert (ext, size) stats . fromMaybe Map.empty
                                   ) fn
                       ) Map.empty

class JAble t where
  toJ :: t -> JSON.JSValue
  
instance (JAble k, JAble a) => JAble (Map k a) where
  toJ = JSON.JSObject . JSON.toJSObject .
        map (\(k, a) ->
                 let JSON.JSString k' = toJ k
                 in (JSON.fromJSString k', toJ a)
            ) .
        Map.toList
instance JAble Integer where
  toJ = JSON.JSRational False . fromInteger
instance JAble Double where
  toJ = JSON.JSRational True . toRational
instance JAble Day where
  toJ = JSON.JSString . JSON.toJSString . show
instance JAble SC.ByteString where
  toJ = JSON.JSString . JSON.toJSString . SC.unpack

createJSON :: Map SC.ByteString (Stats (SC.ByteString, Integer) Day) -> IO ()
createJSON = writeFile "index.json" .
             JSON.encode .
             toJ . convertSizesToDownloads
    where convertSizesToDownloads :: Map SC.ByteString (Stats (SC.ByteString, Integer) Day)
                                  ->  Map SC.ByteString (Map SC.ByteString (Map Day Double))
          convertSizesToDownloads = Map.map $
                                    Map.mapKeys fst .
                                    Map.mapWithKey (\(ext, size) extStats ->
                                                        Map.map ((/ fromIntegral size) . fromIntegral) extStats
                                                   )

createGraphs :: Map SC.ByteString (Stats (SC.ByteString, Integer) Day) -> IO ()
createGraphs fnStats
    = do forM_ (Map.toList fnStats) $ \(fn, extStats) ->
             do putStrLn $ SC.unpack fn ++ " " ++ (show $ Map.keys extStats)
                render fn extStats
         writeFile "index.html" indexSource
    where indexSource = "<h1>Pentamedia Stats</h1>" ++
                        concatMap (\(fn, extStats) ->
                                       "<h2>" ++ SC.unpack fn ++ "</h2>" ++
                                       "<p>" ++ printf "%.1f" (downloads extStats) ++ " downloads</p>" ++
                                       "<img src=\"" ++ SC.unpack fn ++ ".png\"/>"
                                  ) (Map.toList fnStats)
          downloads :: Stats (SC.ByteString, Integer) Day -> Double
          downloads
              = foldl' (\sum ((ext, fileSize), stats) ->
                            sum + (fromIntegral (Map.fold (+) 0 stats) / fromIntegral fileSize)
                       ) 0 .
                Map.toList
          render fn extStats
              = do dataSources <- forM (zip [0..] $ Map.toList extStats) $ \(i, ((ext, size), stats)) ->
                                  do writeData i size stats
                                     return $ (ext, dataFile i)
                   writeFile plotSourceFile $ plotSource (SC.unpack fn ++ ".png") dataSources
                   system $ "gnuplot " ++ plotSourceFile
                   removeFile plotSourceFile
                   mapM_ removeFile $ map (SC.unpack . snd) dataSources
                   return ()
          plotSource :: String -> [(SC.ByteString, SC.ByteString)] -> String
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
                               "'" ++ SC.unpack dataFile ++  "' using 1:2 title '" ++ SC.unpack ext ++ "' with boxes"
                          ) (zip [0..] dataSources)
          plotSourceFile = "graph.gnuplot"
          dataFile :: Int -> SC.ByteString
          dataFile i = SC.pack $ "data-" ++ show i
          writeData :: Int -> Integer -> FileStats Day -> IO ()
          writeData i fileSize stats
              = withFile (SC.unpack $ dataFile i) WriteMode $ \f ->
                forM_ (fillDayStats stats) $ \(day, size) ->
                hPutStrLn f $ show day ++ " " ++ show (fromIntegral size / fromIntegral fileSize)
          fillDayStats :: FileStats Day -> [(Day, Integer)]
          fillDayStats stats
              = do day <- dateRange (fst $ Map.findMin stats, fst $ Map.findMax stats)
                   return (day, fromMaybe 0 $ Map.lookup day stats)

createOutput :: Map SC.ByteString (Stats (SC.ByteString, Integer) Day) -> IO ()
createOutput fnStats = createJSON fnStats >>
                       createGraphs fnStats

main = C.getContents >>=
       return .
       foldl' (flip collectRequest) Map.empty .
       filter (isPentaMedia . reqPath) .
       filter reqIsGet .
       map parseLine .
       C.lines >>=
       getFileSizes >>=
       createOutput .
       groupByExt .
       reduceFilenames .
       forgetHosts .
       limitByHost
