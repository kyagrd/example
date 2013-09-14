{-# LANGUAGE OverloadedStrings #-}

-- | You can find a tutorial covering this template at https://www.fpcomplete.com/school/attoparsec

module Main where

import Data.Word
import Data.Time
import Data.Attoparsec.Char8
import Control.Applicative
import Data.Either (rights)
import Data.Monoid hiding (Product)
import Data.String
import Data.Char (toLower)
import Data.Foldable (foldMap)
-- ByteString stuff
import Data.ByteString.Char8 (ByteString,singleton)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Lazy (toChunks)
-- HTTP protocol to perform downloads
import Network.HTTP.Conduit

----------------------
------- FILES --------
----------------------

data File = URL String | Local FilePath

-- | Files where the logs are stored.
--   Modify this value to read logs from
--   other sources.
logFiles :: [File]
logFiles = tail $ tail $
  [ Local "sellings.log"
  , Local "sellings2.log"
  , URL "http://daniel-diaz.github.io/misc/sellings3.log"
  ]

getFile :: File -> IO ByteString
-- simpleHttp gets a lazy bytestring, while we
-- are using strict bytestrings.
getFile (URL str) = mconcat . toChunks <$> simpleHttp str
getFile (Local fp) = B.readFile fp

-----------------------
-------- TYPES --------
-----------------------

-- | Type for IP's.
data IP = IP Word8 Word8 Word8 Word8 deriving (Eq,Show)

-- | Type for products.
data Product = Mouse | Keyboard | Monitor | Speakers deriving (Eq,Show,Enum)

productFromID :: Int -> Product
productFromID n = toEnum (n-1)

data Source = Internet | Friend | NoAnswer deriving (Eq,Show)

-- | Each log entry in the log file is represented by a value
--   of this type. Modify the fields of 'LogEntry' accordingly
--   to your log file of interest. However, 'entryTime' is a
--   reasonable field and is also used for merging.
data LogEntry =
  LogEntry { entryTime :: LocalTime
           , entryIP   :: IP
           , entryProduct   :: Product
           , source    :: Source
             } deriving (Eq, Show)

instance Ord LogEntry where
  le1 <= le2 = entryTime le1 <= entryTime le2

type Log = [LogEntry]

-----------------------
------- PARSING -------
-----------------------

-- | Parser of values of type 'IP'.
parseIP :: Parser IP
parseIP = do
  d1 <- decimal
  char '.'
  d2 <- decimal
  char '.'
  d3 <- decimal
  char '.'
  d4 <- decimal
  return $ IP d1 d2 d3 d4

-- | Parser of values of type 'LocalTime'.
timeParser :: Parser LocalTime
timeParser = do
  y  <- count 4 digit
  char '-'
  mm <- count 2 digit
  char '-'
  d  <- count 2 digit
  char ' '
  h  <- count 2 digit
  char ':'
  m  <- count 2 digit
  char ':'
  s  <- count 2 digit
  return $
    LocalTime { localDay = fromGregorian (read y) (read mm) (read d)
              , localTimeOfDay = TimeOfDay (read h) (read m) (read s)
                }

-- | Parser of values of type 'Product'.
productParser :: Parser Product
productParser =
     (string "mouse"    >> return Mouse)
 <|> (string "keyboard" >> return Keyboard)
 <|> (string "monitor"  >> return Monitor)
 <|> (string "speakers" >> return Speakers)

sourceParser :: Parser Source
sourceParser =
      (string "internet" >> return Internet)
  <|> (string "friend" >> return Friend)
  <|> (string "noanswer" >> return NoAnswer)

-- | Parser of log entries.
logEntryParser :: Parser LogEntry
logEntryParser = do
  t <- timeParser
  char ' '
  ip <- parseIP
  char ' '
  p <- productParser
  s <- option NoAnswer $ char ' ' >> sourceParser
  return $ LogEntry t ip p s

logParser :: Parser Log
logParser = many $ logEntryParser <* endOfLine

-----------------------
------- MERGING -------
-----------------------

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) =
  if x <= y
     then x : merge xs (y:ys)
     else y : merge (x:xs) ys

-----------------------
------ RENDERING ------
-----------------------

-- | Character that will serve as field separator.
--   It should not be one of the characters that
--   appear in the fields.
sepChar :: Char
sepChar = ','

-- | Rendering of IP's to ByteString.
renderIP :: IP -> ByteString
renderIP (IP a b c d) =
     fromString (show a)
  <> singleton '.'
  <> fromString (show b)
  <> singleton '.'
  <> fromString (show c)
  <> singleton '.'
  <> fromString (show d)

-- | Render a log entry to a CSV row as ByteString.
renderEntry :: LogEntry -> ByteString
renderEntry le =
     fromString (show $ entryTime le)
  <> singleton sepChar
  <> renderIP (entryIP le)
  <> singleton sepChar
  <> fromString (fmap toLower $ show $ entryProduct le)
  <> singleton sepChar
  <> fromString (fmap toLower $ show $ source le)

-- | Render a log file to CSV as ByteString.
renderLog :: Log -> ByteString
renderLog = foldMap $ \le -> renderEntry le <> singleton '\n'

----------------------
-------- MAIN --------
----------------------

main :: IO ()
main = do
  files <- mapM getFile logFiles
  let -- Parsed logs
      logs :: [Log]
      logs = rights $ fmap (parseOnly logParser) files
      -- Merged log
      mergedLog :: Log
      mergedLog = foldr merge [] logs
  BC.putStrLn $ renderLog mergedLog
