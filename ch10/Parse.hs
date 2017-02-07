-- file: ch10/Parse.hs
module Parse where
  import PNM
  import Data.Int (Int64)
  import Data.Word (Word8)
  import Data.Char(chr)
  import qualified Data.ByteString.Lazy as L
  import qualified Data.ByteString.Lazy.Char8 as L8

  data ParseState = ParseState {
        string :: L.ByteString
      , offset :: Int64
      } deriving (Show)

  newtype Parse a = Parse {
    runParse :: ParseState -> Either String (a, ParseState)
  }

  simpleParse :: ParseState -> (a, ParseState)
  simpleParse = undefined

  betterParse :: ParseState -> Either String (a, ParseState)
  betterParse = undefined

  -- The identity parser
  identity :: a -> Parse a
  identity a = Parse (\s -> Right (a, s))

  parse :: Parse a -> L.ByteString -> Either String a
  parse parser initState
    = case runParse parser (ParseState initState 0) of
      Left err          -> Left err
      Right (result, _) -> Right result

  -- Record syntax, updates, and pattern matching
  modifyOffset :: ParseState -> Int64 -> ParseState
  modifyOffset initState newOffset =
    initState { offset = newOffset }

  parseByte :: Parse Word8
  parseByte =
    getState ==> \initState ->
    case L.uncons (string initState) of
      Nothing ->
          bail "no more input"
      Just (byte,remainder) ->
          putState newState ==> \_ ->
          identity byte
        where newState = initState { string = remainder,
                                     offset = newOffset }
              newOffset = offset initState + 1

  -- obtaining and modifying the parse state
  getState :: Parse ParseState
  getState = Parse (\s -> Right (s, s))

  putState :: ParseState -> Parse ()
  putState s = Parse (\_ -> Right ((), s))

  bail :: String -> Parse a
  bail err = Parse $ \s -> Left $
           "byte offset " ++ show (offset s) ++ ": " ++ err

  (==>) :: Parse a -> (a -> Parse b) -> Parse b
  firstParser ==> secondParser  =  Parse chainedParser
    where chainedParser initState   =
            case runParse firstParser initState of
              Left errMessage ->
                  Left errMessage
              Right (firstResult, newState) ->
                  runParse (secondParser firstResult) newState

  instance Functor Parse where
    fmap f parser = parser ==> \result -> identity (f result)

  -- Using Functors for Parsing
  w2c :: Word8 -> Char
  w2c = chr . fromIntegral

  parseChar :: Parse Char
  parseChar = w2c <$> parseByte

  peekByte :: Parse (Maybe Word8)
  peekByte = (fmap fst . L.uncons . string) <$> getState
  
  peekChar :: Parse (Maybe Char)
  peekChar = fmap w2c <$> peekByte

  parseWhile :: (Word8 -> Bool) -> Parse [Word8]
  parseWhile p = (fmap p <$> peekByte) ==> \mp ->
                  if mp == Just True
                  then parseByte ==> \b ->
                    (b:) <$> parseWhile p
                  else identity []

  parseWhileVerbose p =
    peekByte ==> \mc ->
      case mc of
        Nothing -> identity []
        Just c | p c ->
                    parseByte ==> \b ->
                    parseWhileVerbose p ==> \bs ->
                    identity (b:bs)
               | otherwise ->
                    identity []