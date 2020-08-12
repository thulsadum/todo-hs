module Todo
  ( Todo,
    isDone,
    getDescription,
    getCompletionDate,
    getPriority,
    getCreationDate,
  )
where

-- import Text.Read
-- import Text.ParserCombinators.ReadPrec

import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM, replicateM)
import Data.Char (isSpace)
import Data.Time (Day, fromGregorian)
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadP (count)

-- TODO Tag parsing

data Todo = Todo
  { isDone :: Bool,
    getPriority :: Maybe String,
    getCompletionDate :: Maybe Day,
    getCreationDate :: Maybe Day,
    getDescription :: String
  }
  deriving (Show, Eq)

instance Read Todo where
  readsPrec _ s = fixParsedTodo <$> result
    where
      result =
        case readP_to_S parseTodo s of
          [] -> []
          [x] -> [x]
          x : _ -> [x] -- NOTE / FIXME this seems to be quite a crude fix to the ambiguous parsing

-- FIXME would be better to fix grammar, i guess
fixParsedTodo :: (Todo, String) -> (Todo, String)
fixParsedTodo
  ( todo@Todo
      { getCompletionDate = Just date,
        getCreationDate = Nothing
      },
    s
    ) = (todo {getCompletionDate = Nothing, getCreationDate = Just date}, s)
fixParsedTodo x = x

-- instance Show Todo where
--     show = undefined

{-----------------------
    PARSING HELPER
-------------------------}

parseTodo :: ReadP Todo
parseTodo = do
  done <- optDone
  prio <- optPriority
  complDate <- optCompletionDate
  createDate <- optCreationDate
  desc <- description
  return $ Todo done prio complDate createDate desc

digit :: ReadP Char
digit = choice $ map char ['0' .. '9']

rest :: ReadP [Char]
rest = length <$> look >>= \n -> count n get

maybeParse :: ReadP a -> ReadP (Maybe a)
maybeParse p = option Nothing $ Just <$> p

-- parseDate :: ReadPrec Day
parseDate :: ReadP Day
parseDate = do
  year <- read <$> count 4 digit
  char '-'
  month <- read <$> count 2 digit
  char '-'
  day <- read <$> count 2 digit

  return $ fromGregorian year month day

optDone :: ReadP Bool
optDone = option False $ trueP <* munch1 isSpace
  where
    trueP = char 'x' >> return True

optPriority :: ReadP (Maybe String)
optPriority = maybeParse aPrio
  where
    aPrio = between (char '(') (char ')') (munch1 $ (/= ')')) <* skipSpaces

optCompletionDate :: ReadP (Maybe Day)
optCompletionDate = maybeParse parseDate'
  where
    parseDate' = parseDate <* skipSpaces

optCreationDate :: ReadP (Maybe Day)
optCreationDate = maybeParse parseDate'
  where
    parseDate' = parseDate <* skipSpaces

description :: ReadP String
description = rest
