module Todo (Todo, isDone, getDescription) where

-- import Text.Read
-- import Text.ParserCombinators.ReadPrec
import Data.Time (fromGregorian, Day)
import Control.Applicative ((<*>),(<$>))
import Text.ParserCombinators.ReadP
import Control.Monad (replicateM, forM)
import Text.ParserCombinators.ReadP (count)
import Data.Char (isSpace)


-- TODO Tag parsing

data Todo =
    Todo { isDone            ::       Bool
         , getPriority       :: Maybe String
         , getCompletionDate :: Maybe Day
         , getCreationDate   :: Maybe Day
         , getDescription    ::       String
    } deriving (Show)

instance Read Todo where
    readsPrec _ s = result
        where result = 
                case readP_to_S parseTodo s of
                    [] -> []
                    [x] -> [x]
                    x:_ -> [x] -- NOTE / FIXME this seems to be quite a crude fix to the ambiguous parsing


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
    where   trueP = char 'x' >> return True


optPriority :: ReadP (Maybe String)
optPriority = maybeParse aPrio
    where   aPrio  = between (char '(')  (char ')') (munch1 $ (/= ')'))  <* skipSpaces


optCompletionDate :: ReadP (Maybe Day)
optCompletionDate = maybeParse parseDate'
    where parseDate' = parseDate <* skipSpaces

optCreationDate :: ReadP (Maybe Day)
optCreationDate = maybeParse parseDate'
    where parseDate' = parseDate <* skipSpaces

description :: ReadP String
description = rest
