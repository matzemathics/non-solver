{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}

module NonParser where

import Control.Applicative hiding (many)
import Control.Monad
import Control.Monad.Fail
import qualified Data.Bifunctor as Bi
import Data.Functor
import Data.Monoid
import Numeric
import Text.Parsec hiding ((<|>))

data NonFile = NonFile
  { nf_metadata :: [MetaData],
    nf_colors :: [(Char, RGB)],
    nf_width :: Int,
    nf_height :: Int,
    nf_rows :: [Hints],
    nf_cols :: [Hints],
    nf_goal :: Maybe [Char]
  }
  deriving (Show)

data Key
  = NfCatalogue
  | NfTitle
  | NfBy
  | NfCopyright
  | NfLicense
  | NfColor
  | NfWidth
  | NfHeight
  | NfRows
  | NfCols
  | NfGoal
  deriving (Eq, Show)

type Hints = [(Int, Maybe Char)]

data Field
  = Meta MetaData
  | Color (Char, RGB)
  | Width Int
  | Height Int
  | Rows [Hints]
  | Cols [Hints]
  | Goal [Char]
  deriving (Show)

type Dimensions = (Maybe Int, Maybe Int)

type MetaData = (Key, String)

data RGB = RGB Int Int Int deriving (Eq, Show)

-- >>> parse (metadata) "" "title \"test\""
-- Right (Meta (NfTitle,"test"))

nfString :: Parsec String u [Char]
nfString = char '\"' >> manyTill anyChar (char '\"')

metafield :: Key -> Parsec String u Field
metafield k = do
  try (string $ keyName k) >> spaces
  Meta . (,) k <$> nfString <* endField

metadata :: Parsec String u Field
metadata =
  metafield NfCatalogue <|> metafield NfTitle
    <|> metafield NfBy
    <|> metafield NfCopyright

license :: Parsec String u Field
license =
  try (string "license") >> spaces
    >> Meta . (,) NfLicense <$> (nfString <* endField <|> restOfLine)

anyOf :: [Char] -> Parsec String u [Char]
anyOf = many . oneOf

-- >>> parse endField "" ""
-- Right ()

endField :: Parsec String u ()
endField = void (anyOf " \t" >> endOfLine) <|> eof

first :: (Foldable t, Alternative m) => t a -> m a
first = getAlt . foldMap (Alt . pure)

byte :: Parsec String u Int
byte = fmap fst (head . readHex <$> count 2 hexDigit) <?> "byte"

color :: Parsec String u Field
color = do
  try (string "color") >> spaces
  i <- lower <* spaces <?> "character designation"
  [r, g, b] <- char '#' >> count 3 byte <?> "hex color"
  spaces >> endOfLine
  return $ Color (i, RGB r g b)

restOfLine :: Parsec String u String
restOfLine = manyTill anyChar (try endOfLine)

width :: Parsec String Dimensions Field
width = updateDim "width" Bi.first Width

height :: Parsec String Dimensions Field
height = updateDim "height" Bi.second Height

int :: Parsec String u Int
int = read <$> many1 digit

updateDim k m f = do
  try (string k) >> spaces
  n <- int <* endField
  modifyState (m (<|> return n))
  return $ f n

des :: Parsec String u (Maybe Char)
des = optionMaybe lower

hints :: Parsec String u Hints
hints = (((,) <$> int <*> des) `sepBy` (char ',' >> spaces)) <* endField

hintBlock :: Key -> (u -> Maybe Int) -> Parsec String u [Hints]
hintBlock k f = do
  try (string $ keyName k) >> spaces
  dims <- getState >>= first . f
  count dims hints

rows :: Parsec String Dimensions Field
rows = Rows <$> hintBlock NfRows snd

cols :: Parsec String Dimensions Field
cols = Cols <$> hintBlock NfCols fst

goal :: Parsec String u Field
goal = try (string "goal") >> spaces >> Goal <$> nfString <* endField

nonField :: Parsec String Dimensions Field
nonField = metadata <|> license <|> color <|> width <|> height <|> rows <|> cols <|> goal

-- >>> parse (many anyChar) "" "ab."
-- Right "ab."

nonFile :: Parsec String Dimensions [Field]
nonFile = concat <$> many (return <$> nonField <|> restOfLine $> [])

getKey :: Field -> Key
getKey (Color _) = NfColor
getKey (Width _) = NfWidth
getKey (Height _) = NfHeight
getKey (Rows _) = NfRows
getKey (Cols _) = NfCols
getKey (Goal _) = NfGoal
getKey (Meta (a, _)) = a

parseNonFile :: String -> String -> Either ParseError (Either String NonFile)
parseNonFile f s = mkNonFile `Bi.second` runParser nonFile (Nothing, Nothing) f s

defNonFile :: Int -> Int -> [Hints] -> [Hints] -> NonFile
defNonFile h w r c =
  NonFile
    { nf_metadata = [],
      nf_colors = [],
      nf_width = w,
      nf_height = h,
      nf_rows = r,
      nf_cols = c,
      nf_goal = Nothing
    }

keyName :: Key -> [Char]
keyName NfHeight = "height"
keyName NfWidth = "width"
keyName NfRows = "rows"
keyName NfCols = "columns"
keyName NfCatalogue = "catalogue"
keyName NfTitle = "title"
keyName NfBy = "by"
keyName NfCopyright = "copyright"

getField :: Key -> [(Key, b)] -> Either [Char] b
getField k = maybe (Left $ "missing " ++ keyName k) Right . lookup k

instance MonadFail (Either [Char]) where
  fail = Left

setField :: NonFile -> Field -> NonFile
setField nf (Color c) = nf {nf_colors = c : nf_colors nf}
setField nf (Goal g) = nf {nf_goal = nf_goal nf <|> Just g}
setField nf (Meta m) = nf {nf_metadata = nf_metadata nf ++ [m]}
setField nf _ = nf

mkNonFile :: [Field] -> Either [Char] NonFile
mkNonFile fs = do
  let kf = [(getKey x, x) | x <- fs]
  (Width w) <- getField NfWidth kf
  (Height h) <- getField NfHeight kf
  (Rows r) <- getField NfRows kf
  (Cols c) <- getField NfCols kf
  return $ foldl setField (defNonFile h w r c) fs