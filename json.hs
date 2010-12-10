{-# LANGUAGE TemplateHaskell, QuasiQuotes, GADTs, DeriveDataTypeable #-}
{-# LANGUAGE OverlappingInstances, TypeSynonymInstances, FlexibleInstances #-}
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Parsec hiding ((<|>))
import Text.Parsec.Pos
import Data.Generics
import Control.Applicative hiding (many)

data JSON = JSNull                    -- ^ 空
          | JSNumber Int              -- ^ 数
          | JSString String           -- ^ 文字列
          | JSArray  [JSON]           -- ^ 配列
          | JSObject [(String, JSON)] -- ^ オブジェクト
          | JSQuote  String           -- ^ アンチクォート
            deriving (Show, Eq, Data, Typeable)

class IsJSON a where
  toJSON :: a -> JSON

instance IsJSON JSON where
  toJSON = id

instance IsJSON Int where
  toJSON = JSNumber

instance IsJSON Integer where
  toJSON = JSNumber . fromInteger

instance IsJSON String where
  toJSON = JSString

instance IsJSON a => IsJSON [a] where
  toJSON = JSArray . map toJSON

instance IsJSON () where
  toJSON = const JSNull

instance IsJSON a => IsJSON [(String, a)] where
  toJSON = JSObject . map (\(a,b) -> (a, toJSON b))

json :: Parsec String () JSON
json = lexeme (jsquote <|> jsnull <|> try jsnumber <|> try jsstring <|> try jsary <|> jsobj)

lexeme :: Parsec String () a -> Parsec String () a 
lexeme p = skipMany space *> p

symbol :: String -> Parsec String () String
symbol   = lexeme . string

jsnull, jsnumber, jsstring, jsary, jsobj :: Parsec String () JSON
jsnull   = JSNull <$ symbol "null"
jsnumber = JSNumber . read <$> many1 digit
jsstring = JSString <$> between (symbol "\"") (string "\"") (many1 $ noneOf "\"")
jsary    = JSArray  <$> between (symbol "[") (symbol "]") (json `sepBy` symbol ",")
jsobj    = JSObject <$> between (symbol "{") (symbol "}") (pair `sepBy` symbol ",")
pair = (,) <$> (ident <* symbol ":") <*> json
jsquote  = JSQuote <$> (symbol "`" *> ident)

ident :: Parsec String () String
ident = (:) <$> lexeme (letter <|> char '_') <*> many (alphaNum <|> char '_')

js :: QuasiQuoter
js = QuasiQuoter { quoteExp = parseExp
                 , quotePat = parsePat
                 }

parseExp :: String -> ExpQ
parseExp str = do
  loc <- location
  let pos = uncurry (newPos $ loc_filename loc) (loc_start loc)
      ans = parse (setPosition pos >> (json <* many space)) (loc_filename loc) str
      jsv = either (error.show) id ans
  dataToExpQ (const Nothing `extQ` antiQuoteE) jsv

antiQuoteE :: JSON -> Maybe ExpQ
antiQuoteE (JSQuote nm) = Just $ appE (varE 'toJSON) (varE $ mkName nm)
antiQuoteE _ = Nothing

parsePat = undefined