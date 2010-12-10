{-# LANGUAGE TemplateHaskell #-}
module Printf (printf) where
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Numeric
import Control.Applicative
import Data.Maybe
import Control.Monad

data Printf = Lit Char
            | String
            | Dec
            | Float
            | Show
            | Char
              deriving (Show, Eq)

readDec' :: String ->  Maybe(Int, String)
readDec' = listToMaybe . readDec

parsePrintf :: String -> [Printf]
parsePrintf ""     = []
parsePrintf (c:cs) = fromJust $ 
  case c of
    '%' -> do (sym, rest) <- readSym cs
              return (sym : parsePrintf rest)
          <|> return (Lit '%' : parsePrintf cs)
    _   -> return (Lit c : parsePrintf cs)
  where
    readSym [] = Nothing
    readSym (c':cs') = case c' of
                         's' -> return (String, cs')
                         'S' -> return (Show, cs')
                         'c' -> return (Char, cs')
                         'd' -> return (Dec, cs')
                         'f' -> return (Float, cs')
                         '%' -> return (Lit '%', cs')
                         _   -> Nothing

buildFun :: [Printf] -> ExpQ
buildFun [] = stringE ""
buildFun xs = do
  funs <- mapM toExpQ xs
  lamE (map varP $ mapMaybe snd funs) (foldr appE [|""|] $ map fst funs)

toExpQ (Lit c) = return ([|(c:)|] , Nothing)
toExpQ fun     = do
  name <- newName "a"
  exp  <- case fun of
            String -> [| (++) $(varE name) |]
            Show   -> [| (++) (show $(varE name)) |]
            Dec    -> [| (++) (show ($(varE name) :: Int)) |]
            Float  -> [| (++) (show ($(varE name) :: Float)) |]
            Char   -> [| (:) $(varE name) |]  
  return (return exp, Just name)

printf :: QuasiQuoter
printf = QuasiQuoter { quoteExp = buildFun . parsePrintf
                     , quotePat = undefined}