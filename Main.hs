module Main where

import Data.Scientific hiding (scientific)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (scientific)
import Data.Void
import System.Environment
import System.Exit
import System.IO


{-
language
e = c | x | e e | \x -> e | let x = e in e
-}

data E1 =
  Con1 Scientific |
  App1 E1 E1 |
  Var1 String |
  Lam1 String E1 |
  Let1 String E1 E1
    deriving Show

-- parser
type Parser a = Parsec Void String a

parser :: Parser E1
parser = do
  e <- expression
  eof
  return e

expression :: Parser E1
expression = do
  terms <- some term
  return (foldl1 App1 terms)

term :: Parser E1
term =
  grouped <|>
  constant <|>
  lambda <|>
  try variable <|>
  letbinding

constant :: Parser E1
constant = fmap Con1 (symbol scientific)

grouped :: Parser E1
grouped = do
  symbol (char '(')
  inner <- expression
  symbol (char ')')
  return inner

lambda :: Parser E1
lambda = do
  symbol (char '\\')
  x <- symbol identifier
  symbol (string "->")
  body <- expression
  return (Lam1 x body)

identifier :: Parser String
identifier = do
  c <- lowerChar
  cs <- many alphaNumChar
  case c:cs of
    "let" -> fail "let is not an allowed identifier"
    "in"  -> fail "in is not an allowed identifier"
    _     -> return (c:cs)
  
variable :: Parser E1
variable = fmap Var1 (symbol identifier)

letbinding :: Parser E1
letbinding = do
  symbol (string "let")
  x <- symbol identifier
  symbol (char '=')
  e1 <- expression
  symbol (string "in")
  e2 <- expression
  return (Let1 x e1 e2)

symbol :: Parser a -> Parser a
symbol p = do
  x <- p
  space -- zero or more spaces
  return x

-- evaluator

-- crunch an expression as far as it will go
eval :: E1 -> E1
eval (Var1 name) = Var1 name
eval (Con1 x) = Con1 x
eval (Lam1 name body) = Lam1 name body
eval (App1 e1 e2) = case eval e1 of
  Lam1 name body -> eval (subst name e2 body)
  other          -> App1 other (eval e2)
eval (Let1 name e1 e2) = eval (subst name e1 e2)

subst :: String -> E1 -> E1 -> E1
subst name rep (Var1 name')
  | name == name' = rep
  | otherwise = Var1 name'
subst name rep (Con1 x) = Con1 x
subst name rep (Lam1 name' body)
  | name == name' = Lam1 name' body
  | otherwise = Lam1 name' (subst name rep body)
subst name rep (Let1 name' e1 e2)
  | name == name' = Let1 name' (subst name rep e1) e2
  | otherwise = Let1 name' (subst name rep e1) (subst name rep e2)
subst name rep (App1 e1 e2) = App1 (subst name rep e1) (subst name rep e2)


-- printer

flat :: E1 -> String
flat e0 = go 0 e0 where
  go _ (Con1 x) = show x
  go _ (Var1 name) = name
  go 2 (App1 e1 e2) = par (go 0 (App1 e1 e2))
  go _ (App1 e1 e2) = go 1 e1 ++ " " ++ go 2 e2
  go 0 (Lam1 name body) = "\\" ++ name ++ " -> " ++ go 0 body
  go _ (Lam1 name body) = par (go 0 (Lam1 name body))
  go 0 (Let1 name e1 e2) = "let " ++ name ++ " = " ++ go 0 e1 ++ " in " ++ go 0 e2
  go _ (Let1 name e1 e2) = par (go 0 (Let1 name e1 e2))
  par x = "(" ++ x ++ ")"

pp :: E1 -> String
pp e0 = go 0 0 e0 where
  go lvl _ (Con1 x) = show x
  go lvl _ (Var1 name) = name
  go lvl 2 (App1 e1 e2) = par (go lvl 0 (App1 e1 e2))
  go lvl _ (App1 e1 e2)
    | sizeLte (flat e2) 50 = go lvl 1 e1 ++ " " ++ go lvl 2 e2
    | otherwise = go lvl 1 e1 ++ "\n" ++ indent (lvl+1) ++ go (lvl+1) 2 e2
  go lvl 0 (Lam1 name body) = "\\" ++ name ++ " -> " ++ go lvl 0 body
  go lvl _ (Lam1 name body) = par (go lvl 0 (Lam1 name body))
  go lvl 0 (Let1 name e1 e2)
    | not (isLet e2) && sizeLte (flat e2) 50 =
        "let " ++ name ++ " = " ++ go lvl 0 e1 ++ " in " ++ go lvl 0 e2
    | otherwise = "let " ++ name ++ " = " ++ go lvl 0 e1 ++ " in " ++
                  "\n" ++ indent lvl ++ go lvl 0 e2
  go lvl _ (Let1 name e1 e2) = par (go (lvl+1) 0 (Let1 name e1 e2))
  par x = "(" ++ x ++ ")"
  indent lvl = replicate (lvl*2) ' '
  sizeLte "" _ = True
  sizeLte (c:cs) 0 = False
  sizeLte (c:cs) n = sizeLte cs (n-1)
  isLet (Let1 _ _ _) = True
  isLet _ = False



-- runner program
main = do
  [infile] <- getArgs
  txt <- readFile infile
  case parse parser infile txt of
    Left err -> do
      hPutStrLn stderr (parseErrorPretty err)
      exitFailure
    Right prog -> case eval prog of
      Con1 x -> print x
      Lam1 name body -> do
        x <- getInputNumber
        let result = eval (App1 (Lam1 name body) (Con1 x))
        putStrLn (pp result)
      other -> do
        hPutStrLn stderr "bad program:"
        hPutStrLn stderr (pp other)
        exitFailure

getInputNumber :: IO Scientific
getInputNumber = do
  raw <- getContents
  case parse (scientific :: Parser Scientific) "" raw of
    Right x -> return x
    Left _ -> do
      hPutStrLn stderr "bad input, not a number"
      exitFailure

