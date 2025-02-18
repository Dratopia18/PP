module Parser (parseLambda, parseLine) where

import Control.Monad
import Control.Applicative
import Data.Char

import Lambda
import Binding

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

-- failParser e un parser care nu accepta niciun input
failParser :: Parser a
failParser = Parser $ \s -> Nothing -- nu accepta niciun input

-- charParser e un parser care accepta un caracter anume
charParser :: Char -> Parser Char
charParser c = Parser $ \s -> case s of -- pentru un input s
    x:xs | x == c -> Just (c, xs) -- daca primul caracter e c, atunci returneaza c si restul inputului
    _ -> Nothing -- altfel, nu accepta inputul

-- predicateParser e un parser care accepta un caracter daca functia primita e adevarata
predicateParser :: (Char -> Bool) -> Parser Char
predicateParser p = Parser $ \s -> case s of -- pentru un input s
    x:xs | p x -> Just (x, xs) -- daca primul caracter e in functia p, atunci returneaza caracterul si restul inputului
    _ -> Nothing -- altfel, nu accepta inputul

-- Monad e o interfata care permite definirea operatiilor return si bind
instance Monad Parser where
    return a = Parser $ \s -> Just (a, s) -- return a e un parser care accepta orice input s si returneaza a si s
    m >>= f = Parser $ \s -> case parse m s of -- daca facem bind (>>=) intre un parser m si o functie f si un input s, aplicam parserul m pe inputul s
        Nothing -> Nothing -- daca nu accepta inputul, nu accepta nici parserul
        Just (a, rest) -> parse (f a) rest -- altfel, parsam restul inputului cu f a

instance Applicative Parser where
    af <*> mp = do
        f <- af
        p <- mp
        return $ f p
    pure = return

-- Functor e o interfata care permite definirea operatiei fmap
instance Functor Parser where
    fmap f mp = do -- pentru un parser mp si o functie f
        p <- mp -- parsam mp
        return $ f p -- returnam f p

-- Alternative e o interfata care permite definirea operatiilor empty si <|>
instance Alternative Parser where
    empty = failParser -- empty e un parser care nu accepta niciun input (failParser)
    p1 <|> p2 = Parser $ \s -> case parse p1 s of -- p1 <|> p2 e un parser care accepta inputul s daca p1 sau p2 il accepta
        Nothing -> parse p2 s -- daca p1 nu accepta inputul, atunci incercam cu p2
        Just (a, rest) -> Just (a, rest) -- altfel, returnam a si restul inputului

-- plusParser e un parser care accepta unul sau mai multe elemente
plusParser :: (Parser a) -> Parser [a]
plusParser p = do -- pentru un parser p
    x <- p -- parsam p
    xs <- starParser p -- parsam p pana nu mai accepta inputul
    return (x:xs) -- returnam lista cu x si xs

-- starParser e un parser care accepta zero sau mai multe elemente
starParser :: Parser a -> Parser [a] 
starParser p = (plusParser p) <|> (return []) -- pentru un parser p, starParser p e pana la plusParser p sau []

-- whitespaceParser e un parser care accepta spatiu
whitespaceParser :: Parser [Char]
whitespaceParser = starParser (charParser ' ') -- starParser pentru spatiu

-- variableParser e un parser care accepta o variabila
variableParser :: Parser Lambda
variableParser = do 
    x <- predicateParser isAlpha -- primul caracter e litera
    xs <- starParser (predicateParser isAlphaNum) -- urmatoarele caractere sunt litere sau cifre
    return $ Var (x:xs) -- returnam variabila
-- lambdaParser e un parser care accepta o expresie lambda
lambdaParser :: Parser Lambda
lambdaParser = macroParser <|> variableParser <|> abstractionParser <|> applicationParser -- lambdaParser poate fi macroParser, variableParser, abstractionParser sau applicationParser

-- abstractionParser e un parser care accepta o abstractie
abstractionParser :: Parser Lambda
abstractionParser = do
    _ <- charParser '\\' -- caracterul 'λ'
    _ <- whitespaceParser -- spatiu
    var <- variableParser -- variabila
    let x = case var of -- x e variabila
            Var s -> s -- daca e Var, atunci x e s
            _ -> error "abstractionParser: invalid input" -- altfel, returnam eroare
    _ <- whitespaceParser -- spatiu
    _ <- charParser '.' -- punem punct
    _ <- whitespaceParser -- spatiu
    body <- lambdaParser -- corpul abstractiei
    return $ Abs x body -- returnam abstractia

-- applicationParser e un parser care accepta o aplicatie
applicationParser :: Parser Lambda
applicationParser = do
    _ <- charParser '(' -- deschidem paranteza
    _ <- whitespaceParser -- spatiu
    e1 <- lambdaParser -- prima expresie
    _ <- whitespaceParser -- spatiu
    e2 <- lambdaParser -- a doua expresie
    _ <- whitespaceParser -- spatiu
    _ <- charParser ')' -- inchidem paranteza
    return $ App e1 e2 -- returnam aplicatia

-- isUpperOrDigit verifica daca un caracter e litera mare sau cifra
isUpperOrDigit :: Char -> Bool
isUpperOrDigit c = isUpper c || isDigit c

-- macroParser e un parser care accepta un macro
macroParser :: Parser Lambda
macroParser = do
    x <- plusParser (predicateParser isUpperOrDigit) -- un macro e format din litere mari sau cifre
    return $ Macro x -- returnam macro

-- bindingParser e un parser care accepta o legatura
bindingParser :: Parser Line
bindingParser = do
    var <- plusParser (predicateParser isUpper) -- variabila e formata din litere mari
    _ <- whitespaceParser -- spatiu
    _ <- charParser '=' -- punem egal
    _ <- whitespaceParser -- spatiu
    expr <- lambdaParser -- punem expresia
    return $ Binding var expr -- returnam legatura

-- evalParser e un parser care accepta o evaluare
evalParser :: Parser Line
evalParser = do
    expr <- lambdaParser -- punem expresia
    return $ Eval expr -- returnam evaluarea

-- 2.1. / 3.2.
-- parseLambda primeste un sir de caractere si returneaza expresia lambda corespunzatoare
parseLambda :: String -> Lambda
parseLambda s = case parse lambdaParser s of -- parsam lambdaParser pe s
    Just (l, "") -> l -- daca accepta inputul, returnam l
    _ -> error "parseLambda: invalid input" -- altfel, returnam eroare

-- 3.3.
-- parseLine primeste un sir de caractere si returneaza o legatura sau o evaluare
parseLine :: String -> Either String Line
parseLine s = case parse (bindingParser <|> evalParser) s of -- parsam bindingParser sau evalParser pe s
    Just (l, "") -> Right l -- daca accepta inputul, returnam l
    _ -> Left "parseLine: invalid input" -- altfel, returnam eroare
