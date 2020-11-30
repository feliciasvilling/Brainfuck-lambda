

import Text.Parsec

data Exp = App Exp Exp | Var String | Lam String Exp

type Env = [(String, Val)]

data Val = Closure Env String Exp | Literal String | ErrorVal String deriving Show

-- exp =  exp exp | "\" var* "." exp | var "=" exp ";" exp | "(" exp ")" | var
-- var = "<" | ">" | "+" | "-" | "!" | "?" | "#" | alpha*

strip ('(':cs) = strip' [] cs where
    strip' acc [')'] = reverse acc
    strip' acc (c:cs) = strip' (c:acc) cs
    strip' acc [] = reverse acc
strip cs = cs


instance Show Exp where
    show e = strip $ showExp e where
        showExp (App (App a b) c) = "(" ++ strip (showExp $ App a b) ++ " " ++ showExp c ++ ")"
        showExp (App a b) = "(" ++ showExp a ++ " " ++ showExp b ++ ")"
        showExp (Var s) = s
        showExp (Lam v1 (Lam v2 b)) = "(\\" ++ v1 ++ " " ++ tail (strip $ show $ Lam v2 b) ++ ")"
        showExp (Lam v b) = "(\\" ++ v ++ "." ++ strip (show b) ++ ")"

reserved = "\\.=;() "

alpha = do 
    name <- many1 $ oneOf ['0'..'9'] <|> letter
    return $ Var name

single = do
    name <- noneOf reserved
    return $ Var [name]

variable = alpha <|> single

parens = do
    char '('
    exp <- expression
    char ')'
    return exp


application = do
    atoms <- sepBy (variable <|> parens) spaces
    return $ foldl1 App atoms

lambda = do
    char '\\'
    vars <- sepBy (many1 letter) spaces
    char '.'
    exp <- expression
    return $ foldr Lam exp vars

declaration = do
    Var var <- variable
    spaces
    char '='
    exp1 <- expression
    char ';'
    exp2 <- expression
    return $ App (Lam var exp2) exp1

expression = do 
    spaces
    exp <- try declaration <|> lambda <|> application 
    spaces
    return exp

grammar :: Parsec String () Exp
grammar = expression


eval env (Var s) = case lookup s env of
    Just val -> val
    Nothing -> ErrorVal $ "cant find find variable " ++ s ++ " in environment"
eval env (Lam var body) = Closure env var body
eval env (App exp1 exp2) = apply val1 val2 where
    val1 = eval env exp1
    val2 = eval env exp2

apply (Closure env var body) arg = eval ((var, arg): env) body

main = do
    let source = "true = \\x. x; true"
    let parsed = parse grammar "fuckup" source
    print parsed
    let result = case parsed of
            Right program -> eval [] program
            Left error -> ErrorVal $ show error

    print result