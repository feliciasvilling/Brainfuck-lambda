import Text.Parsec hiding (State)
import Control.Monad.State

data Mode = Strict | ByName

data Exp = App Mode Exp Exp | Var String | Lam String Exp

type Env = [(String, Val)]

data Val =  Thunk Env Exp | Closure Env String Exp | Inc | Dec | ErrorVal String

-- exp =  exp exp | "\" var* "." exp | var "=" exp ";" exp | "(" exp ")" | var
-- var = "<" | ">" | "+" | "-" | "!" | "?" | "#" | alpha*

strip ('(':cs) = strip' [] cs where
    strip' acc [')'] = reverse acc
    strip' acc (c:cs) = strip' (c:acc) cs
    strip' acc [] = reverse acc
strip cs = cs


instance Show Exp where
    show e = strip $ showExp e where
        showExp :: Exp -> String
        showExp (App _ (App m a b) c) = "(" ++ strip (showExp $ App m a b) ++ " " ++ showExp c ++ ")"
        showExp (App _ a b) = "(" ++ showExp a ++ " " ++ showExp b ++ ")"
        showExp (Var s) = s
        showExp (Lam v1 (Lam v2 b)) = "(\\" ++ v1 ++ " " ++ tail (strip $ show $ Lam v2 b) ++ ")"
        showExp (Lam v b) = "(\\" ++ v ++ "." ++ strip (show b) ++ ")"


instance Show Val where
    show (Thunk _ exp) = show exp
    show (Closure _ v b) = "\\" ++ v ++ "." ++ strip (show b)
    show Inc = "+"
    show Dec = "-"
    show (ErrorVal s) = "error: " ++ s

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
    return $ foldl1 app atoms

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
    return $ app (Lam var exp2) exp1

expression = do 
    spaces
    exp <- try declaration <|> lambda <|> application 
    spaces
    return exp

grammar :: Parsec String () Exp
grammar = expression


eval :: [(String, Val)] -> Exp -> State Int Val
eval env (Var s) = case lookup s env of
    Just val -> return val
    Nothing -> return $ ErrorVal $ "cant find find variable " ++ s ++ " in environment"
eval env (Lam var body) = return $ Closure env var body
eval env (App Strict exp1 exp2) = do
    val1 <- eval env exp1
    val2 <- eval env exp2
    fun <- unthunk val1
    apply fun val2
eval env (App ByName exp1 exp2) = do
    val1 <- eval env exp1
    fun <- unthunk val1
    apply fun $ Thunk env exp2

apply :: Val -> Val -> State Int Val
apply (Closure env var body) arg = eval ((var, arg): env) body
apply Inc arg = do
    old <- get
    put $ old + 1
    return arg
apply Dec arg = do
    old <- get
    put $ old - 1
    return arg

unthunk :: Val -> State Int Val
unthunk (Thunk env body) = do
    val <- eval env body
    unthunk val
unthunk val = return val

initialStore = 0
app = App Strict
builtin = [("+", Inc),("-", Dec)]

main = do
    let source = "(\\x. x x x) (++)"
    let parsed = parse grammar "fuckup" source
    print parsed
    let (result, store) = case parsed of
            Right program -> runState (eval builtin program) initialStore
            Left error -> (ErrorVal $ show error, initialStore)
    print result
    print store
