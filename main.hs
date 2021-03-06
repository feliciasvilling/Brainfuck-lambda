import Control.Monad.State
import Data.Char
import System.Environment
import System.IO
import Text.Parsec hiding (State)

data Mode = Strict | ByName

data Exp = App Mode Exp Exp | Var String | Lam String Exp

type Env = [(String, Val)]

data Val = Thunk Env Exp | Closure Env String Exp | Inc | Dec | Read | Ask | Tell | Back | Fore | ErrorVal String

-- exp =  exp exp | "\" var* "." exp | var "=" exp ";" exp | "(" exp ")" | var
-- var = "<" | ">" | "+" | "-" | "!" | "?" | "@" | alpha*

strip ('(' : cs) = strip' [] cs
  where
    strip' acc [')'] = reverse acc
    strip' acc (c : cs) = strip' (c : acc) cs
    strip' acc [] = reverse acc
strip cs = cs

instance Show Exp where
  show e = strip $ showExp e
    where
      showExp :: Exp -> String
      showExp (App Strict (Lam v body) exp) = v ++ " = " ++ showExp exp ++ ";\n" ++ showExp body
      showExp (App _ (App m a b) c) = "(" ++ strip (showExp $ App m a b) ++ " " ++ showExp c ++ ")"
      showExp (App _ a b) = "(" ++ showExp a ++ " " ++ showExp b ++ ")"
      showExp (Var s) = s
      showExp (Lam v1 (Lam v2 b)) = "(\\" ++ v1 ++ " " ++ tail (strip $ show $ Lam v2 b) ++ ")"
      showExp (Lam v b) = "(\\" ++ v ++ ". " ++ strip (show b) ++ ")"

instance Show Val where
  show (Thunk _ exp) = "[" ++ show exp ++ "]"
  show (Closure _ v b) = "\\" ++ v ++ "." ++ strip (show b)
  show Inc = "+"
  show Dec = "-"
  show Ask = "?"
  show Tell = "!"
  show Back = "<"
  show Fore = ">"
  show (ErrorVal s) = "error: " ++ s

reserved = "\\.:=;()# "

comment = do
  char '#'
  many $ noneOf ['\n']
  return ' '

blank = many $ space <|> comment

alpha = do
  name <- many1 (oneOf ['0' .. '9'] <|> letter) <?> "alphanumeric"
  return $ Var name

single = do
  name <- noneOf reserved
  return $ Var [name]

variable = alpha <|> single <?> "variable"

parens = do
  char '('
  exp <- expression
  char ')'
  return exp

atom = variable <|> parens

application = do foldl1 app <$> application'
  where
    application' = do
      fun <- atom
      blank
      arg <-
        try application' <|> do
          a <- atom
          return [a]
      return $ fun : arg

lambda = do
  char '\\'
  blank
  vars <- many1 $ do
    l <- variable
    blank
    return l
  char '.' <|> char ':'
  exp <- expression
  return $ foldr lam exp vars
  where
    lam (Var s) body = Lam s body

declaration = do
  Var var <- variable
  blank
  char '='
  exp1 <- expression
  char ';'
  exp2 <- expression
  return $ App Strict (Lam var exp2) exp1

expression = do
  blank
  exp <- try declaration <|> lambda <|> try application <|> atom <?> "expression"
  blank
  return exp

grammar :: Parsec String () Exp
grammar = expression

eval :: [(String, Val)] -> Exp -> StateT ([Int], Int, [Int]) IO Val
eval env exp = do
  val <- eval' env exp
  unthunk val

eval' env (Var s) = case lookup s env of
  Just val -> return val
  Nothing -> return $ ErrorVal $ "cant find find variable " ++ s ++ " in environment"
eval' env (Lam var body) = return $ Closure env var body
eval' env (App Strict exp1 exp2) = do
  fun <- eval env exp1
  val2 <- eval' env exp2
  apply fun val2
eval' env (App ByName exp1 exp2) = do
  fun <- eval env exp1
  apply fun $ Thunk env exp2

apply (Closure env var body) arg = eval ((var, arg) : env) body
apply Inc arg = do
  (back, current, forward) <- get
  put (back, current + 1, forward)
  return arg
apply Dec arg = do
  (back, current, forward) <- get
  put (back, current - 1, forward)
  return arg
apply Read arg = do
  (_, current, _) <- get
  apply (church current) arg
apply Tell arg = do
  (_, current, _) <- get
  liftIO $ putChar $ chr current
  return arg
apply Ask arg = do
  ch <- liftIO getChar
  (back, _, forward) <- get
  put (back, ord ch, forward)
  return arg
apply Back arg = do
  state <- get
  case state of
    ([], current, forward) -> put ([], 0, current : forward)
    (new : back, current, forward) -> put (back, new, current : forward)
  return arg
apply Fore arg = do
  state <- get
  case state of
    (back, current, []) -> put (current : back, 0, [])
    (back, current, new : forward) -> put (current : back, new, forward)
  return arg
apply (ErrorVal e) _ = return $ ErrorVal e
apply _ (ErrorVal e) = return $ ErrorVal e
apply val _ = do
  return $ ErrorVal $ show val ++ " is not a function"

church n = Closure [] "f" $ Lam "x" $ iter n (Var "x")
  where
    iter 0 arg = arg
    iter n arg = app (Var "f") $ iter (n -1) arg

unthunk (Thunk env body) = do
  val <- eval' env body
  unthunk val
unthunk val = return val

initialStore = ([], 0, [])

app = App ByName

builtin = [("+", Inc), ("-", Dec), ("@", Read), ("?", Ask), ("!", Tell), ("<", Back), (">", Fore)]

main = do
  hSetBuffering stdin NoBuffering
  args <- getArgs
  let (debug, sourceFile) = case args of
        [source] -> (False, source)
        ["debug", source] -> (True, source)
  source <- readFile sourceFile
  let parsed = parse grammar "fuckup" source
  when debug $ print parsed
  res <- case parsed of
    Right program -> runStateT (eval builtin program) initialStore
    Left error -> return (ErrorVal $ show error, initialStore)
  putChar '\n'
  when debug $ print res
