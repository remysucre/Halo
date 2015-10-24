import System.Environment
import Grammar

type TTable = [Row]
type Row = [(Sentence, Bool)]

truthTable :: Sentence -> TTable
truthTable s = map (row s) atomAssigns
  where atomAssigns = assign $ atoms s

atoms :: Sentence -> [Symbol]
atoms (Var s) = [s]
atoms (Neg s) = atoms s
atoms (Conj s1 s2) = atoms s1 ++ atoms s2
atoms (Disj s1 s2) = atoms s1 ++ atoms s2
atoms (Bicond s1 s2) = atoms s1 ++ atoms s2
atoms (Cond s1 s2) = atoms s1 ++ atoms s2

row :: Sentence -> [(Symbol, Bool)] -> Row
row sen sbs = sss ++ [eval sen sbs]
  where sss = map toSent sbs
        toSent (s, b) = (Var s, b)

eval :: Sentence -> [(Symbol, Bool)] -> (Sentence, Bool)
eval sen sbs = 
  case sen 
    of (Var x) -> (sen, xb) where Just xb = lookup x sbs
       (Neg s) -> (sen, not $ snd (eval s sbs))
       (Conj s1 s2) -> (sen, and [(snd (eval s1 sbs)), (snd (eval s2 sbs))])
       (Disj s1 s2) -> (sen, or [(snd (eval s1 sbs)), (snd (eval s2 sbs))])
       (Cond s1 s2) -> (sen, or [(not $ snd (eval s1 sbs)), snd (eval s2 sbs)])
       (Bicond s1 s2) -> (sen, snd (eval s1 sbs) == snd (eval s2 sbs))

assign :: [Symbol] -> [[(Symbol, Bool)]]
assign ss = foldr addBind [[]] ss
  where addBind a binds = foldr (\b bs -> ((a, True) : b) : ((a, False):  b) : bs) [] binds

prettyPrint :: TTable -> String
prettyPrint tt = unlines $ map (\r -> printR r ++ "\n") tt
  where printR r = concat $ map (\(s, b) -> show s ++ " = " ++ show b ++ " ") r

main = do 
  [f] <- getArgs
  s <- readFile f
  let sen = sentence $ lexer $ s
  putStr $ prettyPrint $ truthTable sen
