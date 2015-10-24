type Symbol = String
data Sentence = Atom    Symbol
              | Neg     Sentence
              | Conj    Sentence Sentence
              | Disj    Sentence Sentence
              | Bicond  Sentence Sentence
              | Cond    Sentence Sentence

-- TODO make this moral
instance Show Sentence where
  show (Atom s) = s
  show (Neg s) = "~" ++ show s
  show (Conj s1 s2) = "(" ++ show s1 ++ " & " ++ show s2 ++ ")"
  show (Disj s1 s2) = "(" ++ show s1 ++ " v " ++ show s2 ++ ")"
  show (Bicond s1 s2) = "(" ++ show s1 ++ " <-> " ++ show s2 ++ ")"
  show (Cond s1 s2) = "(" ++ show s1 ++ " -> " ++ show s2 ++ ")"

main = do
  print (Atom "Goerge");
  print (Neg (Conj (Atom "a")(Atom "b")));
