{
  module Main where
}

%name sentence
%tokentype { Token }
%error { parseError }

%token 
      
      var             { TokenVar $$ }
      '|'             { TokenDisj }
      '&'             { TokenConj }
      '~'             { TokenNeg }
      '('             { TokenOB }
      ')'             { TokenCB }
      '>'             { TokenCond }
      '='             { TokenBicond }

%%

Sentence  : var                               { Var $1 }
          | '~' Sentence                      { Neg $2 }
          | '(' Sentence '|' Sentence ')'     { Disj $2 $4 }
          | '(' Sentence '&' Sentence ')'     { Conj $2 $4 }
          | '(' Sentence '>' Sentence ')'     { Cond $2 $4 }
          | '(' Sentence '=' Sentence ')'     { Bicond $2 $4 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

type Symbol = String
data Sentence = Var     Symbol
              | Neg     Sentence
              | Conj    Sentence Sentence
              | Disj    Sentence Sentence
              | Bicond  Sentence Sentence
              | Cond    Sentence Sentence

instance Show Sentence where
  show (Var s) = s
  show (Neg s) = "~" ++ show s
  show (Conj s1 s2) = "(" ++ show s1 ++ " & " ++ show s2 ++ ")"
  show (Disj s1 s2) = "(" ++ show s1 ++ " v " ++ show s2 ++ ")"
  show (Bicond s1 s2) = "(" ++ show s1 ++ " <-> " ++ show s2 ++ ")"
  show (Cond s1 s2) = "(" ++ show s1 ++ " -> " ++ show s2 ++ ")"

data Token
   = TokenVar Symbol
   | TokenDisj
   | TokenConj
   | TokenNeg
   | TokenOB
   | TokenCB
   | TokenIf
   | TokenBicond
   | TokenCond
  deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
      | isSpace c = lexer cs
      | isAlpha c = lexVar (c:cs)
lexer ('|':cs) = TokenDisj     : lexer cs
lexer ('&':cs) = TokenConj    : lexer cs
lexer ('~':cs) = TokenNeg    : lexer cs
lexer ('=':cs) = TokenBicond : lexer cs
lexer ('>':cs) = TokenCond   : lexer cs
lexer ('(':cs) = TokenOB     : lexer cs
lexer (')':cs) = TokenCB     : lexer cs

lexVar cs =
   case span isAlpha cs of
      (var,rest)   -> TokenVar var : lexer rest

main = getContents >>= print . sentence . lexer
}
