{
  module Main where
}

%name sentence
%tokentype { Token }
%error { parseError }

%token 
      
      var             { TokenVar $$ }
      'v'             { TokenOr }
      '&'             { TokenAnd }
      '~'             { TokenNeg }
      '('             { TokenOB }
      ')'             { TokenCB }
      ">"             { TokenIf }
      "="             { TokenBicond }

%%

Sentence  : var                               { Var $1 }
          | '~' Sentence                      { Neg $2 }
          | '(' Sentence 'v' Sentence ')'     { Disj $2 $4 }
          | '(' Sentence '&' Sentence ')'     { Conj $2 $4 }
          | '(' Sentence '>' Sentence ')'     { Cond $2 $4 }
          | '(' Sentence "=" Sentence ')'     { Bicond $2 $4 }

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
  
  data Token
     = TokenVar Symbol
     | TokenOr
     | TokenAnd
     | TokenNeg
     | TokenOB
     | TokenCB
     | TokenIf
     | TokenBicond
  deriving Show

  lexer :: String -> [Token]
  lexer [] = []
  lexer (c:cs) 
        | isSpace c = lexer cs
        | isAlpha c = lexVar (c:cs)
  lexer ('v':cs) = TokenOr     : lexer cs
  lexer ('&':cs) = TokenAnd    : lexer cs
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
