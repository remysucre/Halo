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
