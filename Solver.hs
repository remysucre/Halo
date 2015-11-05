{-# LANGUAGE BangPatterns #-}
{-# OPTIONS -Wall -Werror -fno-warn-missing-signatures  #-}
-- import Data.List
import System.Environment (getArgs)
import Prelude hiding (fail, succ)

import Grammar

type Assignment = [(Symbol, Bool)]

makeFormulaTrue f fail succ = makeFormula f True [] fail succ

-- with partial assignment cur, try to make formula produce
-- the value bool. A
makeFormula :: Sentence -> Bool -> Assignment -> a -> (Assignment -> a -> a) -> a
makeFormula (Var x) bool = makeLit x bool
makeFormula (Neg s) bool = makeFormula s (not bool)
makeFormula (Conj s1 s2) True = makeAll s1 s2 True
makeFormula (Conj s1 s2) False = makeAny s1 s2 False
makeFormula (Disj s1 s2) True = makeAny s1 s2 True
makeFormula (Disj s1 s2) False = makeAll s1 s2 False
makeFormula (Cond s1 s2) bool = makeFormula (s1 `implies` s2) bool
makeFormula (Bicond s1 s2) bool = makeFormula (s1 `equiv` s2) bool

s1 `implies` s2 = Neg s1 `Disj` s2
s1 `equiv` s2 = (s1 `implies` s2) `Conj` (s2 `implies` s1)

makeAll, makeAny :: Sentence -> Sentence -> Bool -> Assignment -> a -> (Assignment -> a -> a) -> a
makeAll s1 s2 bool cur fail succeed = 
        makeFormula s1 bool cur fail 
                                (\cur' resume -> makeFormula s2 bool cur' resume succeed)

makeAny s1 s2 bool cur fail succeed = 
        makeFormula s1 bool cur (makeFormula s2 bool cur fail succeed)
                                succeed

makeLit lit bool cur fail succeed  
        | satisfies lit bool cur = succeed cur fail
        | binds lit cur = fail
        | otherwise = succeed (bind lit bool cur) fail

bind lit bool cur = (lit, bool) : cur

satisfies lit bool alist = 
      case lookup lit alist
        of Just bool' -> bool == bool'
           _ -> False

binds var al = 
      case lookup var al
        of Just _ -> True
           _ -> False


-- test = (sentence . lexer) "(a & a)"
main = do 
  [sent] <- getArgs
  let ast = (sentence . lexer) sent
  print $ makeFormulaTrue ast Nothing (\cur _ -> Just cur) 
