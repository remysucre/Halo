{-# LANGUAGE BangPatterns #-}
import Grammar
import Data.List

type Assignment = [(Symbol, Bool)]

makeFormulaTrue f fail succ = makeFormula f True [] fail succ

-- with partial assignment cur, try to make formula produce
-- the value bool. 
makeFormula (Var x) bool cur fail succeed = 
            makeLit x bool cur fail succeed
makeFormula (Neg s) bool cur fail succeed = 
            makeFormula s (not bool) cur fail succeed
makeFormula (Conj s1 s2) True cur fail succeed = 
            makeAll s1 s2 True cur fail succeed
makeFormula (Conj s1 s2) False cur fail succeed = 
            makeAny s1 s2 False cur fail succeed
makeFormula (Disj s1 s2) True cur fail succeed = 
            makeAny s1 s2 True cur fail succeed
makeFormula (Disj s1 s2) False cur fail succeed = 
            makeAll s1 s2 False cur fail succeed

makeAll s1 s2 bool cur fail succeed = 
        makeFormula s1 bool cur fail 
                                (\cur resume -> makeFormula s2 bool cur fail succeed)

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
        of Just bool -> True
           _ -> False

binds var al = 
      case lookup var al
        of Just _ -> True
           _ -> False


test = (sentence . lexer) "(a & ~a)"
main = print $ makeFormulaTrue test [] (\cur res -> cur) 
