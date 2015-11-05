data Derivation = 
        Assume Sentence
      | OrInt Derivation Sentence
      | OrElim { a, b :: Sentence, ad, bd :: Derivation }
      | AndInt { a, b :: Sentence, ad, bd :: Derivation }
      | AndElim Derivation Sentence 
      | NegInt { a :: Sentence, ad :: Derivation }
      | NegElim -- TODO

derived :: Derivation -> Sentence
derived (Assume phi) = phi
derived (OrInt _ phi) = phi
derived (OrElim {ad = d1, bd = d2 }) 
  | derived d1 == derived d2 = derived d1
  | otherwise = error "invalid derivation"
derived (AndInt {a = phi1, b = phi2 }) = Conj phi1 phi2
derived (AndElim d phi) = phi
derived (NegInt { a = phi, ad = d1}) = (Neg phi)
