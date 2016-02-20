module Charla

data Numero = Cero | Suc Numero

infixr 9 :/:
data Lista : Numero -> Type -> Type where
  Fin   : Lista Cero e
  (:/:) : e -> Lista n e -> Lista (Suc n) e

repetir : (n : Numero) -> e -> Lista n e
repetir Cero    x = Fin
repetir (Suc n) x = x :/: repetir n x
