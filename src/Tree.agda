module Tree where

open import Data.Vec
open import Data.Nat

data Head : Set where
  H : Head

infixr 5 _⇒_
data Expr : Set where
  S   :                             Expr -- something
  _⇒_ :               Expr → Expr → Expr -- "->" expression
  E   : Head → ∀{n} →  Vec Expr n → Expr -- arbitrary expression

-- the termination checker is very unhappy with this one
leftBalance : Expr → Expr
leftBalance S                 = S
leftBalance (e ⇒ S)           = e ⇒ S
leftBalance (e ⇒ (f ⇒ g))     = leftBalance ((e ⇒ f) ⇒ g)
leftBalance (e ⇒ E H [])      = (e ⇒ E H [])
leftBalance (e ⇒ E H (a ∷ x)) = E H (leftBalance (leftBalance e ⇒ a) ∷ (map leftBalance x))
leftBalance (E H x)           = E H (map leftBalance x)


-- leftBalance (e ⇒ E H (a ∷ x)) = E H ((leftBalance (e ⇒ a)) ∷ (map leftBalance x))
