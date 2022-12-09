module Test.Intro

import Data.Vect
import Decidable.Equality

data EqualDenis : a -> a -> Type where
  ReflDenis : (x : b) -> EqualDenis x x

data LessEqual : Nat -> Nat -> Type where
  LEEqual: (x : Nat) -> LessEqual x x
  LELess: (x : Nat) -> (y : Nat) -> LessEqual x y -> LessEqual x (S y)

le46 : LessEqual 4 6
le46 = LELess 4 5 (LELess 4 4 (LEEqual 4))

trans_theorem : (n : Nat) -> (m : Nat) -> (k : Nat) -> LessEqual n m -> LessEqual m k -> LessEqual n k
trans_theorem n m m lenm (LEEqual m) = lenm
trans_theorem n m (S k) lenm (LELess m k lemk) = LELess n k (trans_theorem n m k lenm lemk)

x : EqualDenis 3 3
x = ReflDenis 3

data IsElem : (x : a) -> (xs : Vect n a) -> Type where
  IsElemHere : (x : a) -> (xs : Vect n a) -> IsElem x (x :: xs)
  IsElemThere : (x : a) -> (y : a) -> (xs : Vect n a) -> IsElem x xs -> IsElem x (y :: xs)


p1 : IsElem 1 [1, 1, 2]
p1 = IsElemHere 1 [1, 2]

p2 : IsElem 1 [1, 1, 2]
p2 = IsElemThere 1 1 [1, 2] (IsElemHere 1 [2])

elem : DecEq a => (x : a) -> (xs : Vect n a) -> Maybe (IsElem x xs)
elem x [] = Nothing
elem x (y::xs) with (decEq x y)
  elem x (x::xs) | Yes Refl = Just (IsElemHere x xs)
  elem x (y::xs) | No neq = case elem x xs of
    Nothing => Nothing
    Just isElem => Just (IsElemThere x y xs isElem)

remove : (x : a) -> (xs : Vect (S n) a) -> IsElem x xs -> Vect n a
remove x (x :: ys) (IsElemHere x ys) = ys
remove x (y :: z :: zs) (IsElemThere x y (z::zs) prf) = x :: (remove x (z::zs) prf)

xs : Vect 3 Nat
xs = [1, 1, 2]

ys : Vect 2 Nat
ys = remove 1 xs (IsElemThere 1 1 [1, 2] (IsElemHere 1 [2]))

doStuff : Maybe (Vect 2 Nat)
doStuff = do
  prf <- Intro.elem 1 xs
  let ys = Intro.remove 1 xs prf
  pure ys

main : IO ()
main = printLn "Hello, World!"