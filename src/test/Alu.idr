module Alu 

import AluInput
import Text.Lexer
import Text.Parser.Core
import Text.Parser
import Data.List
import Data.SnocList
import Data.SortedMap as M
import Control.App
import Control.App.Console

%default total

maxZ = 26 * 26 * 26 + 19

data Var = W | X | Y | Z
data Val = ConstVal Integer | VarVal Var

data Op = Input Var 
        | Add Var Val
        | Mul Var Val
        | Div Var Val
        | Mod Var Val
        | Eql Var Val

Show Var where
        show W = "w"
        show X = "x"
        show Y = "y"
        show Z = "z"

Show Val where
        show (ConstVal i) = show i
        show (VarVal v) = show v

Show Op where
        show (Input v) = "inp " ++ show v
        show (Add v v') = show v ++ " = " ++ show v ++ " + " ++ show v'
        show (Mul v v') = show v ++ " = " ++ show v ++ " * " ++ show v'
        show (Div v v') = show v ++ " = " ++ show v ++ " / " ++ show v'
        show (Mod v v') = show v ++ " = " ++ show v ++ " % " ++ show v'
        show (Eql v v') = show v ++ " = " ++ show v ++ " == " ++ show v'

data Token = Command String | Const Integer | Variable String | Spaces

Show Token where
        show (Command s) = s
        show (Const i) = show i
        show (Variable s) = s
        show (Spaces) = " "

toInt' : String -> Integer
toInt' = cast

command : Lexer
command = choice $ map exact ["inp", "add", "mul", "div", "mod", "eql"]

variable : Lexer
variable = choice $ map exact ["w", "x", "y", "z"]

tokens : TokenMap Token
tokens = [
        (intLit, \x => Const (toInt' x)),
        (command, \x => Command x),
        (variable, \x => Variable (cast x)),
        (spaces, \x => Spaces)
]

var : Grammar state Token True Var
var = terminal "Can't parse Var" (\s => 
        case s of
        Variable "w" => Just W
        Variable "x" => Just X
        Variable "y" => Just Y
        Variable "z" => Just Z
        _ => Nothing)

constVal : Grammar state Token True Val
constVal = terminal "Can't parse const Val" (\s =>
        case s of
        Const n => Just (ConstVal n)
        _ => Nothing)


val : Grammar state Token True Val
val = constVal <|> (VarVal <$> var)

matchCommand : String -> Grammar state Token True String
matchCommand cmd = terminal "Can't parse command" (\s =>
        case s of
            Command x => if x == cmd then Just x else Nothing
            _ => Nothing)

doInp : Grammar state tok True String ->
    Grammar state tok True Var ->
    Grammar state tok True Op
doInp cmd v = do
        cmd' <- cmd
        v' <- v
        pure $ Input v'

op : Grammar state Token True Op 
op = Input <$> ((matchCommand "inp") *> var) <|>
     Add <$> ((matchCommand "add") *> var) <*> val <|>
     Mul <$> ((matchCommand "mul") *> var) <*> val <|>
     Div <$> ((matchCommand "div") *> var) <*> val <|>
     Mod <$> ((matchCommand "mod") *> var) <*> val <|>
     Eql <$> ((matchCommand "eql") *> var) <*> val

filterSpaces = List.filter (\x => case x.val of
        Spaces => False
        _ => True)

record State where
        constructor MkState
        w : Integer
        x : Integer
        y : Integer
        z : Integer

Show State where
        show (MkState w x y z) = "w: " ++ show w ++ " x: " ++ show x ++ " y: " ++ show y ++ " z: " ++ show z

getVal : Val -> State -> Integer
getVal (ConstVal i) = \x => i
getVal (VarVal x) = \s =>
        case x of
        W => s.w
        X => s.x
        Y => s.y
        Z => s.z

calcOp : (Integer -> Integer -> Integer) -> Val -> Var -> State -> State
calcOp oper value variable state = 
        case variable of
        W => {w := oper state.w (getVal value state)} state
        X => {x := oper state.x (getVal value state)} state
        Y => {y := oper state.y (getVal value state)} state
        Z => {z := oper state.z (getVal value state)} state

boolToInt : Bool -> Integer
boolToInt True = 1
boolToInt False = 0

InputList = SnocList Integer
StateMap = SortedMap State InputList

-- We will at each point in time have a map of all possible states into
-- the largest input which could lead into this state. By the end we
-- need to take a look at the state where z is 0 and find the largest input.

Eq State where
        (==) = \x, y => x.w == y.w && x.x == y.x && x.y == y.y && x.z == y.z

Ord State where
        compare (MkState w x y z) (MkState w' x' y' z') = compare (w, x, y, z) (w', x', y', z')

-- return lexicographically largest list
f : InputList -> InputList -> InputList
f = \x, y => if x < y then x else y

step : Op -> (State, InputList) -> StateMap
step (Input x) = \(s, l) => fromList $ map 
        (\curInput => (calcOp (\x, y => y) (ConstVal curInput) x s,
         l:<curInput)) [1..9]
step (Add x y) = \(s, l) => singleton (calcOp (+) y x s) l
step (Mul x y) = \(s, l) => singleton (calcOp (*) y x s) l
step (Div x y) = \(s, l) => singleton (calcOp (div) y x s) l
step (Mod x y) = \(s, l) => singleton (calcOp (mod) y x s) l
step (Eql x y) = \(s, l) => singleton (calcOp (\a, b => boolToInt (a == b)) y x s) l

filterBigZ : StateMap -> StateMap
filterBigZ sm = fromList $ filter (\(s, l) => s.z < maxZ) $ toList sm

applyOne : Op -> StateMap -> (State, InputList) -> StateMap
applyOne op acc curState =
        mergeWith f acc (filterBigZ $ step op curState)

apply : StateMap -> Op -> StateMap
apply curMap op =  foldl (applyOne op) empty (M.toList curMap)

getOpList : Nat -> List Op
getOpList n = concatMap (\op => case op of
        Right x => [fst x]
        Left er => []) $ map (parse op) (map (filterSpaces . fst . (lex tokens)) (take n input))

initMap : StateMap
initMap = singleton (MkState 0 0 0 0) empty

finalMap : Nat -> StateMap
finalMap n = foldl apply initMap (getOpList n)

main : IO()
main = do
        n <- getLine
        printLn (sort $ map snd $ filter (\(s, l) => s.z == 0) $ M.toList $ finalMap (cast n))