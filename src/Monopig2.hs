{-# LANGUAGE OverloadedStrings, LambdaCase, GeneralizedNewtypeDeriving #-}
module Monopig2 where

import Text.Read (readMaybe)
import Data.Semigroup (Max(..), stimes)
import Data.Monoid
import Data.Vector ((//), (!), Vector)
import qualified Data.Vector as V (replicate)



type Stack = [Int]
type Memory = Vector Int
--type Processor = VM -> VM

memSize = 4

data VM a = VM { stack :: Stack
               , status :: Maybe String
               , memory :: Memory
               , journal :: a
               } deriving Show

mkVM = VM mempty mempty (V.replicate memSize 0)

--setStack :: Stack -> Processor
setStack x (VM _ s m l) = VM x s m l

--setStatus :: Maybe String -> Processor
setStatus x (VM s _ m l) = VM s x m l

--setMemory :: Memory -> Processor
setMemory x (VM s st _ l) = VM s st x l

addRecord x (VM s st m j) = VM s st m (x <> j)

newtype Action a = Action { runAction :: a -> a }

instance Semigroup (Action a) where
    Action f <> Action g = Action (g . f)

instance Monoid (Action a) where
    mempty = Action id

newtype Program a = Program { getProgram :: Action (VM a) }
    deriving (Semigroup, Monoid)

type Program' a = (VM a -> VM a) -> Program a

--run :: Program -> Processor -- Action (f :: VM -> VM) -> (VM -> VM)
run = runAction . getProgram

--exec :: Program -> VM
exec prog = run (prog id) (mkVM ())

execLog p prog = run (prog $ \vm -> addRecord (p vm) vm) (mkVM mempty)

logStack vm = [stack vm]
logStackUsed = Max . length . stack
logSteps= const (Sum 1)
logMemoryUsed = Max . getSum . count . memory
    where count = foldMap (\x -> if x == 0 then 0 else 1)
--err :: String -> Processor
err m = setStatus . Just $ "Error! " ++ m

--program :: (Stack -> Processor) -> Program
program f p = Program . Action $
    \vm -> case status vm of
        Nothing -> p . (f (stack vm)) $ vm
        m -> vm

--programM :: ((Memory, Stack) -> Processor) -> Program
programM f p = Program . Action $
    \vm -> case status vm of
        Nothing -> p . (f (memory vm, stack vm)) $ vm
        m -> vm

pop = program $
    \case x:s -> setStack s
          _    -> err "pop expected an argument!\n"

push x = program $! \s -> setStack (x:s)

dup = program $
    \case x:s -> setStack (x:x:s)
          _   -> err "dup expected an argument!\n"

swap = program $!
    \case x:y:s -> setStack (y:x:s)
          _     -> err "swap exptected two arguments!\n"

exch = program $!
    \case x:y:s -> setStack (y:x:y:s)
          _     -> err "exch expected two arguments!\n"

indexed i f = programM $! if (i < 0 || i >= memSize)
                        then const $ err $ "expected index within 0 and " ++ show memSize
                        else f
put i = indexed i $!
    \case (m, x:s) -> setStack s . setMemory (m // [(i, x)])
          _        -> err "put expected an argument!\n"

get i = indexed i $! \(m, s) -> setStack ((m ! i) : s)

unary n f = program $
    \case x:s -> setStack (f x:s)
          _   -> err $ "operation " ++ show n ++ " expected an argument"

binary n f = program $!
    \case x:y:s -> setStack (f x y:s)
          _     -> err $ "operation " ++ show n ++ " expected two arguments"

add    = binary "add" (+)
sub    = binary "sub" (flip (-))
mul    = binary "mul" (*)
frac   = binary "frac" (flip div)
modulo = binary "modulo" (flip mod)
neg    = unary "neg" (\x -> -x)
inc    = unary "inc" (\x -> x+1)
dec    = unary "dec" (\x -> x-1)
eq     = binary "eq" (\x -> \y -> if (x == y) then 1 else 0)
neq    = binary "neq" (\x -> \y -> if (x /= y) then 1 else 0)
lt     = binary "lt" (\x -> \y -> if (x > y) then 1 else 0)
gt     = binary "gt" (\x -> \y -> if (x < y) then 1 else 0)

--branch :: Program -> Program -> Program
branch br1 br2 p = program go id
    where go (x:s) = proceed p (if (x /= 0) then br1 else br2) s
          go _     = err "branch expected and argument!\n"

--while :: Program -> Program -> Program
while test body p = program (const go) id
    where go vm = let res = proceed p test (stack vm) vm
              in case (stack res) of
                     0:s -> proceed p mempty s res
                     _:s -> go $ proceed p body s res
                     _   -> err "vhile expected an argument!\n" vm

--rep :: Program -> Program
rep body p = program go id
    where go (n:s) = proceed p (stimes n body) s
          go _     = err "rep expected an argument!\n"

-- proceed :: Program -> Stack -> Processor
proceed p prog s = run (prog p) . setStack s


fact = dup <> push 2 <> lt <>
       branch (push 1) (dup <> dec <> fact) <>
       mul

fact1 = push 1 <> swap <>
        while (dup <> push 1 <> gt)
        (
            swap <> exch <> mul <> swap <> dec
        ) <>
        pop

range = exch <> sub <> dup<> push 0 <> gt <> branch (rep (dup <> inc)) (neg <> rep (dup <> dec))

{-
 range = exch <> sub <> dup<> push 0 <> gt <>
fact2 = inc <> push 1 <> swap <> range <> dec <> dec <> rep mul
range1 = exch <> sub <> rep (dup <> inc)

 - -}
range1 = exch <> sub <> rep (dup <> inc)

fact2 = mconcat [push 1, swap, range, dup, dec, rep mul]
fact4 = mconcat [ dec, push 2, swap, range, push 3, sub, rep mul]
toTrace = exec $ push 4 <> fact4
-- итеративный факториал с использованием памяти
fact3 = dup <> put 0 <> dup <> dec <>
        rep (dec <> dup <> get 0 <> mul <> put 0) <>
        get 0 <> swap <> pop

copy2 = exch <> exch

gcd1 = while (copy2 <> neq)
       (
           copy2 <> lt <> branch mempty (swap) <> exch <> sub
       ) <>
       pop

pow = swap <> put 0 <> push 1 <> put 1 <>
      while (dup <> push 0 <> gt)
      (
        dup <> push 2 <> modulo <>
        branch (dec <> get 0 <> dup <> get 1 <> mul <> put 1) (get 0) <>
        dup <> mul <> put 0 <>
        push 2 <> frac
      ) <>
      pop <> get 1

f &&& g = \r -> (f r, g r)

{-
sieve = push 2 <>
        while (dup <> dup <> mul <> push memSize <> lt)
        (dup <> get <> branch mempty  <> inc) <>
        pop
-}
{-
fill = dup <> dup <> add <>
       while (dup <> push memSize <> lt)
       (dup <> push 1 <> swap <> put <> exch <> add) <>
       pop
-}
