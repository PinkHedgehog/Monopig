{-# LANGUAGE OverloadedStrings, LambdaCase, GeneralizedNewtypeDeriving, TupleSections #-}
module Monopig3 where


import Data.Semigroup (Max(..), stimes)
import Data.Monoid
import Data.Vector ((//), (!), Vector, toList)
import qualified Data.Vector as V (replicate)



type Stack = [Int]
type Memory = Vector Int
--type Processor = VM -> VM

memSize = 16

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

newtype Program a = Program { getProgram :: ([Code], Action (VM a)) }
    deriving (Semigroup, Monoid)


--run :: Program -> Processor -- Action (f :: VM -> VM) -> (VM -> VM)
none = const id
run = runAction . snd . getProgram

--exec :: Program -> VM
exec prog = run (prog none) (mkVM ())

execLog p prog = run (prog $ \c -> \vm -> addRecord (p c vm) vm) (mkVM mempty)

logStack _ vm = [stack vm]
logStackUsed _ = Max . length . stack
logSteps  _ = const (Sum 1)
-- logMemoryUsed - = Max . getSum . count . memory
--     where count = foldMap (\x -> if x == 0 then 0 else 1)

logCode c _ = [c]
logRun com vm = [pad 10 c ++ "| " ++ pad 20 s ++ "| " ++ m]
  where c = show com
        m = unwords $ show <$> toList (memory vm)
        s = unwords $ show <$> stack vm
        pad n x = take n (x ++ repeat ' ')
        
debug :: Program' [String] -> String
debug = unlines . reverse . journal . execLog logRun

err m = setStatus . Just $ "Error! " ++ m


type Program' a = (Code -> VM a -> VM a) -> Program a

program c f p = Program . ([c],) . Action $
    \vm -> case status vm of
        Nothing -> p c . f (stack vm) $ vm
        _ -> vm

programM c f p = Program . ([c],) . Action $
    \vm -> case status vm of
        Nothing -> p c . f (memory vm, stack vm) $ vm
        _ -> vm

pop = program POP $ 
  \case x:s -> setStack s
        _ -> err "POP expected an argument."

push x = program (PUSH x) $ \s -> setStack (x:s)

dup = program DUP $ 
  \case x:s -> setStack (x:x:s)
        _ -> err "DUP expected an argument."

swap = program SWAP $ 
  \case x:y:s -> setStack (y:x:s)
        _ -> err "SWAP expected two arguments."

exch = program EXCH $ 
  \case x:y:s -> setStack (y:x:y:s)
        _ -> err "EXCH expected two arguments."

app1 c f = program c $
  \case x:s -> setStack (f x:s)
        _ -> err $ "operation " ++ show c ++ " expected an argument"

app2 c f = program c $
  \case x:y:s -> setStack (f x y:s)
        _ -> err $ "operation " ++ show c ++ " expected two arguments"

add = app2 ADD (+)
sub = app2 SUB (flip (-))
mul = app2 MUL (*)
frac = app2 DIV (flip div)
mod = app2 MOD (flip Prelude.mod)
neg = app1 NEG (\x -> -x)
inc = app1 INC (\x -> x+1)
dec = app1 DEC (\x -> x-1)
eq = app2 EQL (\x -> \y -> if (x == y) then 1 else 0)
neq = app2 NEQ (\x -> \y -> if (x /= y) then 1 else 0)
lt = app2 LTH (\x -> \y -> if (x > y) then 1 else 0)
gt = app2 GTH (\x -> \y -> if (x < y) then 1 else 0)

proceed p prog s = run (prog p) . setStack s

rep body p = program (REP (toCode body)) go none
  where go (n:s) = if n >= 0
                   then proceed p (stimes n body) s
                   else err "REP expected positive argument."
        go _ = err "REP expected an argument."

branch br1 br2 p = program (IF (toCode br1) (toCode br2)) go none
   where go (x:s) = proceed p (if (x /= 0) then br1 else br2) s
         go _ = err "IF expected an argument."

while test body p = program (WHILE (toCode test) (toCode body)) (const go) none
  where go vm = let res = proceed p test (stack vm) vm
          in case (stack res) of
               0:s -> proceed p mempty s res
               _:s -> go $ proceed p body s res
               _ -> err "WHILE expected an argument." vm

put i = indexed (PUT i) i $
    \case (m, x:s) -> setStack s . setMemory (m // [(i,x)])
          _ -> err "PUT expected an argument"

get i = indexed (GET i) i $ \(m, s) -> setStack ((m ! i) : s)

indexed c i f = programM c $ if (i < 0 || i >= memSize)
                             then const $ err "index in [0,16]"
                             else f
fact = dup <> push 2 <> lt <>
       branch (push 1) (dup <> dec <> fact) <>
       mul

fact1 = push 1 <> swap <>
        while (dup <> push 1 <> gt)
        (
            swap <> exch <> mul <> swap <> dec
        ) <>
        pop

range = exch <> sub <> dup <> push 0 <> gt <> branch (rep (dup <> inc)) (neg <> rep (dup <> dec))

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
        dup <> push 2 <> Monopig3.mod <>
        branch (dec <> get 0 <> dup <> get 1 <> mul <> put 1) (get 0) <>
        dup <> mul <> put 0 <>
        push 2 <> frac
      ) <>
      pop <> get 1

f &&& g = \r -> (f r, g r)


data Code = IF [Code] [Code]
          | REP [Code]
          | WHILE [Code] [Code]
          | PUT Int | GET Int
          | PUSH Int | POP | DUP | SWAP | EXCH
          | INC | DEC | NEG
          | ADD | MUL | SUB | DIV | MOD
          | EQL | LTH | GTH | NEQ
          deriving (Read, Show)

fromCode :: [Code] -> Program' a
fromCode = hom
    where
        hom = foldMap $ \case
            IF b1 b2 -> branch (hom b1) (hom b2)
            REP p -> rep (hom p)
            WHILE t b -> while (hom t) (hom b)
            PUT i -> put i
            GET i -> get i
            PUSH i -> push i
            POP -> pop
            DUP -> dup
            SWAP -> swap
            EXCH -> exch
            INC -> inc
            DEC -> dec
            ADD -> add
            MUL -> mul
            MOD -> Monopig3.mod
            SUB -> sub
            DIV -> frac
            EQL -> eq
            LTH -> lt
            GTH -> gt
            NEQ -> neq
            NEG -> neg

toCode :: Program' a -> [Code]
toCode prog = fst . getProgram $ prog none

listing :: Program' a -> String
listing = unlines . hom 0 . toCode
  where
    hom n = foldMap f
      where
        f = \case
          IF b1 b2 -> ouput "IF" <> indent b1 <> ouput ":" <> indent b2
          REP p -> ouput "REP" <> indent p
          WHILE t b -> ouput "WHILE" <> indent t <> indent b
          c -> ouput $ show c

        ouput x = [stimes n "  " ++ x]
        indent = hom (n+1)
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
