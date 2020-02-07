{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving, TupleSections #-}

module Monopig5 where

import Data.Monoid hiding ((<>))
import Data.Semigroup (Semigroup(..),stimes,Max(..))
import Data.Vector ((//),(!),Vector,toList)
import qualified Data.Vector as V (replicate)
import Control.Monad
import Control.Monad.Identity
import Pig



type Stack = [Pig]
type Memory = Vector Pig



memSize = 4

data VM a = VM { stack :: Stack
               , status :: Maybe String
               , memory :: Memory
               , journal :: a }
            deriving Show

mkVM = VM mempty mempty (V.replicate memSize Null)

setStack  x (VM _ st m l) = return $ VM x st m l
setStatus x (VM s _ m  l) = return $ VM s x m l
setMemory m (VM s st _ l) = return $ VM s st m l
addRecord x (VM s st m l) = VM s st m (x<>l)

------------------------------------------------------------

data Code = IF [Code] [Code]
          | REP [Code]
          | WHILE [Code] [Code]
          | PUT Int | GET Int
          | PUSH Pig | POP | DUP | SWAP | EXCH
          | INC | DEC | NEG
          | CNC
          | ADD | MUL | SUB | DIV | MOD
          | EQL | LTH | GTH | NEQ
          | ASK | PRT | PRTS String
          | FORK [Code] [Code]
          deriving (Read, Show)

newtype ActionM m a = ActionM {runActionM :: a -> m a}

instance Monad m => Semigroup (ActionM m a) where
  ActionM f <> ActionM g = ActionM (f >=> g)

instance Monad m => Monoid (ActionM m a) where
  ActionM f `mappend` ActionM g = ActionM (f >=> g)
  mempty = ActionM return

newtype Program m a = Program { getProgram :: ([Code], ActionM m (VM a)) }
    deriving (Semigroup, Monoid)

type Program' m a = (Code -> VM a -> m (VM a)) -> Program m a

program c f p = Program . ([c],) . ActionM $
    \vm -> case status vm of
            Nothing -> p c =<< f (stack vm) vm
            m       -> return vm

programM c f p = Program . ([c],) . ActionM $
    \vm -> case status vm of
            Nothing -> p c =<< f (memory vm, stack vm) vm
            m       -> return vm

run :: Monad m => Program m a -> VM a -> m (VM a)
run = runActionM . snd . getProgram

toCode :: Monad m => Program' m a -> [Code]
toCode prog = fst . getProgram $ prog none

none :: Monad m => Code -> VM a -> m (VM a)
none = const return

exec :: Program' Identity () -> VM ()
exec = runIdentity . execM

execM :: Monad m => Program' m () -> m (VM ())
execM prog = run (prog none) (mkVM ())

execLog p prog = run (prog $ \c -> \vm -> return $ addRecord (p c vm) vm) (mkVM mempty)

f &&& g = \c -> \r -> (f c r, g c r)

logStack _ vm   = [stack vm]
logStackUsed _ = Max . length . stack
logSteps _     = const (Sum 1)
logCode c _   = [c]
logRun com vm = [pad 10 c ++ "| " ++ pad 20 s ++ "| " ++ m]
  where c = show com
        m = unwords $ show <$> toList (memory vm)
        s = unwords $ show <$> stack vm
        pad n x = take n (x ++ repeat ' ')

debug p = unlines . reverse . journal <$> execLog logRun p


pop,dup,swap,exch :: Monad m => Program' m a
put,get:: Monad m => Int -> Program' m a
push :: (Monad m, Piggable b) => b -> Program' m a
add,mul,sub,frac,modulo,inc,dec,neg, cnc :: Monad m => Program' m a
eq,neq,lt,gt :: Monad m => Program' m a

err m = setStatus . Just $ "Error : " ++ m

pop = program POP $
    \case x:s -> setStack s
          _   -> err "pop expected an argument."

push x = program (PUSH y) (\s -> setStack (y:s))
    where
        y = case piggify x of
            PInt a    -> PInt (a :: Int)
            PString s -> PString s
            PBool b   -> PBool b
            PObject o -> PObject o
            _         -> Null

dup = program DUP $
  \case x:s -> setStack (x:x:s)
        _   -> err "dup expected an argument."

swap = program SWAP $
  \case x:y:s -> setStack (y:x:s)
        _     -> err "swap expected two arguments."

exch = program EXCH $
  \case x:y:s -> setStack (y:x:y:s)
        _     -> err "expected two arguments."

put i = indexed (PUT i) i $
    \case (m, x:s) -> setStack s <=< setMemory (m // [(i,x)])
          _        -> err "put expected an argument"

get i = indexed (GET i) i $ \(m, s) -> setStack ((m ! i) : s)

indexed c i f = programM c $ if (i < 0 || i >= memSize)
                             then const $ err "index in [0,16]"
                             else f

app1Int c f = program c $
  \case (PInt x:xs) -> setStack (PInt (f x) : xs)
        -- (PInt x):s -> setStack (PInt (f x) : s)
        -- (PString _):s -> err $ "PInt expected, PString found"
        -- (PBool _):s -> err $ "PInt expected, PBool found"
        -- (PObject _):s -> err $ "PInt expected, PObject found"
        (x:xs) -> err $ "PInt expected, " ++ kindOf x ++ " found"
        _ -> err $ "operation " ++ show c ++ " expected an argument"

app2Int :: Monad m => Code -> (Int -> Int -> Int) -> (Code -> VM a -> m (VM a)) -> Program m a
app2Int c f = program c $
  \case PInt x:PInt y:s -> setStack (PInt (f x y) : s)
                -- if kindOf x == "PInt" && kindOf y == "PInt"
                --  then setStack (f x y : s)
                --  else err $ "PInt expected, " ++ head (filter (/= "PInt") [kindOf x, kindOf y]) ++ " found"
        (x:y:s) -> err $ "PInt and PInt expected, " ++ kindOf x ++ ", " ++ kindOf y ++ " found"
        _ -> err $ "operation " ++ show c ++ " expected two arguments"

app2 :: Monad m => Code -> (Pig -> Pig -> Pig) -> (Code -> VM a -> m (VM a)) -> Program m a
app2 c f = program c $
  \case (x:y:s) -> setStack (f x y : s)
        _       -> err $ "operation " ++ show c ++ " expected two arguments"
-- cnc = undefined
cnc = app2 CNC f
    where f :: Pig -> Pig -> Pig
          f x y = PString $! (show y ++ show x)
add = app2Int ADD (+)
sub = app2Int SUB (flip (-))
mul = app2Int MUL (*)
frac = app2Int DIV (flip div)
modulo = app2Int MOD (flip mod)
neg = app1Int NEG (\x -> -x)
inc = app1Int INC (+1)
dec = app1Int DEC (\x -> x-1)
eq = app2Int EQL (\x -> \y -> if (x == y) then 1 else 0)
neq = app2Int NEQ (\x -> \y -> if (x /= y) then 1 else 0)
lt = app2Int LTH (\x -> \y -> if (x > y) then 1 else 0)
gt = app2Int GTH (\x -> \y -> if (x < y) then 1 else 0)

proceed p prog s = run (prog p) <=< setStack s

rep body p = program (REP (toCode body)) go none
  where go (PInt n:s) = if n >= 0
                   then proceed p (stimes n body) s
                   else err "rep expected positive argument."
        go (y:s) = err $ "PInt expected, " ++ kindOf y ++ " found"
        go _ = err "rep expected an argument."

branch br1 br2 p = program (IF (toCode br1) (toCode br2)) go none
   where go (PInt x:s) = proceed p (if x /= 0 then br1 else br2) s
         go (y:s) = err $ "PInt expected, " ++ kindOf y ++ " found"
         go _ = err "branch expected an argument."


while test body p = program (WHILE (toCode test) (toCode body)) (const go) none
  where go vm = do res <- proceed p test (stack vm) vm
                   case stack res of
                     PInt 0:s -> proceed p mempty s res
                     _:s -> go =<< proceed p body s res
                     _ -> err "while expected an argument." vm

ask :: Program' IO a
ask = program ASK $!
  \case s -> \vm -> do x <- getLine
                       setStack (read x:s) vm

prt :: Program' IO a
prt = program PRT $!
  \case x:s -> \vm -> print x >> return vm
        _ -> err "PRT expected an argument"



prtS :: String -> Program' IO a
prtS s = program (PRTS s) $
  const $ \vm -> print s >> return vm
{-
 ask, prt :: Program' IO a
ask = program ASK $
  \case s -> \vm -> do x <- getLine
                       setStack (read x:s) vm

prt = program PRT $
  \case x:s -> \vm -> print x >> return vm
        _ -> err "PRT expected an argument"

prtS :: String -> Program' IO a
prtS s = program (PRTS s) $
  const $ \vm -> print s >> return vm
 - -}

fork :: Program' [] a -> Program' [] a -> Program' [] a
fork br1 br2 p = program (FORK (toCode br1) (toCode br2)) (const go) none
  where go = run (br1 p) <> run (br2 p)

fromCode :: Monad m => [Code] -> Program' m a
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
      SUB -> sub
      DIV -> frac
      MOD -> modulo
      EQL -> eq
      LTH -> lt
      GTH -> gt
      NEQ -> neq
      NEG -> neg
      CNC -> cnc
      _ -> mempty

fromCodeIO :: [Code] -> Program' IO a
fromCodeIO = hom
  where
    hom = foldMap $ \case
      IF b1 b2 -> branch (hom b1) (hom b2)
      REP p -> rep (hom p)
      WHILE t b -> while (hom t) (hom b)
      ASK -> ask
      PRT -> prt
      PRTS s -> prtS s
      c -> fromCode [c]

fromCodeList :: [Code] -> Program' [] a
fromCodeList = hom
  where
    hom = foldMap $ \case
      IF b1 b2 -> branch (hom b1) (hom b2)
      REP p -> rep (hom p)
      WHILE t b -> while (hom t) (hom b)
      FORK b1 b2 -> fork (hom b1) (hom b2)
      c -> fromCode [c]

listing :: Program' IO a -> String
listing = unlines . hom 0 . toCode
  where
    hom n = foldMap f
      where
        f = \case
          IF b1 b2 -> ouput "IF" <> indent b1 <> ouput ":" <> indent b2
          REP p -> ouput "REP" <> indent p
          WHILE t b -> ouput "WHILE" <> indent t <> indent b
          c -> ouput $ show c

        ouput x = [stimes n "    " ++ x]
        indent = hom (n+1)

ioprog = prtS "input first number" <> ask <>
         prtS "input second number" <> ask <>
         rep (prt <> dup <> inc) <>
         prt
         -- []
