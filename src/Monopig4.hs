{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving, TupleSections #-}


import Data.Monoid hiding ((<>))
import Data.Semigroup (Semigroup(..),stimes,Max(..))
import Data.Vector ((//),(!),Vector,toList)
import qualified Data.Vector as V (replicate)
import Control.Monad
import Control.Monad.Identity

type Stack = [Int]
type Memory = Vector Int

memSize = 4

data VM a = VM { stack :: Stack
               , status :: Maybe String
               , memory :: Memory
               , journal :: a
               }
               
mkVM = VM mempty mempty (V.replicate memSize 0)



setStack  x (VM _ st m l) = return $ VM x st m l
setStatus st (VM s _ m l) = return $ VM s st m l
setMemory m (VM s st _ l) = return $ VM s st m l
addRecord x (VM s st m l) = VM s st m (x<>l)



data Code = IF [Code] [Code]
          | REP [Code]
          | WHILE [Code] [Code]
          | PUT Int | GET Int
          | PUSH Int | POP | DUP | SWAP | EXCH
          | INC | DEC | NEG
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
    m -> return vm

programM c f p = Program . ([c],) . ActionM $
  \vm -> case status vm of
    Nothing -> p c =<< f (memory vm, stack vm) vm
    m -> return vm

