{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, FlexibleInstances #-}
module Backend.MachineSpecifics (
   Fragment(..),
   Assem(..),
   Frame(..),
   MachineSpecifics(..),
   Location(..)
) where

import Data.Monoid
import Control.Monad.Trans
import Control.Monad.Error
import Control.Monad.Maybe
import Control.Monad.Trans.Reader (ReaderT)
import Backend.Tree
import Backend.Names

data Fragment f b = FragmentProc {frame :: f, body :: b} deriving (Show)

class (Show a) => Assem a where
  use  :: a -> [Temp]
  def  :: a -> [Temp]
  jumps :: a -> [Label]
  isFallThrough :: a -> Bool
  isMoveBetweenTemps :: a -> Maybe (Temp, Temp)
  isLabel :: a -> Maybe Label
  rename :: a -> (Temp -> Temp) -> a

data Location = Anywhere | InMemory

class (Show f) => Frame f where
  name :: f -> String
  params :: f -> [Exp] --
  size :: f -> Int
  allocLocal :: MonadNameGen m => f -> Location -> m (f, Exp) --
  makeProc :: MonadNameGen m => f -> Stm -> Exp -> m Stm --
  
class (Assem a, Frame f, MonadNameGen m) => MachineSpecifics m a f | m -> a f where
  -- Interface zum Frontend
  wordSize :: m Int
  mkFrame :: String -> Int -> m f --
  -- Codeerzeugung 
  codeGen :: Fragment f [Stm] -> m (Fragment f [a])
  -- Interface zum Registerallokator
  allRegisters :: m (Maybe [Temp])
  generalPurposeRegisters :: m (Maybe [Temp])
  spill :: f -> [a] -> [Temp] -> m (f, [a])  
  -- Interface zum Assembler
  printAssembly :: [Fragment f [a]] -> m String


-- ugly boiler plate

instance (MachineSpecifics m a f) => MachineSpecifics (ReaderT c m) a f where
  wordSize = lift wordSize
  mkFrame name len = lift $ mkFrame name len
  codeGen frag = lift $ codeGen frag
  allRegisters = lift allRegisters 
  generalPurposeRegisters = lift generalPurposeRegisters
  spill frame instrs temps = lift $ spill frame instrs temps
  printAssembly frag = lift $ printAssembly frag

instance (Error e, MachineSpecifics m a f) => MachineSpecifics (ErrorT e m) a f where
  wordSize = lift wordSize
  mkFrame name len = lift $ mkFrame name len
  codeGen frag = lift $ codeGen frag
  allRegisters = lift allRegisters 
  generalPurposeRegisters = lift generalPurposeRegisters
  spill frame instrs temps = lift $ spill frame instrs temps
  printAssembly frag = lift $ printAssembly frag
