{-# LANGUAGE EmptyDataDecls,MultiParamTypeClasses, GeneralizedNewtypeDeriving  #-}
module Backend.DummyMachine where

import Control.Monad
import Control.Monad.Trans.Identity
import Control.Monad.Trans
import Backend.Names
import Backend.Tree
import Backend.MachineSpecifics

data DummyAssem 
data DummyFrame = DummyFrame {fname :: String, fparams :: [Temp], locals :: [Temp], returnTemp :: Temp}
   deriving Show

-- Der Typ DummyAssem is leer, also sind folgende Definitionen sinnvoll:
instance Assem DummyAssem where
  use i = error "unreachable" 
  def i = error "unreachable"
  jumps i = error "unreachable"
  isFallThrough i = error "unreachable"
  isMoveBetweenTemps i = error "unreachable"
  isLabel i = error "unreachable"
  rename i = error "unreachable"

instance Show DummyAssem 

instance Frame DummyFrame where
  name f = fname f
  params f = [ TEMP t | t <- fparams f ]
  size f = (length (params f)) + (length (locals f))
  allocLocal f Anywhere = 
     do t <- nextTemp 
        return (DummyFrame (fname f) (fparams f) (t:(locals f)) (returnTemp f),
                TEMP t)
  allocLocal f InMemory = error "dummy machine has no memory model"
  makeProc f body returnExp =
     return $ SEQ body $ MOVE (TEMP (returnTemp f)) returnExp

newtype DummyMachineT m a = DummyMachineT { runDummyMachineT :: NameGenT m a }
   deriving (Monad, MonadNameGen, MonadTrans)

withDummyMachine :: Monad m => DummyMachineT m a -> m a
withDummyMachine = runNameGenT . runDummyMachineT

instance (Monad m) => MachineSpecifics (DummyMachineT m) DummyAssem DummyFrame where
  wordSize = return 4
  mkFrame name nparams =
    do paramTemps <- replicateM nparams nextTemp 
       returnTemp <- nextTemp
       return $ DummyFrame name paramTemps [] returnTemp
  codeGen (FragmentProc f b) = return (FragmentProc f []) 
  allRegisters = return [] -- return Nothing
  generalPurposeRegisters = return [] -- return Nothing
  -- Der Typ Assem is leer, also sind folgende Definitionen sinnvoll:
  spill frame body temps = return (frame, [])
  printAssembly frags = return ""
