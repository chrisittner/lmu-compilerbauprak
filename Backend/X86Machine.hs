{-# LANGUAGE EmptyDataDecls,MultiParamTypeClasses, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module Backend.X86Machine where
import Backend.Names
import Backend.X86Assem
import qualified Backend.Tree as B
import Backend.InstructionSelection
import Backend.MachineSpecifics
import Control.Monad.Trans.Writer.Strict
import Control.Monad
import Control.Monad.Trans.Identity
import Control.Monad.Trans
import Backend.DummyMachine
import Backend.Names

data X86Frame = X86Frame {fname :: String, fparams :: [Temp], locals :: [Temp], returnTemp :: Temp} deriving Show

instance Frame X86Frame where
  name f = fname f
  params f = [ TEMP t | t <- fparams f ]
  size f = (length (params f)) + (length (locals f))
  allocLocal f Anywhere = do 
        t <- nextTemp 
        return (X86Frame (fname f) (fparams f) (t:(locals f)) (returnTemp f),
                TEMP t)
  allocLocal f InMemory = error "dummy machine has no memory model"
  makeProc f body returnExp = return $ SEQ body $ MOVE (TEMP (returnTemp f)) returnExp

class (Show f) => Frame f where
  name :: f -> String
  params :: f -> [Exp] --
  size :: f -> Int
  allocLocal :: MonadNameGen m => f -> Location -> m (f, Exp) --
  makeProc :: MonadNameGen m => f -> Stm -> Exp -> m Stm --




newtype X86MachineT m a = X86MachineT { runX86MachineT :: NameGenT m a }
   deriving (Monad, MonadNameGen, MonadTrans)

withX86Machine :: Monad m => X86MachineT m a -> m a
withX86Machine = runNameGenT . runX86MachineT

instance (Monad m) => MachineSpecifics (X86MachineT m) X86Assem X86Frame where
  wordSize = return 4
  mkFrame name nparams =
    do paramTemps <- replicateM nparams nextTemp 
       returnTemp <- nextTemp
       return $ DummyFrame name paramTemps [] returnTemp
  codeGen (FragmentProc f b) = do 
  	assemlist <- execWriterT $ munchStm (B.sseq b)
  	return $ FragmentProc f assemlist
  allRegisters = return Nothing
  generalPurposeRegisters = return Nothing

  spill frame body temps = return (frame, []) -- todo
  printAssembly frags = return "" -- todo

