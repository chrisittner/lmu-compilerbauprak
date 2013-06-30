{-# LANGUAGE GeneralizedNewtypeDeriving, UndecidableInstances, TypeSynonymInstances, FlexibleInstances #-}

module Backend.Names(
  Temp, Label, mkLabel, mkNamedTemp,
  MonadNameGen, nextTemp, nextLabel, 
  NameGen, runNameGen,
  NameGenT, runNameGenT, labelName
  ) where

import Data.Monoid
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Maybe
import Control.Monad.Identity
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Writer.Strict
import Text.PrettyPrint
--import Util

data Temp = NamedTemp String | Temp Int 
   deriving (Eq, Ord)

instance Show Temp where
    show (Temp i) = "t" ++ show i
    show (NamedTemp s) = s

--instance Pretty Temp where
--   ppr t = text (show t)

-- May lead to name clashes with 'nextTemp'
-- User must take care to avoid clashes.
mkNamedTemp :: String -> Temp
mkNamedTemp s = NamedTemp s

type Label = String

--instance Pretty Label where
--   ppr l = text l
labelName = drop 2 

mkLabel :: String -> Label
mkLabel l | ('$' `elem` l) = 
              error $ "Label \"" ++ l ++ "\" contains reserver character '$'."
          | otherwise = 'L':l          
            
class Monad m => MonadNameGen m where
  nextTemp :: m Temp
  nextLabel :: m Label
  
newtype NameGenT m a = NameGenT (StateT ([Temp], [Label]) m a) 
  deriving (Monad, MonadTrans)

type NameGen a = NameGenT Identity a 

runNameGen :: NameGenT Identity a -> a
runNameGen = runIdentity . runNameGenT 

instance (Monad m) => MonadNameGen (NameGenT m) where
  nextTemp = NameGenT $ do (t:ts, ls) <- get; put (ts, ls); return t
  nextLabel = NameGenT $ do (ts, l:ls) <- get; put (ts, ls); return l
  
runNameGenT :: (Monad m) => NameGenT m a -> m a
runNameGenT (NameGenT x) = 
   evalStateT x ([Temp i | i<-[0..]], ["L$" ++ (show i) | i <- [(0::Int)..]])

instance MonadNameGen m => MonadNameGen (MaybeT m) where
  nextTemp = lift nextTemp
  nextLabel = lift nextLabel

instance (Error e, MonadNameGen m) => MonadNameGen (ErrorT e m) where
  nextTemp = lift nextTemp
  nextLabel = lift nextLabel

instance (MonadNameGen m) => MonadNameGen (ReaderT a m) where
  nextTemp = lift nextTemp
  nextLabel = lift nextLabel

instance (Monoid a, MonadNameGen m) => MonadNameGen (WriterT a m) where
  nextTemp = lift nextTemp
  nextLabel = lift nextLabel
