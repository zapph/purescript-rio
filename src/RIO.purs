module RIO
       ( RIO(..)
       , ParRIO(..)
       , rio
       , runRIO
       ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader (class MonadAsk, class MonadReader, runReaderT)
import Control.Monad.Reader.Trans (ReaderT(..))
import Control.Parallel (class Parallel, parallel, sequential)
import Effect.Aff (Aff, ParAff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error)

newtype RIO env a = RIO (ReaderT env Aff a)

derive newtype instance rioFunctor :: Functor (RIO env)
derive newtype instance rioApply :: Apply (RIO env)
derive newtype instance rioApplicative :: Applicative (RIO env)
derive newtype instance rioBind :: Bind (RIO env)
derive newtype instance rioMonad :: Monad (RIO env)
derive newtype instance rioMonadEffect :: MonadEffect (RIO env)
derive newtype instance rioMonadAff :: MonadAff (RIO env)
derive newtype instance rioMonadThrow :: MonadThrow Error (RIO env)
derive newtype instance rioMonadError :: MonadError Error (RIO env)
derive newtype instance rioMonadAsk :: MonadAsk env (RIO env)
derive newtype instance rioMonadReader :: MonadReader env (RIO env)

rio :: forall env a. (env -> Aff a) -> RIO env a
rio = RIO <<< ReaderT

runRIO :: forall env a. RIO env a -> env -> Aff a
runRIO (RIO f) = runReaderT f

newtype ParRIO env a = ParRIO (ReaderT env ParAff a)

derive newtype instance parRioFunctor :: Functor (ParRIO env)
derive newtype instance parRioApply :: Apply (ParRIO env)
derive newtype instance parRioApplicative :: Applicative (ParRIO env)

instance parallelRIO :: Parallel (ParRIO env) (RIO env) where
  parallel (RIO f) = ParRIO $ parallel f
  sequential (ParRIO f) = RIO $ sequential f
