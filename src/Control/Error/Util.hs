{-# LANGUAGE FlexibleContexts #-}

module Control.Error.Util (
    module Control.Error.Util,
    module X,
) where

import           Control.Monad              (unless)
import           Control.Monad.Trans.Except as X
import qualified Data.Maybe                 as Maybe
import           Prelude


--------------------------------------------------------------------------------
-- Either operators using `Left` to raise error
--------------------------------------------------------------------------------

infixl 4 <?>
(<?>) :: Maybe b -> a -> Either a b
val <?> m = Maybe.maybe (Left m) Right val

infixl 4 <?&>
(<?&>) :: Either a (Maybe b) -> a -> Either a b
val <?&> m = Maybe.maybe (Left m) Right =<< val

--------------------------------------------------------------------------------
-- ExceptT operators using `throwE` to raise error
--------------------------------------------------------------------------------

infixl 4 <??>
(<??>) :: Monad m => Maybe b -> a -> ExceptT a m b
val <??> m = Maybe.maybe (throwE m) return val

infixl 4 <??&>
(<??&>) :: Monad m => ExceptT a m (Maybe b) -> a -> ExceptT a m b
val <??&> m = Maybe.maybe (throwE m) return =<< val

--------------------------------------------------------------------------------
-- Simple monadic operators using `fail` to raise error
--------------------------------------------------------------------------------

infixl 4 <?.>
(<?.>) :: Monad m => Maybe b -> String -> m b
val <?.> m = Maybe.maybe (fail m) return val

infixl 4 <?&.>
(<?&.>) :: Monad m => m (Maybe b) -> String -> m b
val <?&.> m = Maybe.maybe (fail m) return =<< val

--------------------------------------------------------------------------------
-- Assertions
--------------------------------------------------------------------------------

-- | assert in `Either` monad, using `Left` to raise error
assert :: Bool -> a -> Either a ()
assert condition msg = unless condition $ Left msg

-- | assert in `ExceptT` monad, using `throwE` to raise error
assertE :: Monad m => Bool -> a -> ExceptT a m ()
assertE condition msg = unless condition $ throwE msg

-- | assert in any monad, using `fail` to raise error
assertM :: Monad m => Bool -> String -> m ()
assertM condition msg = unless condition $ fail msg
