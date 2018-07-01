{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Rank2Model where

import Data.Store
import GHC.Generics
import HLRDB
import Data.Time
import qualified Rank2
import qualified Rank2.TH
import Data.Constraint
import Data.Functor.Const
import Data.String (IsString(..))
import Data.Profunctor.Traversing


type Length = Double
type Mass = Double

newtype UserId = UserId Identifier deriving (Generic,Eq,Ord,Show,IsIdentifier,Store)


data User f = User {
    weight :: f Mass
  , height :: f Length
  , firstName :: f String
  , lastName :: f String
  , birthday :: f UTCTime
  } deriving (Generic)

$(Rank2.TH.deriveAll ''User)


userStorageMeta :: User ((Dict :.: Store) :*: Const String)
userStorageMeta = User {
    weight = Comp1 Dict :*: Const "weight"
  , height = Comp1 Dict :*: Const "height"
  , firstName = Comp1 Dict :*: Const "firstName"
  , lastName = Comp1 Dict :*: Const "lastName"
  , birthday = Comp1 Dict :*: Const "birthday"
  }

newtype RBasic a = RBasic { runRBasic :: RedisBasic UserId (Maybe a) }

userStorage :: User RBasic
userStorage = Rank2.fmap f userStorageMeta
  where
    f :: ((Dict :.: Store) :*: Const String) a -> RBasic a
    f ((Comp1 Dict) :*: Const name) =
      RBasic . declareBasic . fromString $ "userInfo" ++ name

-- public API

setUser :: UserId -> User Maybe -> Redis ()
setUser i u = const () <$> Rank2.traverse f ((Rank2.Arrow . (:*:)) Rank2.<$> u Rank2.<*> userStorage)
  where
    f :: (Maybe :*: RBasic) a -> Redis (Const () a)
    f (Nothing :*: _) = pure $ Const ()
    f (Just x :*: RBasic s) = do
      set' s i x
      pure $ Const ()

userQuery :: UserId ~~> User Maybe
userQuery = Rank2.traverse f userStorage
  where
    f :: RBasic a -> UserId ~~> Maybe a
    f (RBasic x) = liftq x

getUsers :: [ UserId ] -> Redis [ User Maybe ]
getUsers = mget $ traverse' userQuery

