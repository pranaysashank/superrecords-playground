{-# language  OverloadedLabels
            , TypeOperators
            , DataKinds
            , TypeSynonymInstances
            , FlexibleInstances
            , MultiParamTypeClasses
            , KindSignatures
            , GADTs
            , StandaloneDeriving #-}

module Lib
    ( someFunc
    ) where

import Control.Applicative
import Data.Profunctor.Product
import Data.Profunctor.Product.Default
import Data.Profunctor
import SuperRecord

import GHC.TypeLits

newtype Person a b c d = Person (Record '["age" := a, "name" := b, "dob" := c, "address" := d])

pPersonApplicative :: Applicative f => Person (f a) (f b) (f c) (f d) -> f (Person a b c d)
pPersonApplicative (Person a) = (\x y z w -> Person (#age := x & #name := y & #dob := z & #address := w & rnil))
                      <$> get #age a
                      <*> get #name a
                      <*> get #dob a
                      <*> get #address a

pPerson :: ProductProfunctor p => Person (p a a') (p b b') (p c c') (p d d') -> p (Person a b c d) (Person a' b' c' d')
pPerson (Person a) = lmap (\(Person x) -> x) ((\x y z w -> Person (#age := x & #name := y & #dob := z & #address := w & rnil))
                                    ***$ lmap (get #age) (get #age a)
                                    **** lmap (get #name) (get #name a)
                                    **** lmap (get #dob) (get #dob a)
                                    **** lmap (get #address) (get #address a))


type R a b c d = Rec '["address" := a, "age" := b, "dob" := c, "name" := d]

pR :: ProductProfunctor p => R (p a a') (p b b') (p c c') (p d d') -> p (R a b c d) (R a' b' c' d')
pR a = (\x y z w -> (#age := x & #name := y & #dob := z & #address := w & rnil))
                                    ***$ lmap (get #age) (get #age a)
                                    **** lmap (get #name) (get #name a)
                                    **** lmap (get #dob) (get #dob a)
                                    **** lmap (get #address) (get #address a)

instance ( ProductProfunctor p
         , Default p a a'
         , Default p b b'
         , Default p c c'
         , Default p d d'
         ) =>
         Default p (R a b c d) (R a' b' c' d') where
    def = pR (#age := def & #name := def & #dob := def & #address := def & rnil)

g :: Applicative f => f (Rec '["name" := String])
g = pure (& rnil) <*> pure (#name := "Alex")

person :: Record '["age" := Int, "name" := String]
person = #name := "Alex"
         & #age := 23
         & rnil


data ABC (n :: Nat) where
  GHI :: ABC n

deriving instance Show (ABC n)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
