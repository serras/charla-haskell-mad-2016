{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE TypeFamilies, TypeOperators #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Charla where

import Data.Aeson
import GHC.Generics

data Numero where
  Cero  ::            Numero
  Suc   :: Numero ->  Numero
  deriving Show

data Arbol a where
  Hoja  :: a                        -> Arbol a
  Nodo  :: Arbol a -> a -> Arbol a  -> Arbol a
  deriving Show

data EstadoValidacion = NoValidada | Validada

data CadenaHTML (v :: EstadoValidacion) where
  NoV  :: String -> CadenaHTML NoValidada
  V    :: String -> CadenaHTML Validada

infixr 8 :/:
data Lista (n :: Numero) e where
  Fin    :: Lista Cero e
  (:/:)  :: e -> Lista n e -> Lista (Suc n) e

primero :: Lista (Suc n) e -> e
-- primero Fin = ??
primero (e :/: _) = e

type family Mas n m where
  Cero  `Mas` m = m
  Suc n `Mas` m = Suc (n `Mas` m)

unir :: Lista n e -> Lista m e -> Lista (n `Mas` m) e
unir Fin        lst = lst
unir (e :/: r)  lst = e :/: unir r lst

class Coleccion c where
  type Elemento c
  vacio :: c
  (//)  :: Elemento c -> c -> c

instance Coleccion [e] where
  type Elemento [e] = e
  vacio = []
  (//)  = (:)

data Persona = Persona { nombre :: String
                       , apellidos :: String
                       , edad :: Integer }
             deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- instance ToJSON Persona
-- instance FromJSON Persona
