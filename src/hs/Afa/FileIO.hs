{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

module Afa.FileIO where

import Data.Array
import qualified Capnp
import qualified Capnp.GenHelpers.ReExports.Data.Vector as V
import Capnp.Gen.Schema.Afa.Pure
import System.IO
import qualified Afa
import qualified Afa.TreeDag.Patterns.Builder as T

hReadAfa :: Handle -> IO Afa.Afa
hReadAfa h = deserializeAfa <$> Capnp.hGetValue h Capnp.defaultLimit

hWriteAfa :: Afa.Afa -> Handle -> IO ()
hWriteAfa afa h = Capnp.hPutValue h$ serializeAfa afa

deserializeAfa :: Afa -> Afa.Afa
deserializeAfa Afa{variableCount, terms, states} = Afa.Afa
  { Afa.varCount = fromIntegral variableCount
  , Afa.terms = listArray (0, length terms - 1)$ map deserializeTerm$ V.toList terms
  , Afa.states = listArray (0, length states - 1)$ map fromIntegral$ V.toList states
  }

deserializeTerm :: Term -> T.Term
deserializeTerm Term'litTrue = T.LTrue
deserializeTerm Term'litFalse = T.LFalse
deserializeTerm (Term'var i) = T.Var$ fromIntegral i
deserializeTerm (Term'state i) = T.State$ fromIntegral i
deserializeTerm (Term'ref i) = T.Ref$ fromIntegral i
deserializeTerm (Term'and xs) = T.And$ map deserializeTerm$ V.toList xs
deserializeTerm (Term'or xs) = T.Or$ map deserializeTerm$ V.toList xs
deserializeTerm (Term'not x) = T.Not$ deserializeTerm x

serializeAfa :: Afa.Afa -> Afa
serializeAfa (Afa.Afa varCount terms states) = Afa
  { variableCount = fromIntegral varCount
  , terms = V.fromList$ map serializeTerm$ elems terms
  , states = V.fromList$ map fromIntegral$ elems states
  }

serializeTerm :: T.Term -> Term
serializeTerm T.LTrue = Term'litTrue
serializeTerm T.LFalse = Term'litFalse
serializeTerm (T.Var i) = Term'var$ fromIntegral i
serializeTerm (T.State i) = Term'state$ fromIntegral i
serializeTerm (T.Ref i) = Term'ref$ fromIntegral i
serializeTerm (T.And xs) = Term'and$ V.fromList$ map serializeTerm xs
serializeTerm (T.Or xs) = Term'or$ V.fromList$ map serializeTerm xs
serializeTerm (T.Not x) = Term'not$ serializeTerm x
