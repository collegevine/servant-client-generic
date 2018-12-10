{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Servant.Client.Generic (
    HasClientEndpoints(..),
    WithClientEndpoints,
    Delete,
    Get,
    Post,
    Put,
    Endpoint(..),
    Part(..),
    TypeInfo(..)
) where

import Data.Monoid ((<>))
import Data.Proxy (Proxy)
import Data.Typeable
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Network.HTTP.Types (Method, methodGet, methodPut, methodPost, methodDelete)
import Servant hiding (Delete, Get, Post, Put)
import qualified Servant as S

--
--
--

class HasClientEndpoints a where
    endpoints :: Proxy a -> [Endpoint]

--
--
--

type family Contains (t :: *) (a :: [*]) where
    Contains t '[] = 'False
    Contains t (t ': ax) = 'True
    Contains t (a ': ax) = Contains t ax

--
--
--

class IsHttpMethod a where
    toHttpMethod :: Proxy a -> Method

instance IsHttpMethod 'GET where
    toHttpMethod = const methodGet

instance IsHttpMethod 'PUT where
    toHttpMethod = const methodPut

instance IsHttpMethod 'POST where
    toHttpMethod = const methodPost

instance IsHttpMethod 'DELETE where
    toHttpMethod = const methodDelete

--
--
--

data WithClientEndpoints (name :: Symbol) a

type Delete name types result = WithClientEndpoints name (S.Delete types result)
type Get name types result = WithClientEndpoints name (S.Get types result)
type Post name types result = WithClientEndpoints name (S.Post types result)
type Put name types result = WithClientEndpoints name (S.Put types result)

--
--
--

data Endpoint = Endpoint {
    eName :: String,
    eParts :: [Part],
    eVerb :: Method,
    eResult :: TypeInfo
} deriving Show

data Part
    = PConstant String
    | PCapture String TypeInfo
    | PCaptureAll String TypeInfo
    | PQueryParam String TypeInfo
    | PRequestBody TypeInfo
    | PHeader String TypeInfo
    deriving Show

data TypeInfo = TypeInfo {
    tiName :: String,
    tiModule :: String,
    tiParameters :: [TypeInfo]
} deriving Show

--
--
--

-- Verb
instance (
    HasServer (Verb method status ctypes a) ctx) =>
    HasServer (WithClientEndpoints name (Verb method status ctypes a)) ctx where
    type ServerT (WithClientEndpoints name (Verb method status ctypes a)) m = ServerT (Verb method status ctypes a) m
    route Proxy ctx d = route (Proxy :: Proxy (Verb method status ctypes a)) ctx d

--
--
--

-- :<|>
instance (
    HasClientEndpoints a,
    HasClientEndpoints b) => HasClientEndpoints (a :<|> b) where
    endpoints Proxy = endpoints (Proxy :: Proxy a) <> endpoints (Proxy :: Proxy b)

-- Path
instance (
    KnownSymbol path,
    HasClientEndpoints api) => HasClientEndpoints (path :> api) where
    endpoints Proxy = prepend $ endpoints (Proxy :: Proxy api)
        where
        prepend = fmap (prependPart part)
        part = PConstant (symbolVal (Proxy :: Proxy path))

-- Capture
instance (
    KnownSymbol cap,
    Typeable a,
    HasClientEndpoints api) => HasClientEndpoints (Capture cap a :> api) where
    endpoints Proxy = prepend $ endpoints (Proxy :: Proxy api)
        where
        prepend = fmap (prependPart part)
        part = PCapture (symbolVal (Proxy :: Proxy cap)) inf
        inf = toTypeInfo (Proxy :: Proxy a)

-- CaptureAll
instance (
    KnownSymbol cap,
    Typeable [a],
    HasClientEndpoints api) => HasClientEndpoints (CaptureAll cap a :> api) where
    endpoints Proxy = prepend $ endpoints (Proxy :: Proxy api) 
        where
        prepend = fmap (prependPart part)
        part = PCaptureAll (symbolVal (Proxy :: Proxy cap)) inf
        inf = toTypeInfo (Proxy :: Proxy [a])

-- Request body
instance (
    Typeable a,
    HasClientEndpoints api) => HasClientEndpoints (ReqBody list (a :: *) :> api) where
    endpoints Proxy = prepend $ endpoints (Proxy :: Proxy api)
        where
        prepend = fmap (prependPart part)
        part = PRequestBody inf
        inf = toTypeInfo (Proxy :: Proxy a)

-- Query param
instance (
    KnownSymbol sym,
    Typeable a,
    HasClientEndpoints api) => HasClientEndpoints (QueryParam sym (a :: *) :> api) where
    endpoints Proxy = prepend $ endpoints (Proxy :: Proxy api)
        where
        prepend = fmap (prependPart part)
        part = PQueryParam (symbolVal (Proxy :: Proxy sym)) inf
        inf = toTypeInfo (Proxy :: Proxy (Maybe a))

-- Header
instance (
    KnownSymbol sym,
    Typeable a,
    HasClientEndpoints api) => HasClientEndpoints (Header sym a :> api) where
    endpoints Proxy = prepend $ endpoints (Proxy :: Proxy api)
        where
        name = symbolVal (Proxy :: Proxy sym)
        prepend = fmap (prependPart part)
        part = PHeader name inf
        inf = toTypeInfo (Proxy :: Proxy a)

-- Verb
instance HasClientEndpoints (Verb method status ctypes a) where
    endpoints Proxy = []

instance (
    Typeable a,
    KnownSymbol name,
    IsHttpMethod method,
    Contains JSON ctypes ~ 'True) => HasClientEndpoints (WithClientEndpoints name (Verb method status ctypes a)) where
    endpoints Proxy = [endpoint]
        where
        endpoint = Endpoint {
            eName = symbolVal (Proxy :: Proxy name),
            eParts = [],
            eVerb = toHttpMethod (Proxy :: Proxy method),
            eResult = toTypeInfo (Proxy :: Proxy a)
        }

--
--
--

prependPart :: Part -> Endpoint -> Endpoint
prependPart part e = e { eParts = part : eParts e }

toTypeInfo :: Typeable a => Proxy a -> TypeInfo
toTypeInfo = toTypeInfo' . typeRep
    where
    toTypeInfo' rep =
        let con = typeRepTyCon rep in
        TypeInfo {
            tiModule = tyConModule con,
            tiName = tyConName con,
            tiParameters = toTypeInfo' <$> typeRepArgs rep
        }
