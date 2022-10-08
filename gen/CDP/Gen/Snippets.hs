{-# LANGUAGE OverloadedStrings #-}
module CDP.Gen.Snippets where

import qualified Data.Text as T

domainLanguageExtensions :: T.Text
domainLanguageExtensions = T.unlines
    [ "{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}"
    , "{-# LANGUAGE ScopedTypeVariables #-}"
    , "{-# LANGUAGE FlexibleContexts #-}"
    , "{-# LANGUAGE MultiParamTypeClasses #-}"
    , "{-# LANGUAGE FlexibleInstances #-}"
    , "{-# LANGUAGE DeriveGeneric #-}"
    ]

domainImports :: T.Text
domainImports = T.unlines
    [ "import           Control.Applicative  ((<$>))"
    , "import           Control.Monad"
    , "import           Control.Monad.Loops"
    , "import           Control.Monad.Trans  (liftIO)"
    , "import qualified Data.Map             as M"
    , "import           Data.Maybe          "
    , "import Data.Functor.Identity"
    , "import Data.String"
    , "import qualified Data.Text as T"
    , "import qualified Data.List as List"
    , "import qualified Data.Text.IO         as TI"
    , "import qualified Data.Vector          as V"
    , "import Data.Aeson.Types (Parser(..))"
    , "import           Data.Aeson           (FromJSON (..), ToJSON (..), (.:), (.:?), (.=), (.!=), (.:!))"
    , "import qualified Data.Aeson           as A"
    , "import qualified Network.HTTP.Simple as Http"
    , "import qualified Network.URI          as Uri"
    , "import qualified Network.WebSockets as WS"
    , "import Control.Concurrent"
    , "import qualified Text.Casing as C"
    , "import qualified Data.ByteString.Lazy as BS"
    , "import qualified Data.Map as Map"
    , "import Data.Proxy"
    , "import System.Random"
    , "import GHC.Generics"
    , "import Data.Char"
    , "import Data.Default"
    , ""
    , "import CDP.Internal.Runtime"
    , "import CDP.Handle"
    ]
