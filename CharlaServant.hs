{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CharlaServant where

import GHC.Generics
import Data.Aeson
import Data.Text
import Servant
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Network.Wai
import Network.Wai.Handler.Warp
import Lucid as L
import Servant.HTML.Lucid
-- import Text.Hamlet
-- import Text.Blaze
-- import Servant.HTML.Blaze
import Control.Monad

import Database.Persist
import Database.Persist.TH

mkPersist sqlSettings [persistLowerCase|
Usuario json
  nombre    String
  apellidos String
  NombreCompleto nombre apellidos
  deriving Show
Contador json
  nombre  String
  valor   Int
  usuario UsuarioId
  deriving Show
|]

type CounterId = Integer
data Counter = Counter { counterId    :: CounterId
                       , counterName  :: Text
                       , counterValue :: Integer }
             deriving (Eq, Show, Generic)
instance FromJSON Counter
instance ToJSON Counter

data Order = Asc | Desc deriving (Eq, Show, Generic)
instance FromJSON Order
instance ToJSON Order
instance FromText Order

type NewAPI     = "new"    :> ReqBody '[PlainText] Text  :> Put   '[JSON] CounterId
type StepAPI    = "step"   :> Capture "id" CounterId     :> Post  '[JSON] Integer
type ValueAPI   = "value"  :> Capture "id" CounterId     :> Get   '[JSON] Integer
type ListAPI    = "list"   :> QueryParam "order" Order   :> Get   '[JSON, HTML] [Counter]
type AppAPI     = NewAPI :<|> StepAPI :<|> ValueAPI :<|> ListAPI

type Estado = (TVar CounterId, TVar [Counter])

instance ToHtml [Counter] where
  toHtml cs = L.ul_ $ forM_ cs $ \(Counter _ nm v) -> do
                L.li_ $ do L.strong_  (L.toHtml nm)
                           L.toHtml ":"
                           L.toHtml (show v)

{-
instance ToMarkup [Counter] where
  toMarkup cs = [shamlet|
<ul>
  $forall c <- cs
    <li>
      <strong>#{counterName c}
      : #{counterValue c} |]
-}

servirNewAPI :: Estado -> Server NewAPI
servirNewAPI (ultimoId, lista) nombre = liftIO $ do
  print <$> readTVarIO ultimoId
  atomically $ do
    modifyTVar ultimoId (+1)
    nuevoId <- readTVar ultimoId
    modifyTVar lista (Counter nuevoId nombre 0 :)
    return nuevoId

servirListAPI :: Estado -> Server ListAPI
servirListAPI (_, lista) _ = liftIO $ readTVarIO $ lista

servidor :: Estado -> Application
servidor s = serve (Proxy :: Proxy (NewAPI :<|> ListAPI))
                   (servirNewAPI s :<|> servirListAPI s)

main :: IO ()
main = do
  ultimoId <- newTVarIO 0
  lista <- newTVarIO []
  run 8081 (servidor (ultimoId, lista))
