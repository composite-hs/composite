module Api (API, api, service) where

import ClassyPrelude
import Control.Arrow (returnA)
import Control.Lens (_Unwrapping, each, toListOf, view)
import Control.Monad.Logger (logInfo)
import Data.Proxy (Proxy(Proxy))
import Foundation (AppStackM, withDb)
import Frames (Record, rlens, rsubset)
import Opaleye ((./=), constant, desc, orderBy, queryTable, restrict, runQuery)
import Servant (ServerT, Get, JSON, (:>))
import Types (ApiUserJson(ApiUserJson), DbUser, UserType(UserTypeRegular), cLogin, cUserType, userTable)

type API = "users" :> Get '[JSON] [ApiUserJson]

api :: Proxy API
api = Proxy

service :: ServerT API AppStackM
service = do
  $logInfo "received request for users"

  users <- withDb $ \ conn ->
    runQuery conn . orderBy (desc $ view (rlens cLogin)) $ proc () -> do
      user@(view (rlens cUserType) -> userType) <- queryTable userTable -< ()
      restrict -< userType ./= constant UserTypeRegular
      returnA -< user

  let _ = users :: [Record DbUser]

  pure $ toListOf (each . rsubset . _Unwrapping ApiUserJson) users