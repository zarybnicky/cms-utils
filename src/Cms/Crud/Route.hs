{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

module Cms.Crud.Route where

import ClassyPrelude.Yesod hiding (get, (<>))
import Cms.Class
import Cms.ActionLog.Class (CmsActionLog)
import Cms.Roles.Class (CmsRoles)
import Control.Monad.Trans.State (StateT, evalStateT, get, put)

type CrudForm app a = Html -> MForm (HandlerT app IO) (FormResult a, WidgetT app IO ())

type PersistCrudEntity app a m =
  ( PathPiece (Key a)
  , Yesod app
  , YesodPersist app
  , YesodPersistBackend app ~ SqlBackend
  , PersistRecordBackend a SqlBackend
  , RenderMessage app m
  , RenderMessage app FormMessage
  , Cms app
  , CmsActionLog app
  , CmsRoles app
  )

data CrudHandler site p c = CrudHandler
  { chAdd :: p -> HandlerT site IO Html
  , chIndex :: p -> HandlerT site IO Html
  , chEdit :: Key c -> HandlerT site IO Html
  , chDelete :: Key c -> HandlerT site IO Html
  , chView :: Key c -> HandlerT site IO Html
  }

handleCrud :: CrudHandler site p c -> CrudRoute p c -> HandlerT site IO Html
handleCrud (CrudHandler h _ _ _ _) (AddR p) = h p
handleCrud (CrudHandler _ h _ _ _) (IndexR p) = h p
handleCrud (CrudHandler _ _ h _ _) (EditR p) = h p
handleCrud (CrudHandler _ _ _ h _) (DeleteR p) = h p
handleCrud (CrudHandler _ _ _ _ h) (ViewR p) = h p

data CrudRoute p c
  = IndexR p
  | AddR p
  | EditR (Key c)
  | DeleteR (Key c)
  | ViewR (Key c)

data EditParent = EditParentView | EditParentIndex

data ViewParent app p
  = ViewParentIndex
  | ViewParentOther (p -> Route app)

deriving instance (Eq (Key c), Eq p) => Eq (CrudRoute p c)
deriving instance (Show (Key c), Show p) => Show (CrudRoute p c)
deriving instance (Read (Key c), Read p) => Read (CrudRoute p c)

instance (PathPiece (Key c), Eq (Key c), PathPiece p, Eq p) =>
         PathMultiPiece (CrudRoute p c) where
  fromPathMultiPiece xs =
    (AddR <$> runSM xs (match "add" *> consumeKey)) <|>
    (IndexR <$> runSM xs (match "index" *> consumeKey)) <|>
    (EditR <$> runSM xs (match "edit" *> consumeKey)) <|>
    (DeleteR <$> runSM xs (match "delete" *> consumeKey)) <|>
    (ViewR <$> runSM xs (match "view" *> consumeKey))
  toPathMultiPiece (AddR p) = ["add", toPathPiece p]
  toPathMultiPiece (IndexR p) = ["index", toPathPiece p]
  toPathMultiPiece (EditR cid) = ["edit", toPathPiece cid]
  toPathMultiPiece (DeleteR cid) = ["delete", toPathPiece cid]
  toPathMultiPiece (ViewR cid) = ["view", toPathPiece cid]

runSM :: [Text] -> StateT [Text] Maybe a -> Maybe a
runSM xs a = evalStateT (a <* (get >>= guard . null)) xs

match :: Text -> StateT [Text] Maybe ()
match t = attemptTakeNextPiece >>= guard . (== t)

consumeKey :: PathPiece k => StateT [Text] Maybe k
consumeKey = maybe mzero return . fromPathPiece =<< attemptTakeNextPiece

attemptTakeNextPiece :: StateT [b] Maybe b
attemptTakeNextPiece = do
  s <- get
  case s of
    (a:as) -> put as >> return a
    [] -> mzero
