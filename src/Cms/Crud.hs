{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Cms.Crud where

import ClassyPrelude.Yesod hiding (get, (<>))
import Cms.ActionLog.Class
import Cms.Class
import Cms.Crud.Route
import Cms.Roles.Class
import Colonnade (Colonnade)
import Data.Default (def)
import Data.Monoid ((<>))
import Text.Blaze (toValue)
import Text.Blaze.Colonnade (encodeTable)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Yesod.Default.Util (widgetFileNoReload)

unit :: ()
unit = ()

data SimpleCrud app p c = SimpleCrud
  { scAdd          :: WidgetT app IO () -> HandlerT app IO Html
  , scIndex        :: p -> HandlerT app IO Html
  , scView         :: Key c -> HandlerT app IO Html
  , scEdit         :: WidgetT app IO () -> HandlerT app IO Html
  , scDelete       :: WidgetT app IO () -> HandlerT app IO Html
  , scDeleteForm   :: WidgetT app IO () 
  , scForm         :: Either p c -> Html -> MForm (HandlerT app IO) (FormResult c, WidgetT app IO ())
  , scFormWrap     :: Enctype -> Route app -> WidgetT app IO () -> WidgetT app IO ()
  , scDeleteDb     :: Key c -> YesodDB app p
  , scAddDb        :: p -> c -> YesodDB app (Key c)
  , scEditDb       :: Key c -> c -> YesodDB app p
  , scMessageWrap  :: Html -> Html
  , scEditParent   :: EditParent
  , scViewParent   :: ViewParent app p
  , scPromoteRoute :: CrudRoute p c -> Route app
  }

data SimplerCrud app a m = SimplerCrud
  { crudBaseR :: CrudRoute () a -> Route app
  , crudForm :: Maybe a -> UTCTime -> CrudForm app a
  , crudIndexWidget :: [(Entity a, Maybe Text)] -> Html
  , crudMsgIndex :: m
  , crudMsgNew :: m
  , crudMsgEdit :: m
  , crudMsgBack :: m
  , crudMsgDelete :: m
  , crudMsgNoEntities :: m
  , crudMsgCreated :: a -> m
  , crudMsgUpdated :: a -> m
  , crudMsgDeleted :: a -> m
  , crudInsertQuery :: a -> YesodDB app (Key a)
  , crudUpdateQuery :: Key a -> a -> YesodDB app ()
  , crudDeleteQuery :: Key a -> YesodDB app ()
  }

emptySimplerCrud
  :: (PersistCrudEntity app a m)
  => (CrudRoute () a -> Route app) -> SimplerCrud app a m
emptySimplerCrud r = SimplerCrud
  r
  (const . const . const $ return (FormMissing, mempty))
  (const mempty)
  (error "Please define your CRUD messages")
  (error "Please define your CRUD messages")
  (error "Please define your CRUD messages")
  (error "Please define your CRUD messages")
  (error "Please define your CRUD messages")
  (error "Please define your CRUD messages")
  (const (error "Please define your CRUD messages"))
  (const (error "Please define your CRUD messages"))
  (const (error "Please define your CRUD messages"))
  insert
  replace
  delete

simplerCrudToHandler
  :: PersistCrudEntity app a m
  => SimplerCrud app a m -> CrudHandler app () a
simplerCrudToHandler SimplerCrud {..} = CrudHandler
  { chIndex = \_ -> do
      can <- getCan
      render <- getUrlRenderParams
      entities <-
        fmap
        (fmap
          (\x@(Entity key _) ->
             (x, flip render [] <$> can (crudBaseR (EditR key)) "GET")))
        (runDB $ selectList [] [])
      adminLayout $ do
        setTitleI crudMsgIndex
        $(widgetFileNoReload def "index")
  , chAdd = \_ -> do
      ct <- liftIO getCurrentTime
      ((results, fWidget), enctype) <- runFormPost $ crudForm Nothing ct
      case results of
        FormSuccess x -> do
          _ <- runDB $ insert x
          logMsg $ crudMsgCreated x
          setMessageI $ crudMsgCreated x
          redirect $ crudBaseR (IndexR ())
        _ -> do
          can <- getCan
          adminLayout $ do
            setTitleI $ crudMsgNew
            $(widgetFileNoReload def "new")
  , chEdit = \eid -> do
      e <- runDB $ get404 eid
      ct <- liftIO getCurrentTime
      ((results, fWidget), enctype) <- runFormPost $ crudForm (Just e) ct
      case results of
        FormSuccess new -> do
          runDB $ replace eid new
          logMsg $ crudMsgUpdated new
          setMessageI $ crudMsgUpdated new
          redirect $ crudBaseR (IndexR ())
        _ -> do
          can <- getCan
          adminLayout $ do
            setTitleI crudMsgEdit
            $(widgetFileNoReload def "edit")
  , chDelete = \eid -> do
      e <- runDB $ get404 eid
      runDB $ delete eid
      logMsg $ crudMsgDeleted e
      setMessageI $ crudMsgDeleted e
      redirect $ crudBaseR (IndexR ())
  , chView = const (return mempty)
  }

encodeClickableTable
  :: (Functor h, Foldable f, Foldable h)
  => Colonnade h (a, Maybe Text) Html
  -> f (a, Maybe Text)
  -> Html
encodeClickableTable t =
  (H.div H.! HA.class_ "table-responsive") .
  encodeTable
    (Just (mempty, mempty))
    mempty
    onclick
    id
    (HA.class_ "table table-striped table-hover")
    t
  where
    onclick (_, mroute) =
      case mroute of
        Nothing -> mempty
        Just route' ->
          HA.onclick ("document.location.href='" <> toValue route' <> "'")
