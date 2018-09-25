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
import Cms.ActionLog.Class (logMsg)
import Cms.Class (adminLayout)
import Cms.Crud.Route (EditParent, ViewParent, CrudHandler(..), PersistCrudEntity, CrudRoute(..), CrudForm)
import Cms.Roles.Class (getCan)
import Colonnade (Colonnade, Headedness)
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

data SimplerCrud app a s = SimplerCrud
  { crudSimplerForm :: Maybe a -> UTCTime -> CrudForm app a
  , crudSimplerTable :: [(s, Maybe Text)] -> Html
  , crudSimplerDb :: CrudDb app () a s
  , crudSimplerMsg :: CrudMessages app a
  }

data CrudMessages app a = CrudMessages
  { crudMsgIndex :: SomeMessage app
  , crudMsgNew :: SomeMessage app
  , crudMsgEdit :: SomeMessage app
  , crudMsgBack :: SomeMessage app
  , crudMsgDelete :: SomeMessage app
  , crudMsgNoEntities :: SomeMessage app
  , crudMsgCreated :: a -> SomeMessage app
  , crudMsgUpdated :: a -> SomeMessage app
  , crudMsgDeleted :: a -> SomeMessage app
  }

data CrudDb app p c s = CrudDb
  { crudDbSelect :: p -> YesodDB app [s]
  , crudDbSelectKey :: s -> Key c
  , crudDbAdd :: p -> c -> YesodDB app (Key c)
  , crudDbEdit :: Key c -> c -> YesodDB app p
  , crudDbDelete :: Key c -> YesodDB app p
  }

defaultCrudDb
  :: PersistCrudEntity app a
  => CrudDb app () a (Entity a)
defaultCrudDb =
  CrudDb (const $ selectList [] []) entityKey (const insert) replace delete

simplerCrudToHandler
  :: PersistCrudEntity app a
  => SimplerCrud app a s -> (CrudRoute () a -> Route app) -> CrudHandler app () a
simplerCrudToHandler SimplerCrud {..} crudBaseR = CrudHandler
  { chIndex = \_ -> do
      let crudIndexWidget = crudSimplerTable
      can <- getCan
      render <- getUrlRenderParams
      entities <-
        fmap
        (fmap (id &&& fmap (`render` []) . flip can "GET" . crudBaseR . EditR . crudDbSelectKey))
        (runDB $ crudDbSelect ())
      adminLayout $ do
        setTitleI crudMsgIndex
        $(widgetFileNoReload def "index")
  , chAdd = \_ -> do
      ct <- liftIO getCurrentTime
      ((results, fWidget), enctype) <- runFormPost $ crudSimplerForm Nothing ct
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
      ((results, fWidget), enctype) <- runFormPost $ crudSimplerForm (Just e) ct
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
  where
    CrudMessages {..} = crudSimplerMsg
    CrudDb {..} = crudSimplerDb

encodeClickableTable
  :: (Applicative h, Headedness h, Foldable f, Foldable h)
  => Colonnade h (a, Maybe Text) Html
  -> f (a, Maybe Text)
  -> Html
encodeClickableTable t =
  (H.div H.! HA.class_ "table-responsive") .
  encodeTable
    (pure (mempty, mempty))
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
