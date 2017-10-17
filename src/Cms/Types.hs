{-# LANGUAGE TypeFamilies #-}

module Cms.Types where

import ClassyPrelude.Yesod
       (MonadWidget, HandlerSite, Route, addScriptEither,
        addStylesheetEither)
import Data.Text (Text)

class ToHuman a where
  toHuman :: a -> Text

class ToHistoryItem a where
  type HistoryItem a :: *
  toHistoryItem :: a -> HistoryItem a
  fromHistoryItem :: HistoryItem a -> a

class HasBootstrap a where
  urlBootstrapCss :: a -> Either (Route a) Text
  urlBootstrapScript :: a -> Either (Route a) Text
  urlJQueryScript :: a -> Either (Route a) Text
  bootstrapWidget :: (MonadWidget m, HandlerSite m ~ a) => a -> m ()
  bootstrapWidget a = do
    addScriptEither (urlJQueryScript a)
    addScriptEither (urlBootstrapScript a)
    addStylesheetEither (urlBootstrapCss a)

class HasFavicon a where
  faviconWidget :: (MonadWidget m, HandlerSite m ~ a) => a -> m ()
