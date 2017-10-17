{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Cms.Class where

import ClassyPrelude.Yesod
import Yesod.Auth (YesodAuth, YesodAuthPersist)

data AdminMenuItem master = MenuItem
    { label :: SomeMessage master -- ^ The text of the item (what the user sees).
    , route :: Route master       -- ^ The Route to which it points.
    , icon  :: Text               -- ^ A <http://glyphicons.bootstrapcheatsheets.com glyphicon> without the ".glyphicon-" prefix.
    }

class ( Yesod app
      , YesodPersist app
      , YesodAuth app
      , YesodAuthPersist app
      , YesodPersistBackend app ~ SqlBackend
      ) => Cms app where
  adminLayout :: WidgetT app IO () -> HandlerT app IO Html
  adminMenu :: [AdminMenuItem app]

  -- | A list of languages to render.
  renderLanguages :: app -> [Text]
  renderLanguages _ = ["en"]

