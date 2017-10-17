{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Cms.ActionLog.Class
  ( CmsActionLog(..)
  ) where

import ClassyPrelude.Yesod

class Yesod app => CmsActionLog app where
  data Log app :: *
  logMsg :: RenderMessage app m => m -> HandlerT app IO ()
