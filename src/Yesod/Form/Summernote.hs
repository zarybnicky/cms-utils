{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Yesod.Form.Summernote
    ( YesodSummernote (..)
    , snHtmlField
    , snHtmlFieldCustomized
    ) where

import Control.Monad (when)
import Data.Maybe (listToMaybe)
import Data.Text (Text, pack)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Hamlet (shamlet, Html)
import Text.Julius (julius, rawJS)
import Yesod.Core (Yesod, HandlerFor, Route, addScriptEither,
                   addStylesheetEither, toWidget, getYesod, preEscapedToMarkup)
import Yesod.Form (Field(..), Enctype(UrlEncoded))

class Yesod a => YesodSummernote a where
    urlSummernoteCss :: a -> Either (Route a) Text
    urlSummernoteScript :: a -> Either (Route a) Text
    summernoteLoadLibrariesAndCss :: a -> Bool
    summernoteLoadLibrariesAndCss _ = False

snHtmlFieldCustomized :: YesodSummernote site => String -> Field (HandlerFor site) Html
snHtmlFieldCustomized cfg = Field
    { fieldParse =
        \e _ -> return $
            Right . fmap preEscapedToMarkup . listToMaybe $ e
    , fieldView = \theId name attrs val _isReq -> do
        toWidget [shamlet|
$newline never
<textarea id="#{theId}" *{attrs} name="#{name}" .html>#{showVal val}
|]
        master <- getYesod
        when (summernoteLoadLibrariesAndCss master) $ do
            addStylesheetEither (urlSummernoteCss master)
            addScriptEither (urlSummernoteScript master)
        toWidget [julius|
$(document).ready(function(){
  var input = document.getElementById("#{rawJS theId}");
  $(input).summernote(#{rawJS cfg}).on('summernote.change',function(){
    $(input).text($(input).summernote('code'));
  });
});|]
    , fieldEnctype = UrlEncoded
    }
  where
    showVal = either id (pack . renderHtml)

snHtmlField :: YesodSummernote site => Field (HandlerFor site) Html
snHtmlField = snHtmlFieldCustomized ""
