{-# LANGUAGE OverloadedStrings     #-}

module Cms.Mailer.Class where

import ClassyPrelude.Yesod (HandlerFor, liftIO, mapMaybe, isJust)
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as T
import Data.Monoid ((<>))
import Network.Mail.Mime (Mail(..), Part(..), Address(..))

class CmsMailer app where
  sendMail :: Mail -> HandlerFor app ()
  sendMail (Mail from tos ccs bccs headers parts) =
    liftIO . T.putStrLn $
    T.unlines
      [ "MAIL"
      , "  From: " <> address from
      , "  To: " <> maddress tos
      , "  Cc: " <> maddress ccs
      , "  Bcc: " <> maddress bccs
      , "  Subject: " <> subject
      , "  Attachment: " <> attachment
      , "  Plain body: " <> getFromParts "text/plain; charset=utf-8"
      , "  Html body: " <> getFromParts "text/html; charset=utf-8"
      ]
    where
      subject = T.concat . map snd $ filter (\(k, _) -> k == "Subject") headers
      attachment :: T.Text
      attachment =
        T.intercalate ", " . mapMaybe partFilename $
        concatMap (filter (isJust . partFilename)) parts
      getFromParts x =
        TE.decodeUtf8 . BC.toStrict . BC.concat . map partContent $
        concatMap (filter ((==) x . partType)) parts
      maddress = T.intercalate ", " . map address
      address (Address n e) =
        let e' = "<" <> e <> ">"
        in case n of
             Just n' -> n' <> " " <> e'
             Nothing -> e'
