{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Cms.Roles.Class where

import ClassyPrelude.Yesod
import qualified Data.Set as S
import Yesod.Auth

-- | Specifies the criteria for authorizing a request.
data Allow a = AllowAll            -- ^ Allow any request (no authentication required).
             | AllowAuthenticated  -- ^ Allow requests in authenticated sessions.
             | AllowRoles a        -- ^ Allow requests in authenticated sessions belonging
                                   -- to users that have at least one matching role.
                                   -- See the `isAuthorizedTo` function for details.
             | AllowNone           -- ^ Allow no requests at all.

class ( YesodAuth app
      , Ord (Roles app)
      ) => CmsRoles app where
  type Roles app
  getUserRoles :: AuthId app -> HandlerT app IO (Set (Roles app))
  setUserRoles :: AuthId app -> Set (Roles app) -> HandlerT app IO ()
  mayAssignRoles :: HandlerT app IO Bool

  defaultRoles :: HandlerT app IO (Set (Roles app))
  defaultRoles = return S.empty

  actionAllowedFor :: Route app -> ByteString -> Allow (Set (Roles app))
  actionAllowedFor _ _ = AllowNone

  isAuthorizedTo :: app
                 -> Maybe (Set (Roles app))
                 -> Allow (Set (Roles app))
                 -> AuthResult
  isAuthorizedTo _ _ AllowNone = Unauthorized "Access denied."
  isAuthorizedTo _ _ AllowAll = Authorized
  isAuthorizedTo _ (Just _) AllowAuthenticated = Authorized
  isAuthorizedTo _ Nothing _ = AuthenticationRequired
  isAuthorizedTo _ (Just urs) (AllowRoles rrs) =
    if S.null (urs `S.intersection` rrs)
      then Unauthorized "Access denied."
      else Authorized


canFor
  :: CmsRoles app
  => app
  -> Maybe (Set (Roles app))
  -> Route app
  -> ByteString
  -> Maybe (Route app)
canFor m murs theRoute method' =
  case isAuthorizedTo m murs $ actionAllowedFor theRoute method' of
    Authorized -> Just theRoute
    _ -> Nothing

getCan
  :: CmsRoles app
  => HandlerT app IO (Route app -> ByteString -> Maybe (Route app))
getCan = do
  mauthId <- maybeAuthId
  canFor <$> getYesod <*> mapM getUserRoles mauthId
