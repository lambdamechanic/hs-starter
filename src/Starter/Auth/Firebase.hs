{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Starter.Auth.Firebase
  ( FirebaseAuth (..),
    FirebaseAuthConfig (..),
    FirebaseAuthError (..),
    FirebaseUser (..),
    Protected,
    firebaseAuthDisabled,
    firebaseAuthHandler,
    firebaseContext,
    loadFirebaseAuthFromEnv,
    mkFirebaseAuth,
  )
where

import Control.Exception (SomeException, displayException, try)
import Control.Lens ((.~), (^.), (^?))
import Control.Monad.Except (ExceptT (..), runExceptT, withExceptT)
import qualified Control.Monad.Except as Except
import Control.Monad.IO.Class (liftIO)
import Crypto.JOSE (JWKSet)
import Crypto.JWT
  ( Audience (Audience),
    ClaimsSet,
    JWTError,
    SignedJWT,
    StringOrURI,
    allowedSkew,
    claimAud,
    claimIss,
    claimSub,
    defaultJWTValidationSettings,
    decodeCompact,
    issuerPredicate,
    string,
    unregisteredClaims,
    verifyClaimsAt,
  )
import Data.Aeson (Value, eitherDecodeStrict')
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Result (Error, Success))
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict qualified as HashMap
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.String (fromString)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import Network.HTTP.Client
  ( Manager,
    httpLbs,
    newManager,
    parseRequest,
    responseBody,
    responseStatus,
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (hAuthorization, statusIsSuccessful)
import Network.Wai (Request, requestHeaders)
import OpenTelemetry.Instrumentation.Servant.Internal (HasEndpoint (..))
import Servant
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
import Starter.Prelude
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

-- | Convenience type alias for guarding routes with Firebase auth.
type Protected api = AuthProtect "firebase" :> api

-- | Servant auth handler will provide a 'FirebaseUser' for protected routes.
type instance AuthServerData (AuthProtect "firebase") = FirebaseUser

instance HasEndpoint api => HasEndpoint (AuthProtect tag :> api) where
  getEndpoint _ = getEndpoint (Proxy :: Proxy api)

-- | Runtime Firebase authentication configuration.
data FirebaseAuthConfig = FirebaseAuthConfig
  { firebaseConfigProjectId :: Text,
    firebaseConfigJwksUri :: Text,
    firebaseConfigAllowedSkew :: NominalDiffTime,
    firebaseConfigCacheTtl :: NominalDiffTime
  }
  deriving stock (Eq, Show)

-- | Holds the verifier used by the Servant auth handler.
data FirebaseAuth = FirebaseAuth
  { firebaseProjectId :: Text,
    firebaseVerifyIdToken :: Text -> IO (Either FirebaseAuthError FirebaseUser)
  }

-- | High level classification of failures when checking an ID token.
data FirebaseAuthError
  = FirebaseAuthInvalidToken Text
  | FirebaseAuthClaimsError Text
  | FirebaseAuthJwksError Text
  | FirebaseAuthUnavailable Text
  deriving stock (Eq, Show)

-- | Minimal Firebase user representation exposed to handlers.
data FirebaseUser = FirebaseUser
  { uid :: Text,
    issuer :: Text,
    audience :: [Text],
    email :: Maybe Text,
    emailVerified :: Maybe Bool,
    name :: Maybe Text,
    picture :: Maybe Text,
    claims :: HashMap.HashMap Text Value
  }
  deriving stock (Eq, Show)

instance Aeson.ToJSON FirebaseUser where
  toJSON FirebaseUser {uid, issuer, audience, email, emailVerified, name, picture, claims} =
    Aeson.object
      [ "uid" Aeson..= uid,
        "issuer" Aeson..= issuer,
        "audience" Aeson..= audience,
        "email" Aeson..= email,
        "emailVerified" Aeson..= emailVerified,
        "name" Aeson..= name,
        "picture" Aeson..= picture,
        "claims" Aeson..= claims
      ]

-- | Construct an auth handler for Servant using the configured verifier.
firebaseAuthHandler :: FirebaseAuth -> AuthHandler Request FirebaseUser
firebaseAuthHandler auth =
  mkAuthHandler $ \req -> do
    token <- either Except.throwError pure (extractBearer req)
    result <- liftIO (firebaseVerifyIdToken auth token)
    either (Except.throwError . toServerError) pure result

-- | Build the Servant context needed for 'serveWithContext'.
firebaseContext :: FirebaseAuth -> Context (AuthHandler Request FirebaseUser ': '[])
firebaseContext auth = firebaseAuthHandler auth :. EmptyContext

-- | A placeholder configuration that always reports Firebase auth as unavailable.
firebaseAuthDisabled :: FirebaseAuth
firebaseAuthDisabled =
  FirebaseAuth
    { firebaseProjectId = "firebase-disabled",
      firebaseVerifyIdToken = \_ -> pure (Left (FirebaseAuthUnavailable "Firebase auth is not configured"))
    }

-- | Load Firebase configuration from environment variables, constructing the verifier.
loadFirebaseAuthFromEnv :: IO (Either Text FirebaseAuth)
loadFirebaseAuthFromEnv = do
  projectIdEnv <- lookupEnv "FIREBASE_PROJECT_ID"
  case projectIdEnv of
    Nothing -> pure (Left "FIREBASE_PROJECT_ID is not set")
    Just rawProjectId -> do
      let projectId = Text.pack rawProjectId
      jwksUri <- maybe defaultJwksUri Text.pack <$> lookupEnv "FIREBASE_CERTS_URL"
      skewSeconds <- lookupEnv "FIREBASE_TOKEN_SKEW_SECONDS"
      clockSkew <- case skewSeconds of
        Nothing -> pure defaultSkew
        Just rawSkew ->
          case readMaybe rawSkew :: Maybe Double of
            Nothing -> pure defaultSkew
            Just seconds
              | seconds < 0 -> pure defaultSkew
              | otherwise -> pure (realToFrac seconds)
      auth <- mkFirebaseAuth
        FirebaseAuthConfig
          { firebaseConfigProjectId = projectId,
            firebaseConfigJwksUri = jwksUri,
            firebaseConfigAllowedSkew = clockSkew,
            firebaseConfigCacheTtl = defaultCacheTtl
          }
      pure (Right auth)

-- | Construct a Firebase verifier using the provided configuration.
mkFirebaseAuth :: FirebaseAuthConfig -> IO FirebaseAuth
mkFirebaseAuth config = do
  manager <- newManager tlsManagerSettings
  cache <- newIORef Nothing
  let runtime = FirebaseRuntime
        { runtimeConfig = config,
          runtimeManager = manager,
          runtimeCache = cache,
          runtimeExpectedIssuer = fromString (Text.unpack issuerText),
          runtimeExpectedAudience = fromString (Text.unpack (firebaseConfigProjectId config))
        }
  pure
    FirebaseAuth
      { firebaseProjectId = firebaseConfigProjectId config,
        firebaseVerifyIdToken = verifyFirebaseToken runtime
      }
  where
    issuerText = "https://securetoken.google.com/" <> firebaseConfigProjectId config

-- | Internal structure storing live verifier state.
data FirebaseRuntime = FirebaseRuntime
  { runtimeConfig :: FirebaseAuthConfig,
    runtimeManager :: Manager,
    runtimeCache :: IORef (Maybe CachedJwks),
    runtimeExpectedIssuer :: StringOrURI,
    runtimeExpectedAudience :: StringOrURI
  }

data CachedJwks = CachedJwks
  { cachedSet :: JWKSet,
    cachedExpiry :: UTCTime
  }

-- | Perform full verification of a Firebase ID token.
verifyFirebaseToken :: FirebaseRuntime -> Text -> IO (Either FirebaseAuthError FirebaseUser)
verifyFirebaseToken runtime rawToken =
  runExceptT $ do
    signed <- withExceptT (FirebaseAuthInvalidToken . renderJwtError) (decodeToken rawToken)
    jwks <- fetchJwks runtime
    now <- liftIO getCurrentTime
    let settings =
          defaultJWTValidationSettings (== runtimeExpectedAudience runtime)
            & issuerPredicate .~ (== runtimeExpectedIssuer runtime)
            & allowedSkew .~ firebaseConfigAllowedSkew (runtimeConfig runtime)
    claims <-
      withExceptT (FirebaseAuthInvalidToken . renderJwtError)
        $ ExceptT (runExceptT (verifyClaimsAt settings jwks now signed) :: IO (Either JWTError ClaimsSet))
    withExceptT FirebaseAuthClaimsError (ExceptT (pure (claimsToUser claims)))

-- | Translate validated claims into the simplified 'FirebaseUser'.
claimsToUser :: ClaimsSet -> Either Text FirebaseUser
claimsToUser claims = do
  subject <-
    maybe (Left "missing subject claim") (Right . stringOrUriToText)
      (claims ^. claimSub)
  issuerText <-
    maybe (Left "missing issuer claim") (Right . stringOrUriToText)
      (claims ^. claimIss)
  let audienceTexts =
        case claims ^. claimAud of
          Nothing -> []
          Just (Audience entries) -> fmap stringOrUriToText entries
      extraList = Map.toList (claims ^. unregisteredClaims)
      extraClaims = HashMap.fromList extraList
      lookupClaim :: Aeson.FromJSON a => Text -> Maybe a
      lookupClaim key = HashMap.lookup key extraClaims >>= fromValue
      emailClaim = lookupClaim "email" :: Maybe Text
      emailVerifiedClaim = lookupClaim "email_verified" :: Maybe Bool
      nameClaim = lookupClaim "name" :: Maybe Text
      pictureClaim = lookupClaim "picture" :: Maybe Text
  pure
    FirebaseUser
      { uid = subject,
        issuer = issuerText,
        audience = audienceTexts,
        email = emailClaim,
        emailVerified = emailVerifiedClaim,
        name = nameClaim,
        picture = pictureClaim,
        claims = extraClaims
      }

-- | Fetch JWKS, respecting cache expiry if present.
fetchJwks :: FirebaseRuntime -> ExceptT FirebaseAuthError IO JWKSet
fetchJwks runtime = do
  now <- liftIO getCurrentTime
  cached <- liftIO $ readIORef (runtimeCache runtime)
  case cached of
    Just CachedJwks {cachedSet, cachedExpiry}
      | now < cachedExpiry -> pure cachedSet
    _ -> do
      jwks <- downloadJwks runtime
      let expiry = addUTCTime (firebaseConfigCacheTtl (runtimeConfig runtime)) now
      liftIO $ writeIORef (runtimeCache runtime) (Just CachedJwks {cachedSet = jwks, cachedExpiry = expiry})
      pure jwks

-- | Retrieve the JWKS from the configured endpoint.
downloadJwks :: FirebaseRuntime -> ExceptT FirebaseAuthError IO JWKSet
downloadJwks runtime = do
  request <- ExceptT $ toEither <$> try (parseRequest (Text.unpack uriText))
  response <- ExceptT $ toEither <$> try (httpLbs request (runtimeManager runtime))
  unless (statusIsSuccessful (responseStatus response))
    $ Except.throwError
      ( FirebaseAuthJwksError
          ( "Failed to fetch Firebase JWKS, status "
              <> Text.pack (show (responseStatus response))
          )
      )
  case eitherDecodeStrict' (BL.toStrict (responseBody response)) of
    Left err -> Except.throwError (FirebaseAuthJwksError (Text.pack err))
    Right jwks -> pure jwks
  where
    uriText = firebaseConfigJwksUri (runtimeConfig runtime)
    toEither :: Either SomeException a -> Either FirebaseAuthError a
    toEither = first (FirebaseAuthJwksError . Text.pack . displayException)

renderJwtError :: JWTError -> Text
renderJwtError = Text.pack . show

-- | Convert a claims value to a specific type.
fromValue :: Aeson.FromJSON a => Value -> Maybe a
fromValue v =
  case Aeson.fromJSON v of
    Error _ -> Nothing
    Success a -> Just a

-- | Extract a bearer token from the incoming request.
extractBearer :: Request -> Either ServerError Text
extractBearer req =
  case lookup hAuthorization (requestHeaders req) of
    Nothing -> Left unauthorizedMissing
    Just headerValue ->
      case decodeUtf8' headerValue of
        Left _ -> Left unauthorizedInvalid
        Right decoded ->
          case parseBearer decoded of
            Nothing -> Left unauthorizedInvalid
            Just token -> Right token
  where
    parseBearer txt =
      case Text.words (Text.strip txt) of
        [prefix, token]
          | Text.toLower prefix == "bearer" -> Just token
        _ -> Nothing
    unauthorizedMissing = attachWww err401 {errBody = textBody "Missing Authorization header"}
    unauthorizedInvalid = attachWww err401 {errBody = textBody "Invalid Authorization header"}

-- | Map domain errors onto Servant 'ServerError's.
toServerError :: FirebaseAuthError -> ServerError
toServerError = \case
  FirebaseAuthInvalidToken msg -> attachWww err401 {errBody = textBody msg}
  FirebaseAuthClaimsError msg -> attachWww err401 {errBody = textBody msg}
  FirebaseAuthJwksError msg -> attachWww err503 {errBody = textBody msg}
  FirebaseAuthUnavailable msg -> attachWww err503 {errBody = textBody msg}

-- | Attach the canonical WWW-Authenticate header for Bearer auth.
attachWww :: ServerError -> ServerError
attachWww err =
  err
    { errHeaders =
        ("WWW-Authenticate", "Bearer realm=\"firebase\"")
          : filter ((/= "WWW-Authenticate") . fst) (errHeaders err)
    }

-- | Render text to a lazy byte string for response bodies.
textBody :: Text -> BL.ByteString
textBody = BL.fromStrict . encodeUtf8

-- | Convert StringOrURI to Text for easier handling.
stringOrUriToText :: StringOrURI -> Text
stringOrUriToText sor =
  case sor ^? string of
    Just txt -> txt
    Nothing -> Text.pack (show sor)

-- | Decode the compact JWT representation.
decodeToken :: Text -> ExceptT JWTError IO SignedJWT
decodeToken tokenText =
  ExceptT . pure $ decodeCompact (BL.fromStrict (encodeUtf8 tokenText))

-- | Default JWKS endpoint hosted by Google for Firebase.
defaultJwksUri :: Text
defaultJwksUri = "https://www.googleapis.com/service_accounts/v1/jwk/securetoken@system.gserviceaccount.com"

-- | Default allowable clock skew (5 minutes).
defaultSkew :: NominalDiffTime
defaultSkew = 300

-- | Default JWKS cache duration (10 minutes).
defaultCacheTtl :: NominalDiffTime
defaultCacheTtl = 600
