{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Starter.Server
  ( app,
    apiProxy,
    Api,
    HealthApi,
    PublicApi,
    PrivateApi,
    server,
    healthServer,
    HealthStatus (..),
    HealthCheckReport (..),
  )
where

import Control.Exception (SomeException, displayException, throwIO, try)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON, Value, encode, object, (.=), (.:), (.:?))
import Data.Aeson qualified as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Bool (bool)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import Data.ByteString.Builder (toLazyByteString)
import Data.Maybe (listToMaybe)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Data.Version (showVersion)
import Lucid (Html, ToHtml (..), toHtmlRaw)
import Lucid.Base (renderBS)
import Lucid.Html5
  ( a_,
    alt_,
    body_,
    charset_,
    class_,
    content_,
    div_,
    dl_,
    dd_,
    doctypehtml_,
    dt_,
    footer_,
    head_,
    h1_,
    h2_,
    html_,
    href_,
    id_,
    img_,
    lang_,
    link_,
    rel_,
    main_,
    meta_,
    name_,
    nav_,
    p_,
    script_,
    src_,
    title_,
    type_
  )
import OpenTelemetry.Instrumentation.Servant.Internal (HasEndpoint (getEndpoint))
import Servant
import Squeal.PostgreSQL (Jsonb (..))
import Squeal.PostgreSQL qualified as PQ
import Starter.Database.Connection (DbConfig (..), withAppConnection)
import Starter.Database.Users
  ( DbUserRow (..),
    insertLoginEvent,
    selectUserCount,
    upsertUser,
  )
import Starter.Auth.Firebase (FirebaseAuth (..), FirebaseUser (..), toServerError)
import Starter.Auth.Session (Protected, SessionUser (..), mkSessionCookie, sessionContext)
import Starter.Env (AppEnv (..))
import Starter.Prelude
import Paths_hs_starter qualified as Paths
import Web.Cookie (renderSetCookie)

-- | HTML response type rendered via lucid.
data HtmlView

instance Accept HtmlView where
  contentType _ = "text/html; charset=utf-8"

instance MimeRender HtmlView (Html ()) where
  mimeRender _ = renderBS

instance HasEndpoint api => HasEndpoint (AuthProtect tag :> api) where
  getEndpoint _ req = getEndpoint (Proxy :: Proxy api) req

-- | API type definition for the Servant server.
type HealthApi = "health" :> Get '[JSON] HealthStatus

type HomeRoute = Get '[HtmlView] (Html ())

type LoginRoute = "login" :> Get '[HtmlView] (Html ())

type PublicApi = HomeRoute :<|> LoginRoute

type SessionApi =
  "session"
    :> "exchange"
    :> ReqBody '[JSON] SessionExchangeRequest
    :> Post '[JSON] (Headers '[Header "Set-Cookie" Text] SessionExchangeResponse)

type PrivateApi =
  Protected
    ( "me"
        :> Get '[HtmlView] (Html ())
    )

type Api = HealthApi :<|> PublicApi :<|> SessionApi :<|> PrivateApi

type ProtectedMeRoute =
  AuthProtect "session"
    :> "me"
    :> Get '[HtmlView] (Html ())

homeRouteProxy :: Proxy HomeRoute
homeRouteProxy = Proxy

loginRouteProxy :: Proxy LoginRoute
loginRouteProxy = Proxy

protectedMeProxy :: Proxy ProtectedMeRoute
protectedMeProxy = Proxy

homeLink :: Link
homeLink = safeLink apiProxy homeRouteProxy

loginLink :: Link
loginLink = safeLink apiProxy loginRouteProxy

protectedMeLink :: Link
protectedMeLink = safeLink apiProxy protectedMeProxy

data HealthStatus = HealthStatus
  { status :: Text,
    timestamp :: UTCTime,
    service :: Text,
    version :: Text,
    checks :: HashMap.HashMap Text HealthCheckReport
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data HealthCheckReport = HealthCheckReport
  { status :: Text,
    observedAt :: UTCTime,
    durationMs :: Double,
    details :: Value
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data SessionExchangeRequest = SessionExchangeRequest
  { serIdToken :: Text,
    serReturnTo :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON SessionExchangeRequest where
  parseJSON =
    Aeson.withObject "SessionExchangeRequest" $ \obj -> do
      token <- obj .: "idToken"
      returnTo <- obj .:? "return_to"
      pure SessionExchangeRequest {serIdToken = token, serReturnTo = returnTo}

data SessionExchangeResponse = SessionExchangeResponse
  { redirect :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

data ErrorResponse = ErrorResponse
  { error :: ErrorBody
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

data ErrorBody = ErrorBody
  { code :: Text,
    message :: Text,
    details :: Value
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

apiProxy :: Proxy Api
apiProxy = Proxy

app :: AppEnv -> Application
app env = serveWithContext apiProxy (sessionContext (sessionConfig env)) (server env)

server :: AppEnv -> Server Api
server env = healthServer env :<|> publicServer env :<|> sessionServer env :<|> privateServer env

healthServer :: AppEnv -> Server HealthApi
healthServer env = do
  timestamp <- liftIO getCurrentTime
  let dbCfg = dbConfig env
      versionText = Text.pack (showVersion Paths.version)
      databaseDetails =
        object
          [ "host" .= dbHost dbCfg,
            "port" .= dbPort dbCfg,
            "database" .= dbName dbCfg,
            "user" .= dbUser dbCfg
          ]
  checkStartedAt <- liftIO getCurrentTime
  dbResult <-
    liftIO . try @SomeException $ withAppConnection dbCfg $ do
      result <- PQ.executeParams selectUserCount ()
      rows <- PQ.getRows result
      case rows of
        [PQ.Only count] -> pure count
        unexpected ->
          liftIO
            $ throwIO
              ( userError
                  ("unexpected result from selectUserCount: " <> show unexpected)
              )
  checkCompletedAt <- liftIO getCurrentTime
  let durationMs :: Double
      durationMs = realToFrac (diffUTCTime checkCompletedAt checkStartedAt) * 1000
  case dbResult of
    Left (ex :: SomeException) ->
      let errorPayload =
            ErrorResponse
              { error =
                  ErrorBody
                    { code = "DATABASE_CHECK_FAILED",
                      message = "Database connectivity check failed",
                      details =
                        object
                          [ "timestamp" .= timestamp,
                            "service" .= otelServiceName env,
                            "version" .= versionText,
                            "observedAt" .= checkCompletedAt,
                            "durationMs" .= durationMs,
                            "database" .= databaseDetails,
                            "exception" .= displayException ex
                          ]
                    }
              }
       in throwError
            err500
              { errBody = encode errorPayload,
                errHeaders = [("Content-Type", "application/json; charset=utf-8")]
              }
    Right count ->
      let databaseReport =
            HealthCheckReport
              { status = "ok",
                observedAt = checkCompletedAt,
                durationMs = durationMs,
                details =
                  object
                    [ "message" .= ("Successfully queried user count" :: Text),
                      "rowCount" .= count,
                      "database" .= databaseDetails
                    ]
              }
       in pure
            HealthStatus
              { status = "ok",
                timestamp = timestamp,
                service = otelServiceName env,
                version = versionText,
                checks = HashMap.fromList [("database", databaseReport)]
              }

publicServer :: AppEnv -> Server PublicApi
publicServer env = pure (homePage env) :<|> pure (loginPage env)

sessionServer :: AppEnv -> Server SessionApi
sessionServer env = sessionExchangeHandler env

privateServer :: AppEnv -> Server PrivateApi
privateServer env (SessionUser user) = meHandler env user

sessionExchangeHandler :: AppEnv -> SessionExchangeRequest -> Handler (Headers '[Header "Set-Cookie" Text] SessionExchangeResponse)
sessionExchangeHandler env SessionExchangeRequest {serIdToken, serReturnTo} = do
  verificationResult <- liftIO (firebaseVerifyIdToken (firebaseAuth env) serIdToken)
  firebaseUser <- either (throwError . toServerError) pure verificationResult
  now <- liftIO getCurrentTime
  allowed <- liftIO (authorizeLogin env firebaseUser)
  dbUser <-
    liftIO (upsertFirebaseUser env firebaseUser allowed now)
      >>= maybe (throwError err500 {errBody = "failed to upsert user"}) pure
  liftIO $ recordFirebaseLogin env dbUser firebaseUser allowed now
  unless allowed $ throwError (forbiddenLogin firebaseUser)
  let cookie = mkSessionCookie (sessionConfig env) now firebaseUser
      cookieHeader =
        decodeUtf8
          ( BL.toStrict
              (toLazyByteString (renderSetCookie cookie))
          )
      redirectTarget = resolveReturnTo serReturnTo
  pure $ addHeader cookieHeader SessionExchangeResponse {redirect = redirectTarget}

meHandler :: AppEnv -> FirebaseUser -> Handler (Html ())
meHandler env user = do
  now <- liftIO getCurrentTime
  allowed <- liftIO (authorizeLogin env user)
  dbUser <-
    liftIO (upsertFirebaseUser env user allowed now)
      >>= maybe (throwError err500 {errBody = "failed to upsert user"}) pure
  liftIO $ recordFirebaseLogin env dbUser user allowed now
  if allowed
    then pure (renderProfilePage (firebaseAuth env) user dbUser)
    else throwError (forbiddenLogin user)

homePage :: AppEnv -> Html ()
homePage _ =
  layoutPage homeLink loginLink "LambdaLabs Starter" $ do
    h1_ "Welcome to hs-starter"
    p_ "This starter kit now ships with a TypeScript frontend for Firebase flows."
    p_ $ do
      "Once signed in, you can "
      a_ [href_ (linkToText protectedMeLink)] "view your profile"
      " to verify stored account details."

loginPage :: AppEnv -> Html ()
loginPage env =
  let FirebaseAuth {firebaseProjectId, firebaseApiKey, firebaseAuthDomain} = firebaseAuth env
      scriptBody = loginScript firebaseProjectId firebaseApiKey firebaseAuthDomain
   in layoutPage homeLink loginLink "Sign in" $ do
        h1_ "Connecting to Firebase"
        p_ "Completing authentication flow with Firebase..."
        script_ [type_ "module"] (toHtmlRaw scriptBody)

layoutPage :: Link -> Link -> Text -> Html () -> Html ()
layoutPage homeNavLink loginNavLink titleText content =
  doctypehtml_ $
    html_ [lang_ "en"] $ do
      head_ $ do
        meta_ [charset_ "utf-8"]
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
        title_ (toHtml titleText)
        link_ [rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/water.css@2/out/water.css"]
      body_ $ do
        nav_ $ do
          a_ [href_ (linkToText homeNavLink)] "Home"
          a_ [href_ (linkToText loginNavLink)] "Sign in"
        main_ [id_ "content"] content
        footer_ $ p_ "Built with Servant and Lucid."

upsertFirebaseUser :: AppEnv -> FirebaseUser -> Bool -> UTCTime -> IO (Maybe DbUserRow)
upsertFirebaseUser env FirebaseUser {uid = subject, issuer = issuerValue, email = userEmail, name = displayName, picture = avatarUrl} allowed now =
  withAppConnection (dbConfig env) $ do
    result <-
      PQ.manipulateParams
        upsertUser
        ( issuerValue,
          subject,
          userEmail,
          displayName,
          avatarUrl,
          allowed,
          Just now,
          now
        )
    rows <- PQ.getRows result
    pure (listToMaybe rows)

recordFirebaseLogin :: AppEnv -> DbUserRow -> FirebaseUser -> Bool -> UTCTime -> IO ()
recordFirebaseLogin env DbUserRow {userId = dbUserId} firebaseUser@FirebaseUser {issuer = issuerValue} allowed now =
  withAppConnection (dbConfig env)
    $ PQ.manipulateParams_
      insertLoginEvent
      ( dbUserId,
        issuerValue,
        Jsonb (Aeson.toJSON firebaseUser),
        allowed,
        now
      )

forbiddenLogin :: FirebaseUser -> ServerError
forbiddenLogin FirebaseUser {uid = subject, issuer = issuerValue} =
  attachJson err403
    { errBody =
        encode
          ErrorResponse
            { error =
                ErrorBody
                  { code = "LOGIN_NOT_ALLOWED",
                    message = "User is not permitted to log in",
                    details =
                      object
                        [ "uid" .= subject,
                          "issuer" .= issuerValue
                        ]
                  }
            }
    }

attachJson :: ServerError -> ServerError
attachJson err =
  err {errHeaders = ("Content-Type", "application/json; charset=utf-8") : filter ((/= "Content-Type") . fst) (errHeaders err)}


renderProfilePage :: FirebaseAuth -> FirebaseUser -> DbUserRow -> Html ()
renderProfilePage _ user dbUser =
  layoutPage homeLink loginLink "Your profile" (profilePanel user dbUser)

profilePanel :: FirebaseUser -> DbUserRow -> Html ()
profilePanel FirebaseUser {uid = subject, issuer = issuerValue, email = emailValue, emailVerified = verified, name = displayName, picture = avatarUrl, audience = audiences, claims = customClaims} DbUserRow {userAllowed} =
  div_ [id_ "profile-panel"] $ do
    h2_ "Firebase profile"
    p_ "These attributes are decoded from your verified Firebase ID token."
    dl_ $ do
      textDetail "UID" subject
      textDetail "Issuer" issuerValue
      boolDetail "Allowed" userAllowed
      maybeTextDetail "Email" emailValue
      maybeBoolDetail "Email verified" verified
      maybeTextDetail "Display name" displayName
      maybeAvatarDetail avatarUrl
      audienceDetail audiences
      claimsDetail customClaims
    div_ [class_ "actions"] $
      a_ [href_ (linkToText protectedMeLink), class_ "button-secondary"] "Refresh profile"

textDetail :: Text -> Text -> Html ()
textDetail label value = do
  dt_ (toHtml label)
  dd_ (toHtml value)

maybeTextDetail :: Text -> Maybe Text -> Html ()
maybeTextDetail label = maybe mempty (textDetail label)

boolDetail :: Text -> Bool -> Html ()
boolDetail label = textDetail label . boolText

maybeBoolDetail :: Text -> Maybe Bool -> Html ()
maybeBoolDetail label = maybe mempty (boolDetail label)

maybeAvatarDetail :: Maybe Text -> Html ()
maybeAvatarDetail = maybe mempty $ \url ->
  dd_ $ img_ [src_ url, alt_ "Firebase user avatar"]

audienceDetail :: [Text] -> Html ()
audienceDetail audiences =
  case audiences of
    [] -> mempty
    xs -> textDetail "Audience" (Text.intercalate ", " xs)

claimsDetail :: HashMap.HashMap Text Value -> Html ()
claimsDetail claimsMap =
  textDetail "Custom claims" (Text.pack (show (HashMap.size claimsMap)))

resolveReturnTo :: Maybe Text -> Text
resolveReturnTo = maybe "/" sanitize
  where
    sanitize candidate =
      let trimmed = Text.takeWhile (/= '#') candidate
          target = if Text.null trimmed then "/" else trimmed
       in if Text.isPrefixOf "//" target || Text.head target /= '/'
            then "/"
            else target

boolText :: Bool -> Text
boolText = bool "No" "Yes"

linkToText :: Link -> Text
linkToText = Text.pack . show . linkURI

loginScript :: Text -> Text -> Text -> Text
loginScript projectId apiKey authDomain =
  let js :: Text -> Text
      js = decodeUtf8 . BL.toStrict . Aeson.encode
   in Text.unlines
        [ "import { initializeApp } from \"https://www.gstatic.com/firebasejs/10.12.0/firebase-app.js\";",
          "import { getAuth, signInWithRedirect, getRedirectResult, GoogleAuthProvider } from \"https://www.gstatic.com/firebasejs/10.12.0/firebase-auth.js\";",
          "",
          "const app = initializeApp({",
          "  apiKey: " <> js apiKey <> ",",
          "  authDomain: " <> js authDomain <> ",",
          "  projectId: " <> js projectId,
          "});",
          "const auth = getAuth(app);",
          "",
          "const result = await getRedirectResult(auth).catch(() => null);",
          "const user = result?.user || auth.currentUser;",
          "if (!user) {",
          "  await signInWithRedirect(auth, new GoogleAuthProvider());",
          "} else {",
          "  const idToken = await user.getIdToken(true);",
          "  const params = new URLSearchParams(window.location.search);",
          "  const returnTo = params.get(\"return_to\") || \"/\";",
          "  const response = await fetch(\"/session/exchange\", {",
          "    method: \"POST\",",
          "    headers: { \"Content-Type\": \"application/json\" },",
          "    body: JSON.stringify({ idToken, return_to: returnTo })",
          "  });",
          "  if (!response.ok) {",
          "    const detail = await response.json().catch(() => ({}));",
          "    const message = detail?.error?.message || \"Login failed\";",
          "    throw new Error(message);",
          "  }",
          "  const payload = await response.json();",
          "  window.location.replace(payload.redirect || \"/\");",
          "}",
          ""
        ]
