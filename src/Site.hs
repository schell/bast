{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent     ( forkIO )
import           Control.Concurrent.MVar
import           Control.Monad.IO.Class ( liftIO )
import           Data.Maybe
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Snaplet.AcidState ( Update, Query, Acid, HasAcid (getAcidStore), makeAcidic, update, query, acidInit )
import           Snap.Util.FileServe
import           Snap.Util.FileUploads
import           System.Posix           ( rename, fileSize, getFileStatus )
import           System.FilePath
import qualified Data.Text              as T
import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as C
import qualified Heist.Interpreted      as I
import qualified Data.Map               as M
------------------------------------------------------------------------------
import qualified Aws                    as Aws
import qualified Aws.S3                 as S3
import           Data.Conduit           ( ($$+-) )
import           Data.Conduit.Binary    ( sinkFile, sinkLbs )
import           Network.HTTP.Conduit   ( withManager, responseBody )
------------------------------------------------------------------------------
import           Application

-- | Stuff for TDP --
------------------------------------------------------------------------------
data Hole = Hole
hole = undefined

------------------------------------------------------------------------------
data Upload = Upload
    { _filename   :: String
    , _location   :: FilePath
    , _localId    :: String
    , _bytesTotal :: Int
    , _bytesDone  :: Int
    , _status     :: String
    } deriving (Show, Eq)

type Uploads = M.Map String Upload

type UploadState = MVar Uploads

------------------------------------------------------------------------------
-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = [("loginError", I.textSplice c) | c <- maybeToList authError]


------------------------------------------------------------------------------
-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
    loginUser "login" "password" Nothing
              (\_ -> handleLogin err) (redirect "/")
  where
    err = Just "Unknown user or password"


------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"


------------------------------------------------------------------------------
-- | Handle new user form submit
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "new_user"
    handleFormSubmit = registerUser "login" "password" >> redirect "/"


------------------------------------------------------------------------------
-- | When a user wants to upload.
handleUpload :: UploadState -> Handler App (AuthManager App) ()
handleUpload psVar = method GET handleGetUpload <|> method POST handlePostUpload
    where policy           = defaultUploadPolicy
          processor        = undefined
          handleGetUpload  = render "upload"
          tmp              = "uploads_tmp"
          partPolicy _     = allowWithMaximumSize $ floor (1.074e9 :: Double)
          userHdl          = handleUploadedFiles psVar
          handlePostUpload = handleFileUploads tmp defaultUploadPolicy partPolicy userHdl
--          handleStreamUpload = do _ <- handleMultipart policy processor
--                                  return ()

------------------------------------------------------------------------------
-- | After a file has been uploaded.
handleUploadedFiles :: UploadState -> [(PartInfo, Either PolicyViolationException FilePath)] -> Handler App (AuthManager App) ()
handleUploadedFiles _ [] = heistLocal (I.bindSplices msgs) $ render "uploaded"
    where msgs = [("msg", I.textSplice "You must specify a file to upload.")]

handleUploadedFiles _ [(_, Left e)] = heistLocal (I.bindSplices msgs) $ render "uploaded"
    where msgs = [("msg", I.textSplice $ policyViolationExceptionReason e)]

handleUploadedFiles psVar [(PartInfo{..}, Right f)] =
    do len <- liftIO $ fmap fileSize $ getFileStatus f
       if len > 0 && isJust partFileName
         then do let location = replaceDirectory f "uploads"
                     msgs     = [ ("msg",  I.textSplice "Uploading...")
                                , ("link", I.textSplice $ T.pack f)
                                ]
                     name     = C.unpack $ fromJust partFileName
                     upload   = Upload { _filename   = name
                                       , _location   = location
                                       , _localId    = name -- For now, eventually BTC addy
                                       , _bytesTotal = fromIntegral len
                                       , _bytesDone  = 0
                                       , _status     = "Starting upload..."
                                       }
                 liftIO $ rename f location
                 _ <- liftIO $ forkIO $ uploadAWS psVar upload
                 heistLocal (I.bindSplices msgs) $ render "uploaded"

         else do let msgs = [("msg", I.textSplice "You must specify a file to upload.")]
                 heistLocal (I.bindSplices msgs) $ render "uploaded"

handleUploadedFiles _ _ = heistLocal (I.bindSplices msgs) $ render "uploaded"
    where msgs = [("msg", I.textSplice "An unknown error occurred")]

------------------------------------------------------------------------------
-- | Uploads our file to S3.
-- https://github.com/aristidb/aws
uploadAWS :: UploadState -> Upload -> IO ()
uploadAWS upsVar up = do
    let name = _localId up
    ups <- takeMVar upsVar
    putMVar upsVar (M.insert name up ups)
    -- Set up some creds with a default config.
    cfg <- Aws.baseConfiguration
    let s3cfg  = Aws.defServiceConfig :: S3.S3Configuration Aws.NormalQuery
        bucket = T.pack name
    -- Set up a resource region with an available http manager.
    withManager $ \mgr -> do
        -- Create a request object with S3.getObject and runt the request.
        gbr <- Aws.pureAws cfg s3cfg mgr $ S3.getBucket bucket
        -- r   <- gbr $$+- sinkLbs
        let f x = x { _status = show gbr }
        ups' <- liftIO $ readMVar upsVar
        _ <- liftIO $ swapMVar upsVar $ M.adjust f name ups'
        return ()


------------------------------------------------------------------------------
-- | Get the status of an upload by key.
getStatus :: UploadState -> Maybe String -> IO String
getStatus upsVar mk = do
    ups  <- liftIO $ readMVar upsVar
    return $ maybe "You must specify an upload key." (f ups) mk
        where f ups name = maybe
                             ("The upload " ++ name ++ " is either complete or does not exist.")
                             _status
                             (M.lookup name ups)


------------------------------------------------------------------------------
-- | Check the status of an upload.
handleStatus :: UploadState -> Handler App (AuthManager App) ()
handleStatus upsVar = do
    param  <- (C.unpack <$>) <$> getParam "key"
    status <- liftIO $ getStatus upsVar param
    heistLocal (I.bindSplices [("status", I.textSplice $ T.pack status)]) $ render "status"

------------------------------------------------------------------------------
-- | Check the status of all uploads.
handleStatusAll :: UploadState -> Handler App (AuthManager App) ()
handleStatusAll upsVar = do
    ups <- liftIO $ readMVar upsVar
    let status = T.pack $ show ups
    heistLocal (I.bindSplices [("status", I.textSplice $ status)]) $ render "status"


------------------------------------------------------------------------------
-- | The application's routes.
routes :: UploadState -> [(B.ByteString, Handler App App ())]
routes upsVar = [ ("/login",    with auth handleLoginSubmit)
                , ("/logout",   with auth handleLogout)
                , ("/new_user", with auth handleNewUser)
                , ("/upload",   with auth $ handleUpload upsVar)
                , ("/status/:key", with auth $ handleStatus upsVar)
                , ("/status-all", with auth $ handleStatusAll upsVar)
                , ("",          serveDirectory "static")
                ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    -- NOTE: We're using initJsonFileAuthManager here because it's easy and
    -- doesn't require any kind of database server to run.  In practice,
    -- you'll probably want to change this to a more robust auth backend.
    a <- nestSnaplet "auth" auth $
           initJsonFileAuthManager defAuthSettings sess "users.json"
    ps <- liftIO $ newMVar M.empty
    addRoutes $ routes ps
    addAuthSplices h auth
    return $ App h s a

