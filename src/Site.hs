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
import qualified Data.Text             as T
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as C
import qualified Heist.Interpreted     as I
------------------------------------------------------------------------------
import           Application

-- Stuff for TDP --
data Hole = Hole
hole = undefined
-------------------

------------------------------------------------------------------------------
data Upload = Upload
    { _filename   :: String
    , _location   :: FilePath
    , _bytesTotal :: Int
    , _bytesDone  :: Int
    }

newtype Uploads = Uploads [Upload]

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
                     upload   = Upload { _filename   = C.unpack $ fromJust partFileName
                                       , _location   = location
                                       , _bytesTotal = fromIntegral len
                                       , _bytesDone  = 0
                                       }
                 liftIO $ putStrLn $ f ++ " -> " ++ location
                 liftIO $ rename f location
                 _ <- liftIO $ forkIO $ uploadAWS psVar upload
                 heistLocal (I.bindSplices msgs) $ render "uploaded"

         else do let msgs = [("msg", I.textSplice "You must specify a file to upload.")]
                 heistLocal (I.bindSplices msgs) $ render "uploaded"

handleUploadedFiles _ _ = heistLocal (I.bindSplices msgs) $ render "uploaded"
    where msgs = [("msg", I.textSplice "An unknown error occurred")]
--    where handleUploadedFile _ xs (_, Left e) =
--              return $ xs ++ [encodeUtf8 $ policyViolationExceptionReason e]
--          handleUploadedFile ps xs (p, Right f) = do
--              -- Create a new url to get upload updates from...
--              uplds <- takeMVar ps
--              let uplds' = PersistentState $ upld:_uploads uplds
--                  upld   = AWSUpload { _filename  = f
--                                     , _bytesDone = 0
--                                     , _bytesLeft = 0
--                                     }
--              putMVar psVar uplds'
--              liftIO $ print p
--              return $ xs ++ [encodeUtf8 $ T.append "File uploaded " $ T.pack f]
--          msgs = foldM (handleUploadedFile psVar) []

uploadAWS _ _ = undefined

------------------------------------------------------------------------------
-- | The application's routes.
routes :: UploadState -> [(B.ByteString, Handler App App ())]
routes psVar = [ ("/login",    with auth handleLoginSubmit)
               , ("/logout",   with auth handleLogout)
               , ("/new_user", with auth handleNewUser)
               , ("/upload",   with auth $ handleUpload psVar)
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
    ps <- liftIO $ newMVar $ Uploads []
    addRoutes $ routes ps
    addAuthSplices h auth
    return $ App h s a

