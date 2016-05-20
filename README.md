# Happstack-session
Serverside sessions for Happstack.

See hackage for documentation.

### Example
    {-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

    module Main where

    import Control.Applicative ((<$>), (<|>), optional)
    import Data.Maybe
    import Data.Text (Text)
    import Data.Text.Lazy (unpack)
    import Happstack.Lite hiding (Session)
    import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
    import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value, class_)
    import qualified Text.Blaze.Html5 as H
    import qualified Text.Blaze.Html5.Attributes as A

    import Happstack.Server.Session
    import Happstack.Server.Session.Memory

    data MyData = MyData Text
                  deriving (Eq, Show)


    main :: IO ()
    main = do
        sessionHandler <- startSession (mkSessionConfig "F4FCD1AA73DE4A31135668B4F2428AC3" "98EDE03AB48FC1F8BECA84D5F98A12F2") memoryStartSession
        serve Nothing $ myServer sessionHandler

    template :: Text -> Html -> Response
    template title body = toResponse $ do
        H.docType
        H.html ! A.lang "no" $ do
            H.head $  do
                H.title (toHtml title)
                H.meta ! A.charset "UTF-8"
            H.body $ do
                body

    myServer :: SessionHandler Int MyData -> ServerPart Response
    myServer sessionHandler = msum [
            nullDir >> homepage,
            dir "setSession" $ setS sessionHandler,
            dir "getSession" $ getS sessionHandler,
            dir "delSession" $ delS sessionHandler,
            errorPage
        ]


    homepage :: ServerPart Response
    homepage = ok $ template "test" $ do
        H.h1 "Test"
        a ! href "/setSession" $ "Set session"
        H.br
        a ! href "/getSession" $ "Get session"
        H.br
        a ! href "/delSession" $ "Delete session"

    setS sessionHandler = do
        setSession sessionHandler (MyData "Birk") 99999
        ok $ template "test" $ do
            H.h1 "Session is set"
            a ! href "/" $ "Home"
            H.br
            a ! href "/getSession" $ "Get session"
            H.br
            a ! href "/delSession" $ "Delete session"

    getS sessionHandler = do
        session <- getSession sessionHandler
        ok $ template "test" $ do
            H.h1 "Session is: "
            H.h2 $ toHtml $ show session
            a ! href "/" $ "Home"
            H.br
            a ! href "/setSession" $ "Set session"
            H.br
            a ! href "/delSession" $ "Delete session"

    delS sessionHandler = do
        deleteSession sessionHandler
        ok $ template "test" $ do
            H.h1 "Session is deleted"
            a ! href "/" $ "Home"
            H.br
            a ! href "/setSession" $ "Set session"
            H.br
            a ! href "/getSession" $ "get session"


    errorPage ::  ServerPart Response
    errorPage = notFound $ template "404" $ do
        H.h1 "404"
