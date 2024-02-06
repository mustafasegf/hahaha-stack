-- ConstraintKinds needed only for 7.8.4
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.Aeson (eitherDecode)
import Data.Maybe
  ( fromMaybe,
  )
import Data.Text (Text)
import Text.RawString.QQ
import Lucid
import Lucid.Base
import Lucid.Servant
import Network.HTTP.Simple (getResponseBody, httpLBS, parseRequest)
import qualified Network.Wai.Handler.Warp as Warp
import Servant
import Servant.HTML.Lucid
import System.Environment
  ( lookupEnv,
  )
import Text.Read
  ( readMaybe,
  )

type API =
  Get '[HTML] (Html ())
    :<|> "data" :> Get '[HTML] (Html ())

api :: Proxy API
api = Proxy

apiLink_ ::
  (IsElem endpoint API, HasLink endpoint) =>
  Proxy endpoint ->
  MkLink endpoint Attribute
apiLink_ = safeAbsHref_ (Proxy :: Proxy API)

server :: [String] -> Server API
server items = return root :<|> return html_data
  where
    root :: Html ()
    root =
      html_ $ do
        head_ $ do
          title_ "hello haskell"
          script_ [src_ "https://unpkg.com/htmx.org@1.9.10", type_ "text/javascript"] (mempty :: Html ())
          script_ [src_ "https://unpkg.com/hyperscript.org@0.9.12", type_ "text/javascript"] (mempty :: Html ())
        body_ $ do
          h1_ "Hello, Haskell!"
          p_ [hxget_ "/data", hxswap_ "innerHTML", hyperscript_ "on click set *background-color to `lightblue`"] "Click me !"

    html_data :: Html ()
    html_data = ul_ $ mapM_ (li_ [hyperscript_ [r| on click 
      js(me)
        if ('clipboard' in window.navigator) {
          window.navigator.clipboard.writeText(me.innerText)
        }
    |]] . toHtml) items

hxget_ :: Text -> Attribute
hxget_ = makeAttribute "hx-get"

hyperscript_ :: Text -> Attribute
hyperscript_ = makeAttribute "_"

hxswap_ :: Text -> Attribute
hxswap_ = makeAttribute "hx-swap"

getList :: IO [String]
getList = do
  request <- parseRequest "https://salin-abangku.vercel.app/api/all"
  response <- httpLBS request
  let body = getResponseBody response
  case eitherDecode body of
    Left _err -> return []
    Right xs -> return (xs :: [String])

htmlList :: [String] -> Html ()
htmlList xs = ul_ $ mapM_ (li_ . toHtml) xs

app :: [String] -> Application
app items = serve api (server items)

main :: IO ()
main = do
  port <- fmap (fromMaybe 8000 . (>>= readMaybe)) (lookupEnv "PORT")
  putStrLn $ "http://localhost:" ++ show port ++ "/"
  items <- getList
  Warp.run port (app items)
