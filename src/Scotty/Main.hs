{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Network.HTTP.Types.Status
import Web.Scotty
--
-- import Data.Monoid (mconcat)
--we want to have a landing page that lets ppl enter their email address in order to record their interes.
christmsly :: ScottyM ()
christmsly = do
  get "/" showLandingPage
  post "/register" register

-- get and post both take a route as their first parameter, and an ActionM action to run if the route is matched. Get only matches if the HTTP request method is GET and post...you can guess.
--Now we need to implement the actions - we'll serve out the html file our designers provided us.

showLandingPage :: ActionM ()
showLandingPage = do
  setHeader "Content-Type" "text/html"
  file "landing.html"

--for the registration we need to do more. We need to be able to take a post request with an email address as a query paramter, and return a JSON document to signal whether or not the request was handled correctly.

register :: ActionM ()
register = do
  emailAddress <- param "email"
  registered <- liftIO (registerInterest emailAddress)
  case registered of
    Just errorMessage -> do
      json $ object [ "error" .= errorMessage ]
      status internalServerError500

    Nothing -> do
      json $ object ["ok" .= ("ok" :: String)]

--we use param to pull out the email parameter from the submission, and the try and call the registerInterest routine. which is an IO action
