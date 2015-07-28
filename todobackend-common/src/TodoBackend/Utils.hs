{-# LANGUAGE OverloadedStrings #-}
module TodoBackend.Utils where

import Network.Wai (Middleware)
import Network.Wai.Middleware.AddHeaders

allowCors :: Middleware
allowCors = addHeaders [
    ("Access-Control-Allow-Origin", "*"),
    ("Access-Control-Allow-Headers", "Accept, Content-Type"),
    ("Access-Control-Allow-Methods", "GET, HEAD, POST, DELETE, OPTIONS, PUT, PATCH")
  ]
