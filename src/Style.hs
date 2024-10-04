{-# LANGUAGE OverloadedStrings #-}

module Style (invenGridStyle) where

import qualified Data.Text as T

invenGridStyle :: T.Text
invenGridStyle =
  "\
  \  body {\
  \    font-family: Arial, sans-serif;\
  \    margin: 0;\
  \    padding: 0;\
  \    background-color: #f5f5f5;\
  \  }\
  \  h1 {\
  \    text-align: center;\
  \    padding: 20px;\
  \  }\
  \  .grid-container {\
  \    display: grid;\
  \    grid-template-columns: repeat(auto-fill, minmax(250px, 1fr));\
  \    gap: 20px;\
  \    padding: 20px;\
  \  }\
  \  .grid-item {\
  \    background-color: #fff;\
  \    border: 1px solid #ddd;\
  \    padding: 15px;\
  \    transition: transform 0.4s;\
  \  }\
  \  .grid-item:hover {\
  \    transform: scale(1.02);\
  \  }\
  \  .item-image {\
  \    width: 100%;\
  \    height: auto;\
  \    border-bottom: 1px solid #ddd;\
  \    margin-bottom: 10px;\
  \  }"
