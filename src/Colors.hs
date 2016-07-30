{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Colors where

import Data.Monoid

colorFun c x = "\3" <> c <> x <> "\x0F"

bold = colorFun "\2"
