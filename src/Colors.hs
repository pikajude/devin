{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Colors where

import Data.Monoid

styleFun c d x = c <> x <> d

colorFun b = styleFun ("\3" <> b) "\3"

bold = styleFun "\2" "\2"

blue  = colorFun "02"
green = colorFun "03"
red   = colorFun "04"
yellow = colorFun "08"
