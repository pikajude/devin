{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Colors where

import Data.Monoid
import Data.String

styleFun :: Monoid m => m -> m -> m -> m
styleFun c d x = c <> x <> d

colorFun :: (Monoid m, IsString m) => m -> m -> m
colorFun b = styleFun ("\3" <> b) "\3"

bold, blue, green, red, yellow :: (Monoid m, IsString m) => m -> m
bold = styleFun "\2" "\2"

blue  = colorFun "02"
green = colorFun "03"
red   = colorFun "04"
yellow = colorFun "08"
