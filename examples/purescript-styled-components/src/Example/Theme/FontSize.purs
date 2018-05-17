module Example.Theme.FontSize where

import Prelude

import Style.Property (FontSizeValue)
import Style.Property.Value (px)

data FontSize
  = Twelve
  | Fourteen
  | Sixteen
  | Twenty

derive instance eqFontSize :: Eq FontSize

fontSize12 = Twelve :: FontSize
fontSize14 = Fourteen :: FontSize
fontSize16 = Sixteen :: FontSize
fontSize20 = Twenty :: FontSize

toValue :: FontSize -> FontSizeValue
toValue = case _ of
  Twelve -> 12.0 # px
  Fourteen -> 14.0 # px
  Sixteen -> 16.0 # px
  Twenty -> 20.0 # px
