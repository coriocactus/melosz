{-# LANGUAGE OverloadedStrings #-}

module Colour where

import qualified System.Random as Random
import qualified Data.Text as Text
import qualified Text.Printf as Printf
import qualified Numeric as Numeric
import qualified Data.Maybe as Maybe

-- Represents RGB color with values from 0.0 to 1.0
data RGB = RGB Double Double Double

-- Parses a hex color string (e.g., "#RRGGBB") to RGB
hexToRGB :: Text.Text -> Maybe RGB
hexToRGB hex =
  case Text.unpack (Text.drop 1 hex) of
    [r1, r2, g1, g2, b1, b2] -> do
      r <- hexPairToDouble [r1, r2]
      g <- hexPairToDouble [g1, g2]
      b <- hexPairToDouble [b1, b2]
      Just $ RGB (r / 255) (g / 255) (b / 255)
    _ -> Nothing
  where
    hexPairToDouble :: String -> Maybe Double
    hexPairToDouble s =
      fmap (fromIntegral . (fst :: (Int, String) -> Int)) $
        Maybe.listToMaybe (Numeric.readHex s :: [(Int, String)])

-- Calculates relative luminance based on WCAG formula
luminance :: RGB -> Double
luminance (RGB r g b) = 0.2126 * compand r + 0.7152 * compand g + 0.0722 * compand b
  where
    compand c = if c <= 0.03928 then c / 12.92 else ((c + 0.055) / 1.055) ** 2.4

-- Calculates contrast ratio between two colors
contrastRatio :: RGB -> RGB -> Double
contrastRatio c1 c2 = (max l1 l2 + 0.05) / (min l1 l2 + 0.05)
  where
    l1 = luminance c1
    l2 = luminance c2

-- Generates a random hex color string
generateRandomHexColor :: IO Text.Text
generateRandomHexColor = do
  r <- Random.randomRIO (0, 255 :: Int)
  g <- Random.randomRIO (0, 255 :: Int)
  b <- Random.randomRIO (0, 255 :: Int)
  return $ Text.pack $ Printf.printf "#%02x%02x%02x" r g b

-- Generates a pair of random hex colors meeting WCAG AA contrast (4.5:1)
generateAccessibleRandomColors :: IO (Text.Text, Text.Text)
generateAccessibleRandomColors = do
  bgColorHex <- generateRandomHexColor
  textColorHex <- generateRandomHexColor
  let maybeBgColor = hexToRGB bgColorHex
  let maybeTextColor = hexToRGB textColorHex
  case (maybeBgColor, maybeTextColor) of
    (Just bgColorRGB, Just textColorRGB) ->
      if contrastRatio bgColorRGB textColorRGB >= 4.5
        then return (bgColorHex, textColorHex)
        else generateAccessibleRandomColors -- Try again
    _ -> generateAccessibleRandomColors -- Should not happen with valid hex generation
