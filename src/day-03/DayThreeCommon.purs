module DayThreeCommon
  ( scoreItem
  )
  where

import Prelude

import Data.Char (toCharCode)

scoreItem :: Char -> Int
scoreItem char = 
  if
    char >= 'a' && char <= 'z'
  then
    (toCharCode char) - (toCharCode 'a') + 1
  else
    (toCharCode char) - (toCharCode 'A') + 27
    