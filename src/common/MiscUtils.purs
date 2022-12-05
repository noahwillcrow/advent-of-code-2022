module MiscUtils
  ( doAndIgnore
  , pass
  )
  where

doAndIgnore :: forall a b. a -> b -> b
doAndIgnore _ b = b

pass :: forall a. a -> a
pass a = a