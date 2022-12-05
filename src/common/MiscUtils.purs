module MiscUtils
  ( doAndIgnore
  , pass
  , unsafeJust
  )
  where

import Data.Maybe (Maybe, fromJust)
import Partial.Unsafe (unsafePartial)

doAndIgnore :: forall a b. a -> b -> b
doAndIgnore _ b = b

pass :: forall a. a -> a
pass a = a

unsafeJust :: forall a. Maybe a -> a
unsafeJust maybeA = unsafePartial (fromJust maybeA)
