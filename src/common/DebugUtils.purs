module DebugUtils
  ( traceReturnValue
  )
  where

import Debug (trace)

traceReturnValue :: forall a. a -> a
traceReturnValue value = trace value \_ -> value
