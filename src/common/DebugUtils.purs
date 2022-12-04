module DebugUtils
  ( traceReturnValue
  )
  where

traceReturnValue :: forall a. a -> a
traceReturnValue value = value -- trace value \_ -> value
