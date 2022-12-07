module PartialListProcessingResult
  ( PartialListProcessingResult
  , createPartialListProcessingResult
  )
  where

import Data.List.Lazy (List)

type PartialListProcessingResult a b = { remainingList :: List a, result :: b }

createPartialListProcessingResult :: forall a b. List a -> b -> PartialListProcessingResult a b
createPartialListProcessingResult remainingList result = { remainingList: remainingList, result: result }
