{- * Module Meant to be imported qualified as GCL.Gen

-}

module Test.Language.GCL.Gen
where

import Hedgehog ( Gen )
import qualified Hedgehog as Hog hiding ( Var, map )
import qualified Hedgehog.Gen as Gen hiding ( map )
import qualified Hedgehog.Range as Range

import Data.Text (Text,  pack, isPrefixOf )

import Language.GCL

bool :: Gen BExp
bool = Gen.choice
  [ BConst <$> Gen.bool_
  , Gen.scale reduceSize $  (:&:) <$> bool <*> bool
  , Gen.scale reduceSize $ (:<=:) <$> int  <*> int
  , Gen.scale reduceSize $   Not  <$> bool
  ]

int :: Gen IExp
int = Gen.choice
  [ fmap IConst $ Gen.integral $ Range.constantFrom (-100) 100 0
  , Var <$> variable
  , Gen.scale reduceSize $ (:-:) <$> int <*> int
  ]

reduceSize :: Hog.Size -> Hog.Size
reduceSize (Hog.Size n) | n > 0 = Hog.Size $ n - 1
                        | otherwise = Hog.Size n

variable :: Gen Text
variable = do
  word <- Gen.text (Range.linear 3 10) Gen.lower
  if isRword word then variable else return word
  where isRword word = and [isPrefixOf rword word  | rword <- rwords]
