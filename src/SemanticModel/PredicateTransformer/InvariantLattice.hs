module SemanticModel.PredicateTransformer.InvariantLattice
where

import Control.Monad.Reader
import Control.Monad.State

import           Data.Hashable
import qualified Data.HashMap.Lazy as Map
import qualified Data.HashSet as Set

import Language.GCL ( BExp((:&:)) )

type Invariant = Set.HashSet Int 

type LazyLattice = Map.HashMap Invariant (Set.HashSet Invariant)

data FactoryEnv inv = FactoryEnv
  { invariantList :: [inv]
  , invariantMap :: Map.HashMap Int inv
  }

type MonadFactory inv
  = MonadReader (FactoryEnv inv)

initialLattice
  :: [inv] -> (FactoryEnv inv, LazyLattice, [(Invariant, inv)])
initialLattice invs =
  let invMap = Map.fromList $ zip [1..] invs
      idxList = zipWith const [1..] invs
      twoElementSubsets = [Set.fromList [x,y] | x <- idxList, y <- idxList, x /= y]
      initialLazyLattice = Map.fromList $ zip
        twoElementSubsets
        $ map (Set.map Set.singleton) twoElementSubsets
      initialPairs = map Set.singleton [1..] `zip` invs
  in (FactoryEnv invs invMap, initialLazyLattice, initialPairs)

updateLattice
  :: [Int]
  -> Invariant
  -> LazyLattice
  -> ([Invariant], LazyLattice)
updateLattice invariantIdxs invKey lattice =  
  let newInvariants = [ let invKey' = Set.insert i invKey
                        in (invKey', nMinusOneSubsets invKey')
                      | i <- invariantIdxs
                      , not (i `Set.member` invKey)
                      ]

      lattice' = foldr addNewInvariant lattice newInvariants
        where
          addNewInvariant (invariant, implicands) =
            Map.insertWith (\_new old -> Set.delete invKey old) invariant implicands

      nextInvariants = map fst $ filter (Set.null . snd) $ Map.toList lattice'  

      lattice'' = Map.filter (not . Set.null) lattice' 

  in (nextInvariants, lattice'')


-- | calculate the maximal proper subsets of an Invariant set
nMinusOneSubsets :: Invariant -> Set.HashSet Invariant
nMinusOneSubsets invKey = Set.fromList [Set.delete i invKey | i <- Set.toList invKey]

-- | calculate the actual invariant from the invariant key set
getInvariant :: Invariant -> Map.HashMap Int BExp -> BExp
getInvariant invKey invMap = foldl1 (:&:)
  [inv | (flip Map.lookup invMap -> Just inv) <- Set.toList invKey]

-- | get the maximal invariant of the lattice (not Top)
maxInvariant :: [inv] -> Invariant
maxInvariant = Set.fromList . zipWith const [1..]
