module SemanticModel.PredicateTransformer.InvariantLattice
where

import Control.Monad.Reader
import Control.Monad.State

import           Data.Hashable
import qualified Data.HashMap.Lazy as Map
import qualified Data.HashSet as Set

import Language.GCL ( BExp((:&:)) )

type Invariant = Set.HashSet Int 

type InvariantMap inv = Map.HashMap Int inv

type LazyLattice = Map.HashMap Invariant (Set.HashSet Invariant)

-- | Calculate the initial state for concurrent solving system
initialLattice
  :: [inv]
  -> (InvariantMap inv, LazyLattice, [Invariant])
initialLattice invs =
  let invMap = Map.fromList $ zip [1..] invs

      idxList = zipWith const [1..] invs

      twoElementSubsets = [ Set.fromList [x,y] | x <- idxList, y <- idxList, x /= y ]

      initialLazyLattice = Map.fromList $ zipWith
        (\l r -> (l, Set.map Set.singleton r))
        twoElementSubsets
        twoElementSubsets

      initialInvariants = zipWith (const . Set.singleton) [1..] invs
      
  in (invMap,  initialLazyLattice, initialInvariants)

-- | Calculate the resulting lattice, given a newly invalidated invariant
updateLattice
  :: [inv]
  -> Invariant
  -> LazyLattice
  -> ([Invariant], LazyLattice)
updateLattice invs invKey lattice =  
  let newInvariants = [ let invKey' = Set.insert i invKey
                        in (invKey', nMinusOneSubsets invKey')
                      | i <- zipWith const [1..] invs
                      , not (i `Set.member` invKey)
                      ]

      -- update lattice with newly reachable invariants and
      -- remove the invalidated invariant from the lesser elements of each
      -- reachable invariant 
      lattice' = foldr addNewInvariant lattice newInvariants
        where addNewInvariant (invariant, implicands)
                = Map.insertWith
                  (\_new old -> Set.delete invKey old)
                  invariant
                  implicands

      -- extract all invariants whose lesser elements have been invalidated 
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
