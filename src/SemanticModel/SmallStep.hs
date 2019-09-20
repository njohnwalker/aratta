module SemanticModel.SmallStep
  ( Transition, o, nStep
  , manyStep
  , stepRun
  , isLiteral, isNotLiteral, rules
  , Trace(..), runTrace
  , get, put, modify, runStateT, gets, mzero, mplus
  , state
  )
where

import Data.Hashable ( Hashable, hash )
import Control.Monad.State.Lazy
import Data.List.Extra ( nubOrdOn )


-- | Trace monad is a monad stack of a List of States
type Trace state = StateT state []

-- | Monad runner, arguments flipped, for convenience (confusion).
runTrace :: config -> Trace config syntax -> [(syntax, config)]
runTrace = flip runStateT

{- | A Transition represents a syntax term together with some sort of state
and rules to transition between states and terms.
-}
class (Eq config, Hashable config, Hashable syntax)
  => Transition config syntax | syntax -> config where

  -- | Predicate to determine a syntax term in normal form.
  isLiteral :: syntax -> Bool

  -- | isNotliteral == not . isLiteral
  isNotLiteral :: syntax -> Bool
  isNotLiteral = not . isLiteral

  -- | A list of potential transition rules for a single step.
  rules :: [syntax -> Trace config syntax]

  -- | run a single transition on a syntactic term.
  o :: syntax -> Trace config syntax
  o = buildStep rules

  -- | Run bounded number of steps
  nStep :: Int -> syntax -> Trace config syntax
  nStep n syntax
    | n >= 0 = foldl ( >>= ) (return syntax) $ replicate n o
  nStep _ syntax = error "nStep: Integer argument must be positive."

  -- | run and return each step in a full execution
  allStep :: syntax -> [Trace config syntax]
  allStep stmt = map (flip nStep stmt) [0..]  

  -- Helper function builds an applicative list of transition rules
  buildStep
    :: [syntax -> Trace config syntax]
    ->  syntax -> Trace config syntax
  buildStep rules syntax
    = foldl (\l r -> l `mplus` r syntax) mzero rules

  -- | Unpack the full trace of a program from it's
  --   monadic context and filter out duplicate results .
  stepRun :: config -> syntax -> [[(syntax, config)]]
  stepRun config syntax
    = takeWhile (not . null) $ [ nubOrdOn hash
                               $ runTrace config
                               $ flip nStep syntax n
                               | n <- [0..]
                               ]

  -- | run a program to completion, filtering out duplicate and
  --   terminated states with each iteration.
  manyStep :: Trace config syntax -> Trace config syntax
  manyStep trace = results `mplus` (toRecurse >>= (\_ -> manyStep toRecurse))
    where results = mfilter isLiteral trace
          intermediate = (mfilter isNotLiteral trace) >>= o
          toRecurse = rebuildState $ nubOrdOn hash $ runTrace undefined intermediate
          rebuildState :: [(syntax, config)] -> Trace config syntax
          rebuildState = foldl (\l (syntax,state) -> l `mplus` (put state >> return syntax)) mzero
