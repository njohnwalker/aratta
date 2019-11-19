module SMTLib.Environment
where

import qualified Data.Map as Map

import qualified SimpleSMT as SMT

type Env = [(String, SMT.Value)]
