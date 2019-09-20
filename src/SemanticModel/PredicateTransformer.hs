module SemanticModel.PredicateTransformer
where

import GHC.Natural
import SimpleSMT ( SExpr )
import Data.Text.Prettyprint.Doc


class WeakestPreVerification lang where
  data LogicExp lang :: *
  data BasicBlock lang :: *

  wp :: lang -> LogicExp lang

  toVerificationCondition :: LogicExp lang -> SExpr

  toBasicBlocks :: lang -> [BasicBlock lang]

  getPre :: BasicBlock lang -> LogicExp lang

  getCode :: BasicBlock lang -> lang

  getPost :: BasicBlock lang -> LogicExp lang

  calculateVC :: BasicBlock lang -> LogicExp lang -> LogicExp lang
  
  displayUnsat
    :: ( Pretty lang
       , Pretty (LogicExp lang)
       )
    => BasicBlock lang  -- ^ refuted basic block
    -> LogicExp lang -- ^ Candidate invariant that is unsatisfiable
    -> IO ()
  displayUnsat block vc =
    putStrLn $ show
        $ "Invalid Verification Condition:" <> line 
        <> nest 4 (pretty vc)
        <> nest (-4) "While checking :" <> line
        <> "@Pre:" <+> pretty (getPre block)
        <> pretty (getCode block)
        <> "@Post:" <+> pretty (getPost block)
        
