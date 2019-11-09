module Language.GCL.Syntax.Abstract
  ( module Language.GCL.Syntax.Abstract
  , pretty
  )
where

import qualified Control.Monad as M ( guard )

import Control.Lens

import Data.Char
import Data.Data
import Data.GenValidity
import Data.GenValidity.Text
import Data.Hashable
import Data.String
import Data.Text ( Text, pack, unpack )
import Data.Text.Prettyprint.Doc.Render.Text ( renderStrict )
import Data.Text.Prettyprint.Doc

import GHC.Generics hiding ( (:*:) )

import qualified Test.QuickCheck as QC

{-|

block ::=
  @req(bexp)
  statement
  @ens(bexp)

statement ::= statement {; statement}
            | if gcs fi
            | do @inv(bexp) gcs od
            | var := iexp

gcs ::= gc {[] gc}

gc ::= bexp -> statement

bexp ::= const
       | bexp :&: bexp
       | Not bexp
       | iexp :<=: iexp

iexp ::= var | const
       | iexp :-: iexp

-}

data GCLProgram = GCLProgram
  { req :: Maybe BExp
  , program :: [Statement]
  , ens :: Maybe BExp
  } deriving (Eq, Ord, Show, Generic, Data)

instance Hashable GCLProgram

data Statement
  = If GuardedCommandSet
  | Do (Maybe BExp) GuardedCommandSet
  | [Variable] := [IExp]
  deriving (Eq, Ord, Show, Generic, Data)

instance Hashable Statement

instance Plated Statement

newtype GuardedCommandSet =
  GCS { getCommandList :: [GuardedCommand] }
  deriving (Eq, Ord, Show, Generic, Data)

instance Hashable GuardedCommandSet

data GuardedCommand = GC
  { guard :: BExp
  , command :: [Statement]
  }
  deriving (Eq, Ord, Show, Generic, Data)

instance Hashable GuardedCommand

data BExp
  = BConst Bool
  | Not BExp
  -- * Logical Connectives
  | BExp :&: BExp
  | BExp :|: BExp
  | BExp :=>: BExp
  -- * Relations
  | IExp :<: IExp
  | IExp :>: IExp
  | IExp :<=: IExp
  | IExp :>=: IExp
  | IExp :==: IExp
  deriving (Eq, Ord, Show, Generic, Data)

infixr 2 :=>:, :|:
infixr 3 :&:
infix 4 :<=:, :>:, :<:, :>=:, :==:

newtype Variable = Variable { getVariableText :: Text }
  deriving (Eq, Ord, Show, Generic, Data)

instance IsString Variable where
  fromString = Variable . pack

instance Hashable Variable
pattern Var_ var = Var (Variable var) 

rwords :: [Variable]
rwords = [ "if", "fi", "do", "od", "true", "false"
         -- probable (short) SMTLIB reserved words
         -- anything that fails smt parse in generated tests
         , "and"
         ]

data IExp
  = Var Variable -- TODO: support existentials in verification annotations
  | IConst Integer
  | IExp :+: IExp
  | IExp :-: IExp
  | IExp :*: IExp
  | IExp :/: IExp
  | IExp :%: IExp
  deriving (Eq, Ord, Show, Generic, Data)
infixl 6 :-:, :+:
infixl 7 :*:, :/:, :%:

makeClassy_ ''GCLProgram
makeClassy_ ''GuardedCommand
makeClassy_ ''GuardedCommandSet

makePrisms ''IExp
makePrisms ''BExp
makePrisms ''Statement

instance Num IExp where
  i + j = i :+: j
  i * j = i :*: j
  i - j = i :-: j

  abs (IConst i) = IConst $ abs i
  abs _ = error "abs: not defined for expressions"

  signum (IConst i) = IConst $ signum i
  signum _ = error "signum: not defined for expressions"

  fromInteger = IConst

instance Plated BExp
instance Plated IExp

deriving instance Hashable BExp
deriving instance Hashable IExp

----------------------------------
-- Pretty-printing GCL programs --
-- | Render GCL AST to Text
renderPretty :: Pretty a => a -> Text
renderPretty = renderStrict . layoutSmart defaultGCLLayoutOptions . pretty
  where
    defaultGCLLayoutOptions = LayoutOptions $ AvailablePerLine 60 1.0

instance Pretty GCLProgram where
  pretty GCLProgram {req, program, ens}
    =  "@req(" <> pretty req <> ")" <> line
    <> pretty program <> line
    <> "@ens(" <> pretty ens <> ")" <> line

instance Pretty Statement where
  prettyList = vsep . map pretty
  pretty = \case
    var := exp
      -> (encloseSep emptyDoc emptyDoc (comma <> space) $ map pretty var)
      <+> ":="
      <+> (encloseSep emptyDoc emptyDoc (comma <> space) $ map pretty exp)
      <+> semi

    If (GCS []) -> "if fi"
    If (GCS (gc:gcs))
      -> "if" <+> (pretty gc) <> line
      <>  case gcs of
            [] -> mempty
            _ -> prettyList gcs <> line
      <> "fi"

    Do inv (GCS []) -> "do" <+> (prettyAnn "@inv" inv) <+> "od"
    Do inv (GCS gcs)
      -> "do" <+> prettyAnn "@inv" inv <> line
      <> prettyList gcs <> line
      <> "od"

instance Pretty GuardedCommand where
  pretty GC { guard, command }
    = align (pretty guard <+> "->" <> nest 3 (line <> pretty command))
  prettyList = vsep . map (("[]"<+>) .  pretty)
  
instance Pretty BExp where
  pretty = \case
    BConst True -> "true"
    BConst False -> "false"
    Not bexp -> "~" <+> smartParensBExp bexp
    e1 :|: e2  -> prettyBoolBinop "|" e1 e2
    e1 :&: e2  -> prettyBoolBinop "&" e1 e2
    e1 :=>: e2 -> prettyBoolBinop "=>" e1 e2    
    e1 :>: e2  -> prettyRelBinop ">" e1 e2
    e1 :<: e2  -> prettyRelBinop "<" e1 e2
    e1 :<=: e2 -> prettyRelBinop "<=" e1 e2
    e1 :>=: e2 -> prettyRelBinop ">=" e1 e2
    e1 :==: e2 -> prettyRelBinop "==" e1 e2

instance Pretty IExp where
  pretty = \case
    Var txt -> pretty txt
    IConst int -> pretty int
    e1 :+: e2 -> prettyIntBinop "+" e1 e2
    e1 :-: e2 -> prettyIntBinop "-" e1 e2
    e1 :*: e2 -> prettyIntBinop "*" e1 e2
    e1 :%: e2 -> prettyIntBinop "%" e1 e2
    e1 :/: e2 -> prettyIntBinop "/" e1 e2

instance Pretty Variable where
  pretty (Variable var) = pretty var

smartParensBExp = \case
  BConst b -> pretty $ BConst b
  n@(Not _) -> pretty n
  r@(_:==:_) -> pretty r
  r@(_:>=:_) -> pretty r
  r@(_:<=:_) -> pretty r
  r@(_:>:_) -> pretty r
  r@(_:<:_) -> pretty r
  bexp -> parens $ pretty bexp

smartParensIExp = \case
  Var var -> pretty var
  IConst i -> pretty i
  iexp -> parens $ pretty iexp

prettyAnn tag Nothing = tag <> parens mempty
prettyAnn tag (Just bexp) = tag <> (parens $ pretty bexp)

prettyIntBinop :: Doc ann -> IExp -> IExp -> Doc ann
prettyIntBinop op e1 e2 =
  smartParensIExp e1 <> softline <> op <> softline <> smartParensIExp e2

prettyRelBinop :: Doc ann -> IExp -> IExp -> Doc ann
prettyRelBinop op e1 e2 =
  pretty e1 <> softline <> op <> softline <> pretty e2

prettyBoolBinop :: Doc ann -> BExp -> BExp -> Doc ann
prettyBoolBinop op e1 e2 =
  smartParensBExp e1 <> softline <> op <> softline <> smartParensBExp e2

---------------------------------------------
-- Generators For GCL Constructs and Tests --

instance Validity BExp

instance QC.Arbitrary BExp where
  arbitrary = genValid
  shrink = shrinkValid

instance GenValid BExp where
  genValid = do
    size <- QC.getSize
    if size <= 0 then BConst <$> genValid
      else QC.scale (subtract 1)
          let size' = min size 1
          in QC.frequency
             [ (    10, BConst <$> genValid )
             , ( size',  Not   <$> genValid )
             , ( size', (:&:)  <$> genValid <*> genValid )
             , ( size', (:|:)  <$> genValid <*> genValid )
             , ( size', (:=>:) <$> genValid <*> genValid )
             , ( size', (:==:) <$> genValid <*> genValid )
             , ( size', (:<=:) <$> genValid <*> genValid )
             , ( size', (:>=:) <$> genValid <*> genValid )
             , ( size', (:<:)  <$> genValid <*> genValid )
             , ( size', (:>:)  <$> genValid <*> genValid )
             ]
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance Validity IExp

instance QC.Arbitrary IExp where
  arbitrary = genValid
  shrink = shrinkValid

instance GenValid IExp where
  genValid = do
    size <- QC.getSize
    if size <= 0 then IConst <$> genValid
      else QC.scale (subtract 1)
          let size' = min size 1
          in QC.frequency
             [ (     5, Var <$> genValid )
             , (     5, IConst <$> genValid )
             , ( size', (:+:)  <$> genValid <*> genValid )
             , ( size', (:-:)  <$> genValid <*> genValid )
             , ( size', (:*:) <$> genValid <*> genValid )
             , ( size', (:/:) <$> genValid <*> genValid )
             , ( size', (:%:) <$> genValid <*> genValid )
             ]
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance Validity Variable where
  validate (Variable var) =
    let varString = unpack var
    in mconcat
    [ check (not $ null varString)
      "the identifier is at least one character long"
    , check (isAlpha (head varString) && isLower (head varString))
      "the identifier begins with a lowercase alphabetical character"
    , check (and [isAlphaNum c | c <- tail varString])
      "the identifier contains only alphanumeric characters"
    ]
instance GenValid Variable where
  genValid = do
    c <- QC.elements ['a'..'z']
    cs <- QC.resize 10
      $ QC.listOf
      $ QC.elements
      $ ['a'..'z'] ++ ['0'..'9']
    let var = Variable $ pack $ c:cs
    if var `elem` rwords
      then genValid
      else return var
  shrinkValid = shrinkValidStructurally
