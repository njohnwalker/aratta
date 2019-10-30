module Language.GCL.Syntax.Abstract where

import qualified Control.Monad as M ( guard )

import Data.Data

import Control.Lens
import Data.Text ( Text )
import GHC.Generics hiding ( (:*:) )
import Data.Hashable

import Data.Text.Prettyprint.Doc.Render.Text ( renderStrict )
import Data.Text.Prettyprint.Doc ( PageWidth(..), LayoutOptions(..)
                                 , layoutSmart, hang, vsep, align,  Doc, Pretty
                                 , pretty, prettyList, emptyDoc
                                 , nest, softline, line, indent
                                 , space, flatAlt, parens
                                 , braces, semi, comma, sep
                                 , encloseSep, punctuate, (<+>), (<>)
                                 )

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
  , program :: Statement
  , ens :: Maybe BExp
  } deriving (Eq, Ord, Show, Generic, Data)

instance Hashable GCLProgram

data Statement
  = Seq [Statement]
  | If GuardedCommandSet
  | Do (Maybe BExp) GuardedCommandSet
  | [Text] := [IExp]
  deriving (Eq, Ord, Show, Generic, Data)

instance Hashable Statement

instance Plated Statement

newtype GuardedCommandSet =
  GCS { getCommandList :: [GuardedCommand] }
  deriving (Eq, Ord, Show, Generic, Data)

instance Hashable GuardedCommandSet

data GuardedCommand = GC
  { guard :: BExp
  , statement :: Statement
  }
  deriving (Eq, Ord, Show, Generic, Data)

instance Hashable GuardedCommand

data BExp
  = BConst Bool
  | Not BExp
  | BExp :&: BExp
  | IExp :<=: IExp
  deriving (Eq, Ord, Show, Generic, Data)
infixr 3 :&:
infix 4 :<=:

type Variable = Text

data IExp
  = Var Variable -- TODO: support existentials in verification annotations
  | IConst Integer
  | IExp :-: IExp
  | IExp :*: IExp
  | IExp :/: IExp
  | IExp :%: IExp
  deriving (Eq, Ord, Show, Generic, Data)
infixl 6 :-:
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

infix 4 :>:, :<:, :>=:
pattern l :>: r  = Not (l :<=: r)
pattern l :<: r  = Not (r :<=: l)
pattern l :>=: r = r :<=: l

infixl 6 :+:
pattern l :+: r  = l :-: (IConst 0 :-: r)

infixr 2 :=>:, :|:
pattern l :|: r  = Not ( Not l :&: Not r)
pattern l :=>: r = Not l :|: r

-- this seems like too much...
infix 4 :==:
pattern l :==: r <-
  ((\case ((l :<=: r) :&: (r2 :<=: l2)) ->
            (l :<=: r) <$ M.guard ((l == l2) && (r == r2))
          _ -> Nothing) -> Just (l :<=: r))
  where l :==: r = (l :<=: r) :&: (r :<=: l)

----------------------------------
-- Pretty-printing GCL programs --
prettyIntBinop :: Doc ann -> IExp -> IExp -> Doc ann
prettyIntBinop op e1 e2 =
  smartParensIExp e1 <> softline <> op <> softline <> smartParensIExp e2

prettyBoolBinop :: Doc ann -> BExp -> BExp -> Doc ann
prettyBoolBinop op e1 e2 =
  smartParensBExp e1 <> softline <> op <> softline <> smartParensBExp e2

instance Pretty IExp where
  pretty = \case
    Var txt -> pretty txt
    IConst int -> pretty int
    e1 :+: e2 -> prettyIntBinop "+" e1 e2
    e1 :-: e2 -> prettyIntBinop "-" e1 e2
    e1 :*: e2 -> prettyIntBinop "*" e1 e2
    e1 :%: e2 -> prettyIntBinop "%" e1 e2
    e1 :/: e2 -> prettyIntBinop "/" e1 e2

instance Pretty BExp where
  pretty = \case -- caution, case order matters (see pattern synonyms)
    BConst True -> "true"
    BConst False -> "false"
    e1 :==: e2 -> prettyIntBinop "==" e1 e2
    e1 :|: e2  -> prettyBoolBinop "|" e1 e2
    e1 :>: e2  -> prettyIntBinop ">" e1 e2
    Not bexp -> "~" <> smartParensBExp bexp
    e1 :&: e2  -> prettyBoolBinop "&" e1 e2
    e1 :<=: e2 -> prettyIntBinop "<=" e1 e2

instance Pretty GuardedCommand where
  pretty GC { guard, statement }
    = align (pretty guard <+> "->" <> nest 3 (line <> pretty statement))

-- not needed instance
-- instance Pretty GuardedCommandSet where

instance Pretty Statement where
  pretty = \case

    Seq stmts -> vsep $ map pretty stmts

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
            _ -> vsep (map (("[]"<+>) . pretty) gcs) <> line
      <> "fi"

    Do inv (GCS []) -> "do" <+> (prettyAnn "@inv" inv) <+> "od"
    Do inv (GCS gcs)
      -> "do" <+> prettyAnn "@inv" inv <> line
      <> vsep (map (("[]"<+>) .  pretty) gcs) <> line
      <> "od"

instance Pretty GCLProgram where
  pretty GCLProgram {req, program, ens}
    =  "@req(" <> pretty req <> ")" <> line
    <> pretty program <> line
    <> "@ens(" <> pretty ens <> ")" <> line

-- | Render GCL AST to Text
renderGCLPretty :: GCLProgram -> Text
renderGCLPretty = renderStrict . layoutSmart defaultGCLLayoutOptions . pretty
  where
    defaultGCLLayoutOptions = LayoutOptions $ AvailablePerLine 60 1.0

smartParensBExp = \case
  BConst b -> pretty $ BConst b
  n@(Not _) -> pretty n
  bexp -> parens $ pretty bexp

smartParensIExp = \case
  Var var -> pretty var
  IConst i -> pretty i
  iexp -> parens $ pretty iexp

prettyAnn tag Nothing = tag <> parens mempty
prettyAnn tag (Just bexp) = tag <> (parens $ pretty bexp)
