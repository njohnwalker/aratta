module Language.GCL.Syntax.Abstract where

import qualified Control.Monad as M ( guard )

import Data.Text ( Text )
import GHC.Generics
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

gc ::= guard -> statement
guard ::= bexp
gcs ::= gc {[] gc}
statement ::= statement {; statement}
            | if gcs fi
            | do gcs od
            | var := iexp

bexp ::= const
       | bexp :&: bexp
       | Not bexp
       | iexp :<=: iexp

iexp ::= var | const
       | iexp :-: iexp

-}

newtype GuardedCommandSet =
  GCS { getCommandList :: [GuardedCommand] }
  deriving (Eq, Ord, Show, Generic)

instance Hashable GuardedCommandSet

data GuardedCommand = GC
  { guard :: BExp
  , statement :: Statement
  }
  deriving (Eq, Ord, Show, Generic)

instance Hashable GuardedCommand

data Statement
  = Seq [Statement]
  | If GuardedCommandSet
  | Do GuardedCommandSet
  | [Text] := [IExp]
  deriving (Eq, Ord, Show, Generic)

instance Hashable Statement

data BExp
  = BConst Bool
  | Not BExp
  | BExp :&: BExp
  | IExp :<=: IExp
  deriving (Eq, Ord, Show, Generic)


pattern l :>: r  = Not (l :<=: r)
pattern l :<: r  = Not (r :<=: l)
pattern l :>=: r = r :<=: l
pattern l :+: r  = l :-: (IConst 0 :-: r)
pattern l :|: r  = Not ( Not l :&: Not r)

-- this seems like too much...
pattern l :==: r <-
  ((\case ((l :<=: r) :&: (r2 :<=: l2)) -> (l :<=: r) <$ M.guard ((l == l2) && (r == r2))
          _ -> Nothing) -> Just (l :<=: r))
  where l :==: r = (l :<=: r) :&: (r :<=: l)

instance Hashable BExp

data IExp
  = Var Text
  | IConst Integer
  | IExp :-: IExp
  deriving (Eq, Ord, Show, Generic)

instance Hashable IExp

----------------------------------
-- Pretty-printing GCL programs --

instance Pretty IExp where
  pretty = \case
    Var txt -> pretty txt
    IConst int -> pretty int
    e1 :+: e2 -> smartParensIExp e1 <> softline <> "+" <> softline <> smartParensIExp e2
    e1 :-: e2 -> smartParensIExp e1 <> softline <> "-" <> softline <> smartParensIExp e2

instance Pretty BExp where
  pretty = \case
    BConst True -> "true"
    BConst False -> "false"
    e1 :==: e2 -> smartParensIExp e1 <> softline <> "==" <> softline <> smartParensIExp e2
    e1 :|: e2 -> smartParensBExp e1 <> softline <> "|" <> softline <> smartParensBExp e2
    e1 :>: e2 -> smartParensIExp e1 <> softline <> ">" <> softline <> smartParensIExp e2
    Not bexp -> parens $ "~" <> pretty bexp 
    e1 :&: e2 -> smartParensBExp e1 <> softline <> "&" <> softline <> smartParensBExp e2
    e1 :<=: e2 -> smartParensIExp e1 <> softline <> "<=" <> smartParensIExp e2

instance Pretty Statement where
  pretty = \case

    Seq stmts -> vsep $ punctuate (space <> semi) $ map pretty stmts

    var := exp -> (encloseSep emptyDoc emptyDoc (comma <> space) $ map pretty var)
              <+> ":="
              <+> (encloseSep emptyDoc emptyDoc (comma <> space) $ map pretty exp)

    If (GCS []) -> "if fi"
    If (GCS (gc:gcs)) ->
      "if" <+> (pretty gc) <> line
      <> encloseSep "[] " "" "[] " (map pretty gcs) <> line
      <> "fi"
    
    Do (GCS []) -> "do od"
    Do (GCS (gc:gcs)) ->
      "do" <+> (pretty gc) <> line
      <> encloseSep "[] " "" "[] " (map pretty gcs) <> line
      <> "od"

instance Pretty GuardedCommand where
  pretty GC { guard, statement }
    = align (pretty guard <+> "->" <> nest 3 (line <> pretty statement))

instance Pretty GuardedCommandSet where
  pretty GCS { getCommandList } = case getCommandList of
    [] -> emptyDoc
    [gc] -> pretty gc
    gc:gcs -> pretty gc 
           <> line
           <> "[]  "
           <> encloseSep "" "" "[] " (map pretty gcs)

-- | Render GCL AST to Text
renderGCLPretty :: Statement -> Text
renderGCLPretty = renderStrict . layoutSmart defaultGCLLayoutOptions . pretty
  where
    defaultGCLLayoutOptions = LayoutOptions $ AvailablePerLine 60 1.0


smartParensBExp = \case
  BConst b -> pretty $ BConst b
  bexp -> parens $ pretty bexp

smartParensIExp = \case
  Var var -> pretty var
  IConst i -> pretty i
  iexp -> parens $ pretty iexp
