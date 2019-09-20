{-# LANGUAGE DeriveAnyClass #-}
module Language.Imp.Syntax.Abstract where

import GHC.Generics
import Data.List ( intercalate )
import Data.Hashable
import Data.Text ( Text )
import Data.Text.Prettyprint.Doc ( Doc, Pretty
                                 , pretty, prettyList, emptyDoc
                                 , nest, softline, line, indent
                                 , space, flatAlt, parens
                                 , braces, semi, comma, hsep
                                 , punctuate, (<+>), (<>)
                                 )

-- | Identifier type synonym
type Id = String

-- | Declaration list type synonym
type DecList = [Id]

-- | GADT representing Imp expressions over integer, boolean, or potentially other types.
data Exp a where
  (:+:)  :: Exp Integer -> Exp Integer -> Exp Integer
  (:/:)  :: Exp Integer -> Exp Integer -> Exp Integer
  (:<=:) :: Exp Integer -> Exp Integer -> Exp Bool
  (:&&:) :: Exp Bool    -> Exp Bool    -> Exp Bool
  Not    :: Exp Bool                   -> Exp Bool
  Var    :: Id                         -> Exp Integer
  Lit    :: a                          -> Exp a
deriving instance Eq a => Eq (Exp a)
deriving instance Ord a => Ord (Exp a)
deriving instance Show a => Show (Exp a)

instance Hashable a => Hashable (Exp a) where
  hashWithSalt salt exp = case exp of
    e1 :+:  e2 -> salt `hashWithSalt` (0::Int) `hashWithSalt` e1 `hashWithSalt` e2
    e1 :/:  e2 -> salt `hashWithSalt` (1::Int) `hashWithSalt` e1 `hashWithSalt` e2
    e1 :<=: e2 -> salt `hashWithSalt` (2::Int) `hashWithSalt` e1 `hashWithSalt` e2
    e1 :&&: e2 -> salt `hashWithSalt` (3::Int) `hashWithSalt` e1 `hashWithSalt` e2
    Not e      -> salt `hashWithSalt` (4::Int) `hashWithSalt` e
    Var var    -> salt `hashWithSalt` (5::Int) `hashWithSalt` var
    Lit lit    -> salt `hashWithSalt` (6::Int) `hashWithSalt` lit

-- | Statement abstract representation
data Stmt
  = Id := Exp Integer
  | Seq Stmt Stmt
  | IfElse (Exp Bool) Stmt Stmt
  | While (Exp Bool) Stmt
  | Empty
  deriving (Eq, Ord, Show, Generic, Hashable)

-- | Program abstract representation
data Pgm = Pgm DecList Stmt
  deriving (Eq, Ord, Show, Generic, Hashable)


-------------------------------
-- Reformatting IMP programs --
instance (Show a, Pretty a) => Pretty (Exp a) where
  pretty (Lit val)    = pretty val
  pretty (Var var)    = pretty var
  pretty (Not e)      = "!" <> pretty e
  pretty (e1 :+: e2)  = parens $ pretty e1 <> softline <> "+"  <+> pretty e2
  pretty (e1 :/: e2)  = parens $ pretty e1 <> softline <> "/"  <+> pretty e2
  pretty (e1 :<=: e2) = parens $ pretty e1 <> softline <> "<=" <+> pretty e2
  pretty (e1 :&&: e2) = parens $ pretty e1 <> softline <> "&&" <+> pretty e2

instance Pretty Stmt where
  pretty Empty                     = braces' Empty
  pretty (x := aexp)               = pretty x <+> ":=" <+> pretty aexp <+> semi
  pretty (Seq stmt1 stmt2)         = pretty stmt1 <> line <> pretty stmt2
  pretty (While bexp stmt)         = "while" <+> pretty bexp <+> braces' stmt
  pretty (IfElse bexp stmt1 stmt2) = "if" <+> pretty bexp
                                 <+> braces' stmt1
                                 <+> "else" <+> braces' stmt2

braces' :: Stmt -> Doc ann
braces' Empty = braces emptyDoc
braces' stmt = braces $ line <> indent 2 (pretty stmt) <> line

instance Pretty Pgm where
  pretty (Pgm ids stmt) = "int" <+> varList <+> semi <> line <> pretty stmt
    where varList = hsep $ punctuate comma $ map pretty ids
