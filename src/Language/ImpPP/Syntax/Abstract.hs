{-# LANGUAGE DeriveAnyClass #-}
module Language.ImpPP.Syntax.Abstract where

import Data.Hashable
import GHC.Generics
import Data.Text.Prettyprint.Doc ( Doc, Pretty
                                 , pretty, prettyList, emptyDoc
                                 , nest, softline, line, indent
                                 , space, flatAlt, parens
                                 , braces, semi, comma, hsep
                                 , punctuate, (<+>), (<>)
                                 )

type Id = String
type DecList = [Id]

data Exp a where
  (:<=:) :: Exp Integer -> Exp Integer -> Exp Bool
  (:&&:) :: Exp Bool    -> Exp Bool    -> Exp Bool
  Not    :: Exp Bool                   -> Exp Bool
  (:+:)  :: Exp Integer -> Exp Integer -> Exp Integer
  (:/:)  :: Exp Integer -> Exp Integer -> Exp Integer
  Read   ::                               Exp Integer
  Inc    :: Id                         -> Exp Integer
  Var    :: Id                         -> Exp Integer
  Lit    :: a                          -> Exp a
  Error  ::                               Exp a
  Spawn  :: Stmt                       -> Exp Integer
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
    Error      -> salt `hashWithSalt` (7::Int)
    Spawn stmt -> salt `hashWithSalt` (8::Int) `hashWithSalt` stmt


data Stmt
  = Id := Exp Integer
  | Decl [Id] Stmt
  | Print (Exp Integer)
  | Join (Exp Integer)
  | Seq Stmt Stmt
  | IfElse (Exp Bool) Stmt Stmt
  | While (Exp Bool) Stmt
  | Empty
  | Halt
  deriving( Show, Eq, Ord, Generic, Hashable )

---------------------------------
-- Reformatting IMP++ programs --
instance (Show a, Pretty a) => Pretty (Exp a) where
  pretty exp = case exp of
    Error      -> "<error: division by 0>"
    Read       -> "read()"
    Lit val    -> pretty val
    Var var    -> pretty var
    Not e      -> "!" <> pretty e
    Inc var    -> "++" <> pretty var
    e1 :+: e2  -> parens $ pretty e1 <> softline <> "+"  <+> pretty e2
    e1 :/: e2  -> parens $ pretty e1 <> softline <> "/"  <+> pretty e2
    e1 :<=: e2 -> parens $ pretty e1 <> softline <> "<=" <+> pretty e2
    e1 :&&: e2 -> parens $ pretty e1 <> softline <> "&&" <+> pretty e2
    Spawn stmt -> parens $ "spawn" <> braces' stmt

instance Pretty Stmt where
  pretty stmt = case stmt of
   Empty                   -> "{}"
   Halt                    -> "halt ;"
   Join aexp               -> "Join" <+> pretty aexp <+> semi
   x := aexp               -> pretty x <+> ":=" <+> pretty aexp <+> semi
   Seq stmt1 stmt2         -> pretty stmt1 <> line <> pretty stmt2
   While bexp stmt         -> "while" <+> pretty bexp <+> braces' stmt
   Print aexp              -> "print" <> parens (pretty aexp) <+> semi
   Decl [] stmt            -> pretty stmt
   IfElse bexp stmt1 stmt2 -> "if" <+> pretty bexp
                                 <+> braces' stmt1
                                 <+> "else" <+> braces' stmt2
   Decl ids stmt -> "int" <+> varList <+> semi <> line <> pretty stmt
    where varList = hsep $ punctuate comma $ map pretty ids

braces' :: Stmt -> Doc ann
braces' Empty = "{}"
braces' stmt = braces $ line <> indent 2 (pretty stmt) <> line
