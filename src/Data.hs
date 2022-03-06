module Data where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified LLVM.AST.IntegerPredicate as Sicmp
import LLVM.AST ( Operand )
import LLVM.AST.Type as LType ()
import LLVM.IRBuilder ( ModuleBuilder, IRBuilderT )
import Control.Monad.Reader ( ReaderT )

-- FOR BETTER SHOW
-- import Text.Pretty.Simple (pPrint)
-- pPrint $ ...

type AST = Node

data Node = Node Data.Type Value
    | Error String
    deriving (Show, Eq)

data Type = TUndefine
    | TError String
    | TNone
    | TVoid
    | TDouble
    | TInteger
    | TBool
    | TFunc [Data.Type]
    deriving (Show, Eq)

data Value =
    VStmt [Node]
  | VKdefs Node
  | VComs String
  | VDefs Node Node
  | VPrototype Node Node
  | VPrototypeArgs [(Node, Node)] Node
  | VArgsType ArgsType
  | VExprs [Node]
  | VForExpr (Node, Node) (Node, Node) Node Node
  | VIfExpr Node Node Node
  | VWhileExpr Node Node
  | VExpr Node [(Node, Node)]
  | VUnary Node Node
  | VPostfix Node Node
  | VCallExpr [Node]
  | VPrimary Node
  | VIdentifier (String, IsDeclaration)
  | VDecimalConst Int
  | VDoubleConst Double
  | VLiteral Node
  | VBoolean Boolean
  | VBinop Binop
  | VUnop Unop
  | VError String
  | VNothing
    deriving (Show, Eq)

type TypedId = Map Identifier Data.Type

type Stmt = [Kdefs]

data Kdefs = KDefs Defs
    | KExprs Exprs
    | KComs Coms
    deriving (Show, Eq)

type Coms = String

data Defs = Defs Prototype Exprs
    deriving (Show, Eq)

data Prototype = Prototype Identifier PrototypeArgs
    deriving (Show, Eq)

data PrototypeArgs = PrototypeArgs [(Identifier, ArgsType)] ArgsType
    deriving (Show, Eq)

data ArgsType = Int | Double | Void
    deriving (Show, Eq)

data Exprs = EForExpr ForExpr
    | EWhileExpr WhileExpr
    | EIfExpr IfExpr
    | EExpr [Expr]
    deriving (Show, Eq)

data ForExpr = ForExpr (Identifier, Expr) (Identifier, Expr) Expr Exprs
    deriving (Show, Eq)

data IfExpr = IfExpr Expr Exprs (Maybe Exprs)
    deriving (Show, Eq)

data WhileExpr = WhileExpr Expr Exprs
    deriving (Show, Eq)

data Expr = Expr Unary [(Binop, Unary)]
    deriving (Show, Eq)

data Unary = Unop Unop Unary | UPostfix Postfix
    deriving (Show, Eq)

data Postfix = Postfix Primary (Maybe CallExpr)
    deriving (Show, Eq)

type CallExpr = [Expr]

data Primary = PId Identifier
    | PLit Literal
    | PBoolean Boolean
    | PExprs Exprs
    deriving (Show, Eq)

type Identifier = String

type IsDeclaration = Bool

data Literal = LInt DecimalConst | LDouble DoubleConst
    deriving (Show, Eq)

type DecimalConst = Int

type DoubleConst = Double

data Boolean = True | False
    deriving (Show, Eq)

data Binop = Mul | Div | Add | Sub | Mod | Gt | Lt | Ge | Le | Eq | Neq | Assign
    deriving (Show, Eq)

data Unop = Not | Minus
    deriving (Show, Eq)

-- LLVM Data

type AssignedValues = Map String Operand

type Codegen = ReaderT AssignedValues (IRBuilderT ModuleBuilder)