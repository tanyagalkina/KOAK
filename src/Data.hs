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
  | VBinop Binop
  | VUnop Unop
  | VError String
  | VNothing
    deriving (Show, Eq)

type TypedId = Map Identifier Data.Type

-- kdefs* #eof
type Stmt = [Kdefs]

-- 'def' defs ';' | expressions ';'
data Kdefs = KDefs Defs
    | KExprs Exprs
    deriving (Show, Eq)

-- prototype expressions
data Defs = Defs Prototype Exprs
    deriving (Show, Eq)

--  ( 'unary' . decimal_const ? | 'binary' . decimal_const ? ) identifier prototype_args
data Prototype = Prototype Identifier PrototypeArgs
    deriving (Show, Eq)

-- '(' ( identifier ':' type ','? ) * ')' ':' type
data PrototypeArgs = PrototypeArgs [(Identifier, ArgsType)] ArgsType
    deriving (Show, Eq)

-- 'int' | 'double' | 'void'
data ArgsType = Int | Double | Void
    deriving (Show, Eq)

-- for_expr | if_expr | while_expr | expression (':' expression )*
data Exprs = EForExpr ForExpr
    | EWhileExpr WhileExpr
    | EIfExpr IfExpr
    | EExpr [Expr]
    deriving (Show, Eq)

-- 'for' identifier '=' expression ',' identifier '<' expression ',' expression 'in' expressions
data ForExpr = ForExpr (Identifier, Expr) (Identifier, Expr) Expr Exprs
    deriving (Show, Eq)

-- 'if' expression 'then' expressions ('else' expressions )?
data IfExpr = IfExpr Expr Exprs (Maybe Exprs)
    deriving (Show, Eq)

-- 'while' expression 'do' expressions
data WhileExpr = WhileExpr Expr Exprs
    deriving (Show, Eq)

-- unary (#binop ( unary ) )*
data Expr = Expr Unary [(Binop, Unary)]
    deriving (Show, Eq)

-- # unop unary | postfix
data Unary = Unop Unop Unary | UPostfix Postfix
    deriving (Show, Eq)

-- primary call_expr?
data Postfix = Postfix Primary (Maybe CallExpr)
    deriving (Show, Eq)

-- '(' ( expression (',' expression ) *) ? ')'
type CallExpr = [Expr]

-- identifier | literal | '(' expressions ')'
data Primary = PId Identifier
    | PLit Literal
    | PExprs Exprs
    deriving (Show, Eq)

-- [a - z A - Z][a - z A - Z 0 - 9]*
type Identifier = String

type IsDeclaration = Bool

-- [0 - 9]+
type DecimalConst = Int

-- ( decimal_const dot [0 - 9]* | dot [0 - 9]+ )
type DoubleConst = Double

--  decimal_const | double_const
data Literal = LInt DecimalConst | LDouble DoubleConst
    deriving (Show, Eq)

data Binop = Mul | Div | Add | Sub | Mod | Gt | Lt | Eq | Neq | Assign
    deriving (Show, Eq)

data Unop = Not | Minus
    deriving (Show, Eq)

-- LLVM Data

type AssignedValues = Map String Operand

type Codegen = ReaderT AssignedValues (IRBuilderT ModuleBuilder)