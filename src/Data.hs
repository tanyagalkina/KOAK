module Data where

type AST = Node

data Node = Node Type Value

data Type = None
    | TDouble Type
    | TInteger Type

data Value = VStmt [Node]
           | VKdefs Node Node
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
           | VIdentifier String
           | VDecimalConst Int
           | VDoubleConst Double
           | VLiteral Node
           | VBinop Binop
           | VUnop Unop
           | VSubExpr Node
           | VNothing

type Stmt = [Kdefs]

data Kdefs = Kdefs Defs Exprs

data Defs = Defs Prototype Exprs

-- Add Unary / Binary

data Prototype = Prototype Identifier PrototypeArgs

data PrototypeArgs = PrototypeArgs [(Identifier, ArgsType)] ArgsType

data ArgsType = Int | Double | Void

data Exprs = EForExpr
    | EWhileExpr
    | EIfExpr
    | EExprs [Expr]

data ForExpr = ForExpr (Identifier, Expr) (Identifier, Expr) Expr Exprs

data IfExpr = IfExpr Expr Exprs (Maybe Exprs)

data WhileExpr = WhileExpr Expr Exprs

data Expr = Expr Unary [(Binop, SubExpr)]

data SubExpr =  SEUnary Unary | SEExpr Expr

data Unary = Unop Unop Unary | Postfix Postfix

data Postfix = MyPostfix Primary (Maybe CallExpr)

type CallExpr = [Maybe Expr]

data Primary = Id Identifier
    | Lit Literal
    | PExprs Exprs

type Identifier = String

type DecimalConst = Int

type DoubleConst = Double

data Literal = LInt DecimalConst | LDouble DoubleConst

data Binop = Mul | Div | Add | Sub | Gt | Lt | Eq | Neq | Assign

data Unop = Not | Minus