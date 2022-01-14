module Data where


Data AST = Node Type Value AST
    | Empty

Data Type = None
    | Double Type
    | Integer Type

Data Stmt = [Kdefs]

Data Kdefs = Defs Exprs

Data Defs = Prototype Exprs

-- add Unary / Binary
Data Prototype = Identifier PrototypeArgs

data PrototypeArgs = [(Identifier, Type)] Type

data ArgsType = Int | Double | Void

Data Exprs = ForExpr
    | WhileExpr
    | IfExpr
    | [Expr]

data ForExpr = (Identifier, Expr) (Identifier, Expr) Expr Exprs

data IfExpr = Expr Exprs (Maybe Exprs)

data WhileExpr = Expr Exprs

Data Expr = Unary [(Binop, SubExpr)]

Data Unary = Unop Unop Unary | Postfix Postfix

Data Postfix = Primary (Maybe CallExpr)

Data CallExpr = [Maybe Expr]

Data Primary = Id Identifier
    | Lit Literal
    | Exprs Exprs

type Identifier = String

-- Dot, Necessary ?

Data DecimalConst = Int

Data DoubleConst = Double

Data Literal = Int DecimalConst | Double DoubleConst

Data Binop = Mul | Div | Add | Sub | Gt | Lt | Eq | Neq | Assign

Data Unop = Not | Minus

Data SubExpr =  Unary Unary | Expr Expr




