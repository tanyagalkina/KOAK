module Data where

type AST = Node

data Node = Node Type Value
    deriving (Show, Eq)

data Type = TNone
    | TDouble Type
    | TInteger Type
    deriving (Show, Eq)

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
    deriving (Show, Eq)

-- kdefs* #eof
type Stmt = [Kdefs]

-- 'def' defs ';' | expressions ';'
data Kdefs = KDefs Defs
    | KExprs Exprs
    deriving (Eq)

instance Show Kdefs where
    show (KDefs defs) = "\nDefs ->\n" ++ show defs
    show (KExprs exprs) = "\nExprs ->\n" ++ show exprs

-- prototype expressions
data Defs = Defs Prototype Exprs
    deriving (Eq)

instance Show Defs where
    show (Defs proto exprs) = "\tPrototype ->\n" ++ show proto
                                                ++ "\n\tBody      ->\n"
                                                ++ show exprs
                                                ++ "\n"

-- ADD UNARY / BINARY IN PROTOTYPE

--  ( 'unary' . decimal_const ? | 'binary' . decimal_const ? ) identifier prototype_args
data Prototype = Prototype Identifier PrototypeArgs
    deriving (Eq)

instance Show Prototype where
    show (Prototype id types) = "\t\tName  -> " ++ show id
                                              ++ "\n\t\tTypes ->\n"
                                              ++ show types

-- '(' ( identifier ':' type ','? ) * ')' ':' type
data PrototypeArgs = PrototypeArgs [(Identifier, ArgsType)] ArgsType
    deriving (Eq)

instance Show PrototypeArgs where
    show (PrototypeArgs [] ret) = "\t\t\tReturn -> " ++ show ret
    show (PrototypeArgs args ret) = "\t\t\tArgs   -> " ++ show args
                                                     ++ "\n\t\t\tReturn -> "
                                                     ++ show ret

-- 'int' | 'double' | 'void'
data ArgsType = Int | Double | Void
    deriving (Show, Eq)

-- for_expr | if_expr | while_expr | expression (':' expression )*
data Exprs = EForExpr ForExpr
    | EWhileExpr WhileExpr
    | EIfExpr IfExpr
    | EExprs [Expr]
    deriving (Eq)

instance Show Exprs where
    show (EForExpr expr) = "\t\tFor -> " ++ show expr
    show (EWhileExpr expr) = "\t\tWhile -> " ++ show expr
    show (EIfExpr expr) = "\t\tIf -> " ++ show expr
    show (EExprs []) = ""
    show (EExprs (x:xs)) = "\t\tExpr -> " ++ show x
                                              ++ "\n"
                                              ++ show (EExprs xs)

-- 'for' identifier '=' expression ',' identifier '<' expression ',' expression 'in' expressions
data ForExpr = ForExpr (Identifier, Expr) (Identifier, Expr) Expr Exprs
    deriving (Show, Eq)

-- 'if' expression 'then' expressions ('else' expressions )?
data IfExpr = IfExpr Expr Exprs (Maybe Exprs)
    deriving (Show, Eq)

-- 'while' expression 'do' expressions
data WhileExpr = WhileExpr Expr Exprs
    deriving (Show, Eq)

-- UNARY OR EXPRESSION -> ONE TO REMOVE / SAME

-- unary (# binop ( unary ) )*
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
data Primary = Id Identifier
    | Lit Literal
    | PExprs Exprs
    deriving (Show, Eq)

-- [a - z A - Z][a - z A - Z 0 - 9]*
type Identifier = String

-- [0 - 9]+
type DecimalConst = Int

-- ( decimal_const dot [0 - 9]* | dot [0 - 9]+ )
type DoubleConst = Double

--  decimal_const | double_const
data Literal = LInt DecimalConst | LDouble DoubleConst
    deriving (Show, Eq)
    
data Binop = Mul | Div | Add | Sub | Gt | Lt | Eq | Neq | Assign
    deriving (Show, Eq)

data Unop = Not | Minus
    deriving (Show, Eq)