module Data where

type AST = Node

data Node = Node Type Value
    deriving (Eq)

instance Show Node where
    show (Node t VNothing) = ""
    show (Node t v) = "\nType -> " ++ show t
                                   ++ "  ---  Value -> "
                                   ++ show v
                                   ++ ""

data Type = TUndefine
    | TNone
    | TDouble Type
    | TInteger Type
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
  | VIdentifier String
  | VDecimalConst Int
  | VDoubleConst Double
  | VLiteral Node
  | VBinop Binop
  | VUnop Unop
  | VNothing
    deriving (Eq)

instance Show Value where
    show (VStmt t) = "Stmt\n" ++ show t
    show (VKdefs n) = "Kdefs\n" ++ show n
    show (VDefs n1 n2) = "Defs\n" ++ show n1 ++ " " ++ show n2
    show (VPrototype n1 n2) = "Prototype\n" ++ show n1 ++ " " ++ show n2
    show (VPrototypeArgs t n) = "ProtypeArgs\n" ++ show t ++ " " ++ show n
    show (VArgsType at) = "ArgsType\n" ++ show at
    show (VExprs t) = "Exprs\n" ++ show t
    show (VForExpr (n1, n2) (n3, n4) n5 n6) = "ForExpr\n" ++ "("
                                                         ++ show n1
                                                         ++ ", "
                                                         ++ show n2
                                                         ++ ") ("
                                                         ++ show n3
                                                         ++ ", "
                                                         ++ show n4
                                                         ++ ") "
                                                         ++ show n5
                                                         ++ " "
                                                         ++ show n6
    show (VIfExpr n1 n2 n3) = "IfExpr\n" ++ show n1
                                        ++ " "
                                        ++ show n2
                                        ++ " "
                                        ++ show n3
    show (VWhileExpr n1 n2) = "WhileExpr\n" ++ show n1 ++ " " ++ show n2
    show (VExpr n t) = "Expr\n" ++ show n ++ " " ++ show t
    show (VUnary n1 n2) = "Unary\n" ++ show n1 ++ " " ++ show n2
    show (VPostfix n1 n2) = "Postfix\n" ++ show n1 ++ " " ++ show n2
    show (VCallExpr t) = "CallExpr\n" ++ show t
    show (VPrimary n) = "Primary\n" ++ show n
    show (VIdentifier s) = "Identifier\n" ++ show s
    show (VDecimalConst i) = "DecimalConst\n" ++ show i
    show (VDoubleConst d) = "DoubleConst\n" ++ show d
    show (VLiteral n) = "Literal\n" ++ show n
    show (VBinop b) = "Binop\n" ++ show b
    show (VUnop u) = "Unop\n" ++ show u
    show VNothing = "Nothing"

-- kdefs* #eof
type Stmt = [Kdefs]

-- 'def' defs ';' | expressions ';'
data Kdefs = KDefs Defs
    | KExprs Exprs
    deriving (Show, Eq)

-- prototype expressions
data Defs = Defs Prototype Exprs
    deriving (Show, Eq)

-- ADD UNARY / BINARY IN PROTOTYPE

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
    | EExprs [Expr]
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
data Primary = PId Identifier
    | PLit Literal
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