module TAC where

--

newtype Label = Label String 
	deriving (Eq,Ord)

instance Show Label where
	show (Label s) = s ++ ":"

data Var = 
	ConstI Integer
 |	ConstS String
 |	ConstB Bool
 |	Var	String
	deriving (Eq,Ord)

instance Show Var where
	show (ConstI i) = show i
	show (ConstS s) = s
	show (ConstB b) = show b
	show (Var s) = "$" ++ s

data Op =
	Plus
 |	Minus
 |	Times
 |	Div
 | 	Mod
 |	LTH
 |	LE
 |	GTH
 |	GE
 |	EQU
 |	NE
 |  Not
 | And
 | Or
	deriving (Eq,Ord)

instance Show Op where
	show Plus = "+"
	show Minus = "-"
	show Times = "*"
	show Div = "/"
	show Mod = "%"
	show LTH = "<"
	show LE = "<="
	show GTH = ">"
	show GE = ">="
	show EQU = "=="
	show NE = "!="
	show Not = "!"
	show And = "&&"
	show Or = "||"


data Tac =
	Blck Label [Tac] 
 |  AssC Var Label Integer -- x := call 
 |	Ass1 Var Var -- x := y
 |	Ass2 Var Op Var -- x := -y
 |	Ass3 Var Var Op Var -- x := y + z
 |  ArrGet Var Var Var -- x := y[i]
 |	ArrPut Var Var Var -- x[i] = y
 |	Jmp Label 
 |	JmpCnd Var Op Var Label Label
 |	Param Var
 |  Call Label Integer
 |  Lab Label
 |	Return Var
 	deriving (Eq,Ord)

instance Show Tac where
 	show (Blck l insL) = (show l) ++ (foldr (\i acc  -> "\n" ++ i ++ acc) "" (map show insL))
 	show (Ass1 v1 v2) = (show v1) ++ " := " ++ (show v2)
 	show (Ass2 v1 op v2) = (show v1) ++ " := " ++ (show op) ++ " " ++ (show v2)
 	show (Ass3 v1 v2 op v3) = (show v1) ++ " := " ++ (show v2) ++ " " ++ (show op) ++ " " ++ (show v3)
 	show (Lab l) = show l 


