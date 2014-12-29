module TAC where

--


-- import AbsLatte

newtype Label = Label String 
	deriving (Eq,Ord,Show)

data Var = 
	ConstI Integer
 |	ConstS String
 |	ConstB Bool
 |	Var	String
	deriving (Eq,Ord,Show)

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
	deriving (Eq,Ord,Show)

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
 	deriving (Eq,Ord,Show)


