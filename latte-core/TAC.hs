module TAC where

--

--newtype Label = Label String 
--	deriving (Eq,Ord)

--instance Show Label where
--	show (Label s) = s ++ ":"

data Address = 
	Const Integer
 |  Lab String
 |	Address	String
 |  Argument String
	deriving (Eq,Ord)

instance Show Address where
	show (Const i) = show i
	show (Lab s) = s
	show (Address s) = "$" ++ s
	show (Argument s) = s

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
 |  And
 |  Or
	deriving (Eq,Ord)

instance Show Op where
	show Plus = " + "
	show Minus = " - "
	show Times = " * "
	show Div = " / "
	show Mod = " % "
	show LTH = " < "
	show LE = " <= "
	show GTH = " > "
	show GE = " >= "
	show EQU = " == "
	show NE = " != "
	show Not = " ! "
	show And = " && "
	show Or = " || "


data Tac =
	Fun Address [Tac]
 |  Blck [Tac]
 |  AssC Address String Int -- x := call 
 |	Ass1 Address Address -- x := y
 |	Ass2 Address Op Address -- x := -y
 |	Ass3 Address Address Op Address -- x := y + z
 |  ArrGet Address Address Address -- x := y[i]
 |	ArrPut Address Address Address -- x[i] = y
 |	Jump Address
 |	JmpCnd Address Op Address Address Address
 |	Param Address
 |  Call Address Int
 |	Constant String String
 |  Label Address
 |	Return Address
 |  VReturn
 |  Fi Address [(Address, Address)]
 |  FunLabel Address
 	deriving (Eq,Ord)

instance Show Tac where
	show (Fun a insL) = "fun " ++ show (a) ++ (foldr (\i acc  -> "\n" ++ i ++ acc) "" (map show (tail insL)))
 	show (Blck insL) = (show $ head insL) ++ (foldr (\i acc  -> "\n\t" ++ i ++ acc) "" (map show (tail insL)))
 	show (AssC a s i) = show a ++ " := " ++ "call " ++ s ++ ", " ++ (show i)
 	show (Ass1 v1 v2) = (show v1) ++ " := " ++ (show v2)
 	show (Ass2 v1 op v2) = (show v1) ++ " :=" ++ (show op) ++ (show v2)
 	show (Ass3 v1 v2 op v3) = (show v1) ++ " := " ++ (show v2) ++ (show op) ++ (show v3)
 	show (Param a) = "param " ++ show a
 	show (Call a i) = "call " ++ (show a) ++ ", " ++ show (i)
 	show (Constant s1 s2) = "Constant " ++ s1 ++ ": " ++ s2
 	show (FunLabel a) = "fun " ++ (show a) ++ ":"
 	show (Label a) = (show a) ++ ":" 
 	show (Return a) = "return " ++ (show a)
 	show (VReturn) = "return" 
 	show (Jump a) = "goto " ++ (show a)
 	show (JmpCnd t1 op t2 l1 l2) = "if " ++ (show t1) ++ (show op) ++ show (t2) ++ 
 									" then goto " ++ (show l1) ++ " else goto " ++ (show l2)
 	show (Fi a1 l) = (show a1) ++ " = fi "++ show l


