module TAC where

--

import qualified Data.Map as Map
import Data.List
--newtype Label = Label String 
--	deriving (Eq,Ord)

--instance Show Label where
--	show (Label s) = s ++ ":"

data Address = 
	Const String Integer
 |  Lab String
 |	Address	String Integer
 |  ConstString String String
	deriving (Eq,Ord)

instance Show Address where
	show (Const t i) = t ++ " " ++ (show i)
	show (Lab s) = s
	show (Address a i) = "%" ++ a ++ (show i)

_show :: Address -> String
_show a = case a of
			Const s i -> show i
			Lab s -> s
			Address a i -> "%" ++ a ++ (show i)
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
	show Plus = " add "
	show Minus = " sub "
	show Times = " mul "
	show Div = " idiv "
	show Mod = " mod "
	show LTH = " icmp slt "
	show LE = " icmp sle "
	show GTH = " icmp sgt "
	show GE = " icmp sge "
	show EQU = " icmp seq "
	show NE = " icmp sne "
	show Not = " ! "
	show And = " && "
	show Or = " || "


data Tac =
    Blck [Tac]
 |  AssC Address String String [String] [Address] -- x := call 
 |	Ass1 Address Address -- x := y
 |	Ass3 String Address Address Op Address -- x := y + z
 |  ArrGet Address Address Address -- x := y[i]
 |	ArrPut Address Address Address -- x[i] = y
 |	Jump Address
 |	JmpCnd Address Address Address
 |	Param Address
 |  Call String [String] [Address]
 |	Constant String String
 |  Label Address
 |	Return String Address
 |  VReturn
 |  Fi Address String [(Address, Address)]
 |  FunLabel String Address [(String,Address)]
 |	Empty
 |	ChangeBlocs
 |	Dump (Map.Map String Address) 
 |  TmpFi Address String String [(Address, Address)]
 |  EndFun
 	deriving (Eq,Ord)

instance Show Tac where
	--show (Fun a insL) = "\nfun " ++ show (a) ++ (foldr (\i acc  -> "\n" ++ i ++ acc) "" (map show (tail insL)))
 	show (Blck insL) = (show $ head insL) ++ (foldr showIns "" (map show (tail insL)))			
 		where showIns i acc =
 			if (i == "") || (i == "entry:") then
 				acc
 			else if i == 	"}" then
 				"\n" ++ i ++ acc
 			else
 				"\n  " ++ i ++ acc
 	
 	show (AssC a ident typ argTypes addrL) = 
		let zipped' = intersperse ", " (map (\(t,a) -> t ++ " " ++ (_show a)) (zip argTypes addrL)) in
		show a ++ " = call " ++ typ ++ " @" ++ ident ++ "(" ++ (concat zipped') ++ ")"
 	
 	show (Ass1 v1 v2) = (show v1) ++ " = " ++ (show v2)
 	
 	show (Ass3 typ v1 v2 op v3) = (show v1) ++ " =" ++ (show op) ++ typ ++ " " ++ (_show v2) ++ ", " ++ (_show v3)
 	
 	show (Param a) = "param " ++ show a
 	--show (Call ident argTypes addrL) = "call " ++ (show a) ++ ", " ++ show (i)
 	
 	show (Constant s1 s2) = "Constant " ++ s1 ++ ": " ++ s2
 	
 	show (Label a) = (show a) ++ ":" 
 	
 	show (Return typ a) = "ret " ++ typ ++ " " ++ (_show a)
 	
 	show (VReturn) = "ret void" 
 	
 	show (Jump a) = "br label %" ++ (show a)
 	
 	show (JmpCnd t1 l1 l2) = "br i1 " ++ (show t1) ++ ", label %" ++ (show l1) ++ ", label %" ++ (show l2)
 	
 	show (Fi a t ls) = 
 		let ls' = map (\(l1,v1) -> "[ " ++ (_show v1) ++ ", %" ++ (show l1) ++ " ]") ls in
 		(show a) ++ " = phi " ++ t ++ " " ++ (concat $ intersperse ", " ls')
 	
 	show (Empty) = ""
 	
 	show (TmpFi a t s l) = (show a) ++ " = TMPfi " ++ t ++ " " ++ s ++ show l
 	
 	show (Dump l) = "Dump " ++ show l
 	
 	show (ChangeBlocs) = "ChangeBlocs"
 	
 	show (FunLabel typ lab args) = 
 		let args' = map (\(s1,a) -> s1 ++ " " ++ (show a)) args in
 		"\ndefine " ++ typ ++ " @" ++ (show lab) ++ "(" ++ (concat $ intersperse ", " args') ++ ") {"
	
	show (EndFun) = "}"


