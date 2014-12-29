module TypeCheckerError where

import AbsLatte
import PrintLatte(printTree) 
--

redeclarationE :: Ident -> String
redeclarationE (Ident i) = "1. Redeclaration of function " ++ i ++ "."

noReturnE :: String
noReturnE = "2. Possibly no return statement"

badTypeE :: Type -> Type -> Expr-> String
badTypeE goodT badT e = "3. Bad type of expression " ++ (printTree e) ++ ".\nShould be " ++ 
									(printTree goodT) ++  ", but actually it is " ++ (printTree badT)

lValueE :: Expr -> String
lValueE e = "4. Expression " ++ (printTree e) ++ "\nisn't a l-value (but it should be)"

identE :: Ident -> String
identE (Ident i) = "5. No such identifier " ++ i

integerE :: Expr -> String
integerE e = "6. Expression " ++ (printTree e) ++ " is not integer (but it should be)"

voidE :: String
voidE = "7. Function type is not void"

unknownE :: String
unknownE = "8. Unknown error"

arrayE :: Expr -> String
arrayE e = "9. Expression" ++ (printTree e) ++ " is not array (but it should be)"

appE :: [Type] -> [Type] -> String
appE t1 t2 = "10. Bad application. Expected argument types are " ++ (show t1)
				++ ",\nbut actual types are " ++ (show t2)

callE :: Expr -> String
callE e = "11. Expression " ++ (printTree e) ++ " is not callable"

addE :: Expr -> Expr -> String
addE e1 e2 = "12. Cannot add " ++ (printTree e1) ++ "to" ++ (printTree e2)

dotE :: Expr -> String
dotE e = "13. Bad using of '.' in expression " ++ (printTree e)


