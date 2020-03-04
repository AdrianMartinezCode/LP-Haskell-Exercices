data Index = Magror | Corpulencia | Sobrepes | Obesitat | Morbida


getIndexByIMC :: Float -> Index
getIndexByIMC imc
	| imc < 18 				= Magror
	| imc >= 18 && imc < 25 = Corpulencia
	| imc >= 25 && imc < 30 = Sobrepes
	| imc >= 30 && imc < 40 = Obesitat
	| otherwise 			= Morbida

getStringNameByIndex :: Index -> String
getStringNameByIndex Magror = "magror"
getStringNameByIndex Corpulencia = "corpulencia normal"
getStringNameByIndex Sobrepes = "sobrepes"
getStringNameByIndex Obesitat = "obesitat"
getStringNameByIndex Morbida = "obesitat morbida"

getIMCByMassHeight :: Int -> Float -> Float
getIMCByMassHeight m h =  fromIntegral m / (h ^ 2)

splitByChar :: [Char] -> Char -> [String]
splitByChar (x:xs) c
	| x == c = 

getStringToChar :: [Char] -> Char -> String
getStringToChar (x)