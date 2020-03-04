--Feu un programa que llegeixi un nom de persona i saludi graciosament.

--Entrada

--L’entrada és un nom de persona.

--Sortida

--Si el nom és masculí, cal escriure ‘Hola maco!’. Si el nom és femení, cal escriure ‘Hola maca!’. Suposeu que tots els noms que acaben en A són femenins.

--Observació

--Per resoldre aquest problema en Haskell, feu una funció main i escolliu el compilador GHC.


main = do
	nom <- getLine
	let b = last nom == 'a' || last nom == 'A'
	if b then do
		putStrLn "Hola maca!"
		return ()
	else do
		putStrLn "Hola maco!"
		return ()

