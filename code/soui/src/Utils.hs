module Utils(allCombinations) where

allCombinations :: [[a]] -> [[a]]
allCombinations (xs : xss) =
    let tailCombinations = allCombinations xss
    in [x : tailComb | x <- xs, tailComb <- tailCombinations]
allCombinations [] = [[]]
