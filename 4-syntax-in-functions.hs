-- This function should print a single digit number as English text, or "unknown" if it's out of the range 0-9
englishDigit :: Int -> String
englishDigit x
    | x > (-1) && x < 10 = show x
    | otherwise          = "unknown"

-- given a tuple, divide fst by snd, using pattern matching. 
-- it should return undefined for division by zero
divTuple :: (Eq a, Fractional a) => (a, a) -> a
divTuple (x, y) = case y of 0 -> undefined
                            y -> x / y

-- if the first three numbers in a list are all zero, return True
-- original featured bad pattern on LHS
threeZeroList :: [Int] -> Bool
threeZeroList (0:0:0:xs) = True
threeZeroList _          = False
