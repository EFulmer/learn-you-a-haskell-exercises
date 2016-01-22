{-
 - Create a type called Validation
 - The type constructor takes one parameter
 - There are two Values: 
 -   Success, which takes that parameter and
 -   Fail String, which represents a failure, and a reason for that failure
 -}
data Validation a = Fail String | Success a
    deriving Show -- for debugging

-- in GHC 7.10 we need to make Validation an Applicative
-- which requires making it a Functor
instance Functor Validation where
    fmap f (Fail s) = Fail s
    fmap f (Success a) = Success (f a)

instance Applicative Validation where
    pure = Success
    (Fail s) <*> _ = Fail s
    _ <*> (Fail s) = Fail s
    (Success a) <*> (Success b) = Success $ a b

-- Make the Validation a Monad
instance Monad Validation where
    return = Success
    Fail s >>= _ = Fail s
    (Success a) >>= f = f a
    
{-
 - Create a function, positiveCheck, which takes a number and returns a successful Validation if it's positive, 
 - and a failed Validation with a String message if not.
 -}
positiveCheck :: (Num a, Ord a) => a -> Validation a
positiveCheck x
    | x > 0     = Success x
    | x == 0    = Fail "x == 0"
    | otherwise = Fail "x < 0"

{-
 - Create a function, evenCheck, which returns a successful Validation if it's even,
 - and a failed Validation with a string message if it's odd
 -}
evenCheck :: (Integral a)  =>  a -> Validation a
evenCheck x = case x `mod` 2 of
                   0 -> Success x
                   1 -> Fail "x is odd"
{-
 - Write a function which uses positiveCheck and evenCheck to make sure a number is both positive and even
 -}
positiveAndEvenCheck :: (Num a, Ord a, Integral a) => a -> Validation a
positiveAndEvenCheck x = positiveCheck x >>= evenCheck
