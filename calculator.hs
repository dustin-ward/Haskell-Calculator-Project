-- 3740 Calculator Project
-- Author: Dustin Ward
--
-- This is a haskell implementation of a postfix calculator made for my 
-- CPSC3740 Programming Languages class.
--
-- Running the program will prompt the user to input a postfix expression.
-- The program will then compute the expression and return the answer.
-- To calculate another expression, the program needs to be run again.

--Helper funtion for modulus operation
modulu :: Double -> Double -> Double
modulu a b = fromIntegral $ mod (round b) (round a)

--Helper function for gcd function
gcd1 :: Double -> Double -> Double
gcd1 a b = fromIntegral $ gcd (round b) (round a)

--Helper function for lcm
lcm1 :: Double -> Double -> Double
lcm1 a b = fromIntegral $ lcm (round b) (round a)

--Helper function for ceiling
ceil :: Double -> Double
ceil a = fromIntegral $ ceiling a

--Helper function for floor
flor :: Double -> Double
flor a = fromIntegral $ floor a

--Calculator function
-- Takes input string and recursively applies the correct version
--of 'f'. 'f' applies various operators as defined below.
calculate :: String -> Double
calculate [] = 0.0
calculate xs = head (foldl f [] (words xs))
  where
    f (x:y:xs) "+" = (x+y):xs             -- x y +  ->  x + y
    f (x:y:xs) "-" = (y-x):xs             -- x y -  ->  x - y
    f (x:y:xs) "*" = (x*y):xs             -- x y *  ->  x * y
    f (x:y:xs) "/" = (y/x):xs             -- x y /  ->  x / y
    f (x:y:xs) "%" = (modulu x y):xs      -- x y %  ->  x % y
    f (x:y:xs) "^" = (y**x):xs            -- x y ^  ->  x ** y  
    f (x:y:xs) "gcd" = (gcd1 x y):xs      -- x y gcd->  gcd x y
    f (x:y:xs) "lcm" = (lcm1 x y):xs      -- x y lcm->  lcm x y
    f (x:y:xs) "log" = (logBase y x):xs   -- x y log->  log Base(x) of y
    f (x:xs) "sqrt" = (sqrt x):xs         -- x sqrt ->  sqrt x
    f (x:xs) "sin" = (sin x):xs           -- x sin  ->  sin x
    f (x:xs) "cos" = (cos x):xs           -- x cos  ->  cos x
    f (x:xs) "tan" = (tan x):xs           -- x tan  ->  tan x
    f (x:xs) "abs" = (abs x):xs           -- x abs  ->  abs x
    f (x:xs) "ceil" = (ceil x):xs         -- x ceil ->  ceiling x
    f (x:xs) "floor" = (flor x):xs        -- x floor->  floor x
    f xs y = (read y):xs

--Main function
-- Takes input from user and runs calculate. Prints output
calculator = do 
putStrLn "Enter postfix expression: "
input <- getLine
print $ calculate input