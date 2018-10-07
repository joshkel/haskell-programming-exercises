sayHello :: String -> IO ()
sayHello x =
    putStrLn ("Hello, " ++ x ++ "!")

triple x = x * 3

circleArea x = pi * x * x

printInc n = print plusTwo
    where plusTwo = n + 2

printInc2 n = let plusTwo = n + 2
              in print plusTwo

whereLet1   = x * 3 + y
    where x = 3
          y = 1000

waxOn = x * 5
    where x = y ^ 2
          y = z + 8
          z = 7

waxOff x = triple x
