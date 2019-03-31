module Reciprocal (modularReciprocal) where

modularReciprocal :: Integral a => a -> a -> a
modularReciprocal modulus n = snd $ modularReciprocal' modulus n
  where
    -- modularReciprocal' n m = (x, y)
    -- n * x + m * y = 1
    modularReciprocal' :: Integral a => a -> a -> (a, a)
    modularReciprocal' _ 0 = notCoprimeError
    modularReciprocal' _ 1 = (0, 1)
    modularReciprocal' m l = (y, x - q * y)
      where
        (q, r) = m `divMod` l
        (x, y) = modularReciprocal' l r

    notCoprimeError :: a
    notCoprimeError =
      error $ unwords
        [ "divider"
        , parentheses $ showIntegral n
        , "must be coprime to modulus"
        , parentheses $ showIntegral modulus
        ]

    parentheses s = "(" ++ s ++ ")"

    showIntegral :: Integral a => a -> String
    showIntegral = show . toInteger
