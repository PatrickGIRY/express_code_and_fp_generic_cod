module CashRegister2 where

import Data.Monoid

newtype Price = Price Double
    deriving (Eq, Show)

instance Monoid Price where
    mempty = Price 0
    mappend (Price p1) (Price p2) = Price (p1 + p2)

add :: Price -> Price -> Price
add = mappend

total :: [Price] -> Price
total = mconcat


newtype Quantity = Quantity Int
    deriving (Show)

multiply :: Price -> Quantity -> Price
multiply (Price p) (Quantity q) = Price (p * (asDouble q))
        where asDouble = fromIntegral
