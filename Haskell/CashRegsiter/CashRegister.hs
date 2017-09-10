module CashRegister where

type Price = Double

type Quantity = Int

multiply :: Price -> Quantity -> Price
multiply p q = p * (asDouble q)
        where asDouble = fromIntegral
