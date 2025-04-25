module Claw.Internal.Numeric (
    mulRII,
) where

-- | Multiply an integral with a real, and round back to integral.
mulRII :: (RealFrac x, Integral n) => x -> n -> n
mulRII x = round . (* x) . fromIntegral
