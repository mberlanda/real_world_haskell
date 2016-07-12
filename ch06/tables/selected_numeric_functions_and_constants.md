# Selected Numeric Functions and Constants

| Item  | Type  | Module  | Description |
|----|----|----|----|
| (+) | Num a => a -> a -> a  | Prelude | Addition  |
| (-) | Num a => a -> a -> a  | Prelude | Subtraction |
| (*) | Num a => a -> a -> a  | Prelude | Multiplication  |
| (/) | Fractional a => a -> a -> a | Prelude | Fractional division |
| (**)  | Floating a => a -> a -> a | Prelude | Raise to the power of |
| (^) | (Num a, Integral b) => a -> b -> a  | Prelude | Raise a number to a non-negative, integral power  |
| (^^)  | (Fractional a, Integral b) => a -> b -> a | Prelude | Raise a fractional number to any integral power |
| (%) | Integral a => a -> a -> Ratio a | Data.Ratio  | Ratio composition |
| (.&.) | Bits a => a -> a -> a | Data.Bits | Bitwise and |
| (.|.) | Bits a => a -> a -> a | Data.Bits | Bitwise or  |
| abs | Num a => a -> a | Prelude | Absolute value  |
| approxRational  | RealFrac a => a -> a -> Rational  | Data.Ratio  | Approximate rational composition based on fractional numerators and denominators  |
| cos | Floating a => a -> a  | Prelude | Cosine. Also provided are acos, cosh, and acosh, with the same type.  |
| div | Integral a => a -> a -> a | Prelude | Integer division always truncated down; see alsoquot  |
| fromInteger | Num a => Integer -> a | Prelude | Conversion from an Integer to any numeric type  |
| fromIntegral  | (Integral a, Num b) => a -> b | Prelude | More general conversion from any Integral to any numeric type |
| fromRational  | Fractional a => Rational -> a | Prelude | Conversion from a Rational. May be lossy. |
| log | Floating a => a -> a  | Prelude | Natural logarithm |
| logBase | Floating a => a -> a -> a | Prelude | Log with explicit base  |
| maxBound  | Bounded a => a  | Prelude | The maximum value of a bounded type |
| minBound  | Bounded a => a  | Prelude | The minimum value of a bounded type |
| mod | Integral a => a -> a -> a | Prelude | Integer modulus |
| pi  | Floating a => a | Prelude | Mathematical constant pi  |
| quot  | Integral a => a -> a -> a | Prelude | Integer division; fractional part of quotient truncated towards zero  |
| recip | Fractional a => a -> a  | Prelude | Reciprocal  |
| rem | Integral a => a -> a -> a | Prelude | Remainder of integer division |
| round | (RealFrac a, Integral b) => a -> b  | Prelude | Rounds to nearest integer |
| shift | Bits a => a -> Int -> a | Bits  | Shift left by the specified number of bits, which may be negative for a right shift.  |
| sin | Floating a => a -> a  | Prelude | Sine. Also provided are asin, sinh, and asinh, with the same type.  |
| sqrt  | Floating a => a -> a  | Prelude | Square root |
| tan | Floating a => a -> a  | Prelude | Tangent. Also provided are atan, tanh, and atanh, with the same type. |
| toInteger | Integral a => a -> Integer  | Prelude | Convert any Integral to an Integer  |
| toRational  | Real a => a -> Rational | Prelude | Convert losslessly to Rational  |
| truncate  | (RealFrac a, Integral b) => a -> b  | Prelude | Truncates number towards zero |
| xor | Bits a => a -> a -> a | Data.Bits | Bitwise exclusive or  |
