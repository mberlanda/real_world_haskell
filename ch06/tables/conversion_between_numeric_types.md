# Conversion Between Numeric Types

| Source Type| Destination Type  ||||
|---|-----|-----|-----|-----|
| | Double, Float | Int, Word | Integer | Rational  |
|Double, Float | fromRational . toRational | truncate *  | truncate *  | toRational  |
|Int, Word | fromIntegral  | fromIntegral  | fromIntegral  | fromIntegral  |
|Integer | fromIntegral  | fromIntegral  | N/A | fromIntegral  |
|Rational  | fromRational  | truncate *  | truncate *  | N/A |

Instead of truncate*, you could also use round, ceiling, or floor