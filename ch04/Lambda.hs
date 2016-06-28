-- file: ch04/Lambda.hs
safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _ = Nothing

-- Anonymous (lambda) function implementation
unsafeHead :: [a] -> a
unsafeHead = \(x:_) -> x