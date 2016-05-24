-- file: ch03/Lending.hs
-- We meet a money reserve of at least 100, we return our new balance after subtracting the amount we have loaned.
lend amount balance = let reserve = 100
                          newBalance = balance - amount
                      in if balance < reserve
                         then Nothing
                         else Just newBalance

-- the where clause
lend2 amount balance = if amount < reserve * 0.5
                       then Just newBalance
                       else Nothing
    where reserve    = 100
          newBalance = balance - amount

-- conditional evaluation with gurads
lend3 amount balance
    | amount <= 0            = Nothing
    | amount > reserve * 0.5 = Nothing
    | otherwise              = Just newBalance
    where reserve    = 100
          newBalance = balance - amount 