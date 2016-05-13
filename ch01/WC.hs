-- file: ch01/WC.hs

main = interact wordCount
-- count the number of lines
--    where wordCount input = show(length (lines input)) ++ "\n"
-- count the number of words
--    where wordCount input = show(length (words input)) ++ "\n"
-- count the number of chars
    where wordCount input = show(length $ input) ++ "\n"