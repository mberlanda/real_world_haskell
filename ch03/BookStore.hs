-- file: ch03/BookStore.hs

-- defining a new data type
data BookInfo = Book Int String [String]
                deriving (Show)

-- type synonyms
type CustomerID = Int
type ReviewBody = String 

data BookReview = BookReview BookInfo CustomerID ReviewBody

-- type synonym to create a shorter name for verbose type
type BookRecord = (BookInfo, BookReview)

-- billing information
type CardHolder = String
type CardNumber = String
type Address = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)

myInfo = Book 9876543210 "abcdefghijklmnopqrstuvwxyz" ["aaa", "bb", "c"]