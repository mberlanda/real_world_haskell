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

-- Pattern Matching
bookID      (Book id title authors) = id
bookTitle   (Book id title authors) = title
bookAuthors (Book id title authors) = authors

-- Wild Card Pattern
nicerID      (Book id _ _)      = id
nicerTitle   (Book _ title _)   = title
nicerAuthors (Book _ _ authors) = authors

-- Record Syntax: define data types instead of boilerplate _
data Customer = Customer {
      customerID :: CustomerID
    , customerName :: String
    , customerAddress :: Address
    } deriving(Show)

customer1 = Customer 271828 "J.R. Hacker"
            ["255 Syntax Ct",
             "Milpitas, CA 95134",
             "USA"]
customer2 = Customer {
              customerID = 271828
            , customerAddress = ["1048576 Disk Drive",
                                 "Milpitas, CA 95134",
                                 "USA"]
            , customerName = "Jane Q. Citizen"
            }