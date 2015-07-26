data BookInfo = Book Int String [String]
  deriving (Show)

bookID (Book id title authors) = id
bookTitle (Book 2 title authors) = title
bookAuthors (Book id title authors) = authors

data MagazineInfo = Magazine Int String [String]
  deriving (Show)
