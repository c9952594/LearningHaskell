module MyLib (
    splitString, 
    splitValues
) where

splitString :: (Char -> Bool) -> String -> [String]
splitString predicate [] = []
splitString predicate xs = let (word, remaining) = break predicate xs 
                               strippedRemaining = dropWhile predicate remaining
                           in
                               word : splitString predicate strippedRemaining

splitValues :: (Char -> Bool) -> (Char -> Bool) -> (String -> a) -> String -> [[a]]
splitValues lineDelimiter valueDelimiter transform contents = [splitValues line | line <- splitString lineDelimiter contents]
                                                              where splitValues line = [transform value | value <- splitString valueDelimiter line]