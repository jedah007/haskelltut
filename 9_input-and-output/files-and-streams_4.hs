-- Program that continuously reads a line and
-- then tells us if the line is a palindrome or not:
main = interact respondPalindromes

respondPalindromes contents = unlines (map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") (lines contents))
    where   isPalindrome xs = xs == reverse xs

respondPalindromes' = unlines . map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") . lines
    where   isPalindrome xs = xs == reverse xs
