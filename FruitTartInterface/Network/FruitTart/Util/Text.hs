module Network.FruitTart.Util.Text where

fromCRLF :: String -> String
fromCRLF ('\r':'\n':rest) = "\n" ++ fromCRLF rest
fromCRLF (c:rest) = [c] ++ fromCRLF rest
fromCRLF [] = []


wordWrap :: String -> Int -> String
wordWrap body width =
    let wordWrap' remainingBody
            | length remainingBody <= width = remainingBody
            | otherwise = let splitPoint = findSplitPoint remainingBody
                          in take splitPoint remainingBody
                             ++ "\n"
                             ++ (wordWrap' $ drop (splitPoint+1) remainingBody)
        nth n list = head $ drop n list
        findSplitPoint line = findSplitPoint' line width
        findSplitPoint' line n = if nth n line == ' '
                                   then n
                                   else if n == 0
                                     then width
                                     else findSplitPoint' line $ n - 1
    in wordWrap' body
