module Text where

fromCRLF :: String -> String
fromCRLF ('\r':'\n':rest) = "\n" ++ fromCRLF rest
fromCRLF (c:rest) = [c] ++ fromCRLF rest
fromCRLF [] = []
