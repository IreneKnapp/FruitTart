module HTML where

escapeHTML :: String -> String
escapeHTML ('&':rest) = "&amp;" ++ escapeHTML rest
escapeHTML ('<':rest) = "&lt;" ++ escapeHTML rest
escapeHTML ('>':rest) = "&gt;" ++ escapeHTML rest
escapeHTML (a:rest) = [a] ++ escapeHTML rest
escapeHTML "" = ""

escapeAttribute :: String -> String
escapeAttribute ('&':rest) = "&amp;" ++ escapeAttribute rest
escapeAttribute ('<':rest) = "&lt;" ++ escapeAttribute rest
escapeAttribute ('>':rest) = "&gt;" ++ escapeAttribute rest
escapeAttribute ('\'':rest) = "&apos;" ++ escapeAttribute rest
escapeAttribute ('"':rest) = "&quot;" ++ escapeAttribute rest
escapeAttribute (a:rest) = [a] ++ escapeAttribute rest
escapeAttribute "" = ""
