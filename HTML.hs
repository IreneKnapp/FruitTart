module HTML where

import Data.List

import Lists

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

newlinesToParagraphs :: String -> String
newlinesToParagraphs text
    = let lines = split '\n' text
          paragraphs = filter (\line -> line /= "") lines
          escapedParagraphs = map escapeHTML paragraphs
      in case (length escapedParagraphs) of
           0 -> ""
           _ -> "<p>" ++ (intercalate "</p>\n<p>" escapedParagraphs) ++ "</p>\n"
