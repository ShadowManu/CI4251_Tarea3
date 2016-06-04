import Text.ParserCombinators.Parsec

import System.Environment (getArgs)
import Data.List (intercalate)

-- UTILITY PARSERS

eol :: Parser String
eol = try (string "\n\r")
      <|> try (string "\r\n")
      <|> string "\n"
      <|> string "\r"
      <?> "end of line"

codeUntilEol :: Parser String
codeUntilEol = many1 $ noneOf "\n\r"

textUntilEol :: Parser String
textUntilEol = do
  notFollowedBy $ string "> "
  many1 $ noneOf "\n\r"

mainHeader :: Parser String
mainHeader = do
  _ <- char '*'
  spaces
  content <- many1 $ noneOf "\n"
  return $ "<h1>" ++ content ++ "</h1>"

secondHeader :: Parser String
secondHeader = do
  _ <- char '#'
  spaces
  content <- many1 $ noneOf "\n"
  return $ "<h2>" ++ content ++ "</h2>"

hsLine :: Parser String
hsLine = do
  _ <- string "> "
  codeUntilEol

hsCode :: Parser String
hsCode = do
  ls <- endBy1 hsLine eol
  return $ "<code>" ++ intercalate "<br />" ls ++ "</code>"

paragraph :: Parser String
paragraph = do
  ls <- endBy1 textUntilEol eol
  return $ "<p>" ++ unwords ls  ++ "</p>"

element :: Parser String
element = try mainHeader
          <|> try secondHeader
          <|> try hsCode
          <|> try paragraph

lhs :: Parser String
lhs = do
  elems <- sepEndBy element (many1 eol)
  eof
  return $ concat elems

parseAndShow :: FilePath -> String -> String
parseAndShow filename input = case parse lhs filename input of
  Left c -> "Error: " ++ show c
  Right r -> r

nameToHtml :: String -> String
nameToHtml ".lhs" = ".html"
nameToHtml (x:xs) = x : nameToHtml xs
nameToHtml "" = ""

parseFile :: FilePath -> IO ()
parseFile path = readFile path >>= writeFile (nameToHtml path) . parseAndShow path

main :: IO ()
main = getArgs >>= mapM_ parseFile
