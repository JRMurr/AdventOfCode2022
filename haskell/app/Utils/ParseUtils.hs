module Utils.ParseUtils where

import Text.Megaparsec
import Text.Megaparsec.Char
import Utils.Mod

parseInt :: Parser Int
parseInt = read <$> some numberChar <?> "integer"
