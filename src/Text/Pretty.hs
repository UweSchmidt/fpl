{-# LANGUAGE DefaultSignatures #-}

module Text.Pretty
where

-- ----------------------------------------

class Pretty v where
  pretty  :: v -> String

  default pretty :: (Show v) => v -> String
  pretty = show

-- ----------------------------------------

spaces :: Int -> String
spaces n = replicate n ' '

alignL :: Int -> String -> String
alignL n xs = xs ++ spaces (n - length xs)

alignR :: Int -> String -> String
alignR n xs = spaces (n - length xs) ++ xs

alignC :: Int -> String -> String
alignC n xs = spaces n1 ++ xs ++ spaces n2
  where
    l  = n - length xs
    n1 = l `div` 2
    n2 = l - n1

type Fmt  = (String, String -> String)
type Fmts = [Fmt]

fmtCell :: Fmt -> String -> String
fmtCell (px, f) xs  = px ++ f xs

fmtRow :: Fmts -> [String] -> String
fmtRow (f1 : fs) (xs : xss) = fmtCell f1 xs ++ fmtRow fs xss
fmtRow []              xss  = unwords xss
fmtRow _                []  = []

-- ----------------------------------------

header :: Char -> String -> [String]
header c xs = ["", xs, map (const c) xs, ""]

-- ----------------------------------------
