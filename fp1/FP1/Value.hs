{-# LANGUAGE LambdaCase #-}

-- Value for MaMa with Int and Bool as basic values

module FP1.Value
where

import FPPL.Prelude
import FPPL.MaMa.ALU
import FPPL.MaMa.Code
import FPPL.MaMa.MachineState
import FPPL.MaMa.Monad
import FPPL.MaMa.Options
import FPPL.MaMa.Run
import FPPL.MaMa.SimpleTypes ()
import FPPL.MaMa.Value

-- ----------------------------------------
--
-- 2. simplest basic value: an Int or a Bool

-- ----------------------------------------

data Val1 = VInt  ! Int
          | VBool ! Bool
          deriving (Show)

instance BasicValue Val1 where
  asInt = prism
          VInt
          (\ case
              VInt y -> Right y
              x      -> Left  x
          )

  asBool = prism
           VBool
           (\ case
               VBool y -> Right y
               x       -> Left  x
           )

-- ----------------------------------------

data Op1 = Addi | Subi | Muli | Divi
         | EQi | NEi
         | LSi | LEi | GEi | GRi
         | Neg
         | And | Or
         | Not
         deriving (Show)

instance ALU Op1 where
  alu = \case
    Addi -> exAddInt
    Subi -> exSubInt
    Muli -> exMulInt
    Divi -> exDivInt
    EQi  -> exEqInt
    NEi  -> exNeInt
    LSi  -> exLtInt
    LEi  -> exLeInt
    GEi  -> exGeInt
    GRi  -> exGtInt
    Neg  -> ex1 asInt negate
    And  -> ex2Bool (&&)
    Or   -> ex2Bool (||)
    Not  -> ex1 asBool not

-- ----------------------------------------

exec1 :: Code1 -> IO MState1
exec1 = flip execMaMaProg opts
  where
    opts = defaultOptions & mamaTrace .~ True

-- ----------------------------------------

instance Pretty Val1 where
  pretty = \case
    VInt  i -> show i
    VBool b -> pretty b

instance Pretty Op1 where
  pretty  = take 8 . (++ replicate 8 ' ') . map toLower . show
  pretty' = reverse . dropWhile isSpace . reverse . pretty

-- ----------------------------------------

type MaMa1 = MaMa Op1 Val1
type Code1 = Code Op1
type MState1 = MState Op1 Val1

-- ----------------------------------------
