{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

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

import Text.Pretty

-- ----------------------------------------
--
-- 2. simplest basic value: an Int or a Bool

-- ----------------------------------------

type MaMa1   = MaMa   Op1 Val1
type Code1   = Code   Op1
type MState1 = MState Op1 Val1

-- ----------------------------------------

data Val1 = VInt  ! Int
          | VBool ! Bool

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

data Op1 = --binary ops
           Addi | Subi | Muli | Divi
         | EQi  | NEi
         | LSi  | LEi  | GEi  | GRi
         | And  | Or
           -- unary ops
         | Neg
         | Not
         | B2I  | I2B
           -- conversion of literals
         | ToInt | ToBool

instance ALU Op1 where
  alu = \case
    -- binary ops
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
    And  -> ex2Bool (&&)
    Or   -> ex2Bool (||)

    -- unary ops
    Neg  -> ex1  asInt negate
    Not  -> ex1  asBool not
    B2I  -> ex1' asBool asInt  fromEnum
    I2B  -> ex1' asInt  asBool (== 0)

    -- default
    _    -> exAbort

--  fromLiteral :: Op1 -> MaMa1 ()
  fromLiteral = \case
    ToInt  -> pushLitInt
    ToBool -> pushLitBool
    _      -> pushLitAbort

-- ----------------------------------------

exec1 :: Code1 -> IO MState1
exec1 = flip execMaMaProg opts
  where
    opts = defaultOptions & mamaTrace .~ True

-- ----------------------------------------
--
-- pretty printing and test output

instance Pretty Val1 where
  pretty = \case
    VInt  i -> show i
    VBool b -> pretty b

instance Pretty Op1 where
  pretty  =  map toLower . show

deriving instance Show Op1
deriving instance Show Val1

-- ----------------------------------------
