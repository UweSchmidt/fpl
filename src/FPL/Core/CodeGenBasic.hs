-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE LambdaCase #-}

module FPL.Core.CodeGenBasic
  ( -- label generation
    label
  , newLab
  , newName

  -- code blocks for functions and closures
  , inNewCodeBlock

    -- environment manipulation
  , withStackLevel'1
  , withVar
  , withVars

    -- MaMa instruction gen
  , loadInt
  , loadBool
  , loadLit
  , mkBasic
  , getBasic
  , pushLoc
  , pushGlb
  , jump
  , branch
  , comp
  , halt
  , noop
  )
where

import FPL.Prelude
import FPL.Core.AbstractSyntax
import FPL.Core.CompEnv
import FPL.Core.CompMonad
import FPL.Core.CompState
import FPL.Core.MaMaCode
import FPL.MaMa.Instr
import FPL.MaMa.SimpleTypes (Offset)

-- ----------------------------------------
--
-- basic code gen actions

gen :: AInstr op -> Comp op ()
gen i
  = codeActive %= addAInstr i

label :: Label -> Comp op ()
label l
  = do pos <- uses codeActive lenACode
       codeActive . labMap . at l .= Just pos

newLab :: Comp op Label
newLab
  = do l <- newName "l"
       return (isoString # l)

newName :: String -> Comp op String
newName px
  = do labGen %= (+ 1)
       uses labGen ((px ++) . show)

-- create a new code block e.g. for a lambda or a local function
-- and make it the active block

newCodeBlock :: Comp op ()
newCodeBlock
  = do ac          <- use codeActive
       codeDelayed %= (ac :)
       codeActive  .= empty'

-- take active code block and append it to
-- the main instr sequence
-- restore last active code block as active

closeCodeBlock :: Comp op ()
closeCodeBlock
  = do ac         <- use codeActive
       codeDone   %= (<> ac)
       ac'        <- uses codeDelayed head
       codeActive .= ac'
       codeDelayed %= tail

inNewCodeBlock :: Comp op a -> Comp op a
inNewCodeBlock compile
  = do newCodeBlock
       r <- compile
       closeCodeBlock
       return r

-- with stack level + 1

withStackLevel'1 :: Comp op a -> Comp op a
withStackLevel'1 compile
  = locally stackLevel (+ 1) compile

withVar :: Var -> Comp op a -> Comp op a
withVar v = locally id (insVar v)

withVars :: [Var] -> Comp op a -> Comp op a
withVars vars
  = locally id extIdMap
  where
    extIdMap env
      = foldl (flip insVar) env vars

-- ----------------------------------------
--
-- "smart constructors" for code generation

loadInt :: Int -> Comp op ()
loadInt i = gen $ LoadInt i

loadBool :: Bool -> Comp op ()
loadBool b = gen $ LoadBool b

loadLit :: op -> String -> Comp op ()
loadLit op' val = gen $ LoadLit op' val

mkBasic :: Comp op ()
mkBasic = gen MkBasic

getBasic :: Comp op ()
getBasic = gen GetBasic

pushLoc :: Offset -> Comp op ()
pushLoc o = gen $ PushLoc o

pushGlb :: Offset -> Comp op ()
pushGlb o = gen $ PushGlb o

jump :: Label -> Comp op ()
jump l = gen $ Jump l

branch :: Bool -> Label -> Comp op ()
branch b l = gen $ Branch b l

comp :: op -> Comp op ()
comp op' = gen $ Comp op'

halt :: Comp op ()
halt = gen $ Halt

noop :: Comp op ()
noop = gen $ Noop

-- ----------------------------------------
