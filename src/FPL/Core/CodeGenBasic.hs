-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE LambdaCase #-}

module FPL.Core.CodeGenBasic
  ( gen
  , lab
  , newLab
  , newName
  , inNewCodeBlock

  , withStackLevel'1
  , withId
  , withIds
  )
where

import FPL.Prelude
import FPL.Core.CompEnv
import FPL.Core.CompMonad
import FPL.Core.CompState
import FPL.Core.MaMaCode

import qualified Data.Sequence as L (length, singleton)
import qualified Data.Map      as M

-- import Text.Pretty

-- ----------------------------------------
--
-- basic code gen actions

gen :: AInstr op -> Comp op ()
gen instr
  = codeActive . instrSeq %= (<> L.singleton instr)

lab :: Label -> Comp op ()
lab l
  = do pos <- uses (codeActive . instrSeq) L.length
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
inNewCodeBlock comp
  = do newCodeBlock
       r <- comp
       closeCodeBlock
       return r

-- with stack level + 1

withStackLevel'1 :: Comp op a -> Comp op a
withStackLevel'1 comp
  = locally stackLevel (+ 1) comp

withId :: Ident -> VarDescr -> Comp op a -> Comp op a
withId i d = withIds $ [(i, d)]

withIds :: [(Ident, VarDescr)] -> Comp op a -> Comp op a
withIds ids comp
  = locally idMap extIdMap comp
  where
    extIdMap idm
      = foldl (\ res (i', d') -> M.insert i' d' res) idm ids

-- ----------------------------------------
