-- the compile state

module FPL.Core.CompState
  ( CompState
  , codeActive
  , codeDelayed
  , codeDone
  , labGen
  )
where

import FPL.Prelude
-- import FPL.MaMa.SimpleTypes
import FPL.Core.MaMaCode

-- import Text.Pretty

-- ----------------------------------------

data CompState op
  = CS { _codeActive  :: ACode op
       , _codeDelayed :: [ACode op]
       , _codeDone    :: ACode op
       , _labGen   :: Int
       }
    deriving (Show)

instance Empty (CompState op) where
  empty'
    = CS { _codeActive  = empty'
         , _codeDelayed = []
         , _codeDone    = empty'
         , _labGen      = 0
         }
  null' (CS ac dcs dc l)
    = null' ac && null dcs && null' dc && l == 0

codeActive :: Lens' (CompState op) (ACode op)
codeActive k s = (\ n -> s { _codeActive = n}) <$> k (_codeActive s)

codeDelayed :: Lens' (CompState op) [ACode op]
codeDelayed k s = (\ n -> s { _codeDelayed = n}) <$> k (_codeDelayed s)

codeDone :: Lens' (CompState op) (ACode op)
codeDone k s = (\ n -> s { _codeDone = n}) <$> k (_codeDone s)

labGen :: Lens' (CompState op) Int
labGen k s = (\ n -> s { _labGen = n}) <$> k (_labGen s)

-- ----------------------------------------
