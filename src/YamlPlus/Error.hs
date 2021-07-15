module YamlPlus.Error
  ( YamlParseError, AsYamlParseError( _YamlParseError ), asYamlParseError )
where

-- base --------------------------------

import Control.Exception  ( Exception )
import Data.Bifunctor     ( first )
import Data.Either        ( Either )
import Data.Eq            ( Eq( (==) ) )
import Data.Function      ( id )
import GHC.Stack          ( CallStack, callStack )
import Text.Show          ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- has-callstack -----------------------

import HasCallstack  ( HasCallstack( callstack ) )

-- lens --------------------------------

import Control.Lens.Lens    ( lens )
import Control.Lens.Prism   ( Prism' )
import Control.Lens.Review  ( (#) )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- yaml -------------------------------

import Data.Yaml  ( ParseException )

--------------------------------------------------------------------------------

data YamlParseError = YamlParseError ParseException CallStack
  deriving Show

instance Exception YamlParseError

instance Eq YamlParseError where
  a == b = show a ≡ show b

instance HasCallstack YamlParseError where
  callstack = lens (\ (YamlParseError _ cs) → cs)
                   (\ (YamlParseError pe _) cs → YamlParseError pe cs)

instance Printable YamlParseError where
  print = P.string ∘ show

class AsYamlParseError ε where
  _YamlParseError ∷ Prism' ε YamlParseError

instance AsYamlParseError YamlParseError where
  _YamlParseError = id

asYamlParseError ∷ AsYamlParseError ε ⇒ Either ParseException α → Either ε α
asYamlParseError =
  first ((_YamlParseError #) ∘ \ pe → YamlParseError pe callStack)


-- that's all, folks! ----------------------------------------------------------
