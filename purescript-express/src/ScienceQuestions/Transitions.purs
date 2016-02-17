module ScienceQuestions.Transitions
  ( Transitions()
  , loadTransitions
  , randomNextToken
  ) where

import           Control.Monad.Eff (Eff())
import           Control.Monad.Eff.Exception (EXCEPTION())
import           Control.Monad.Eff.Random (RANDOM(), randomInt)
import           Data.Array (length)
import           Data.Array.Unsafe (unsafeIndex)
import           Data.JSON
import qualified Data.Map as M
import           Data.Maybe (Maybe(..))
import           Data.Tuple (Tuple(..))
import           Node.Encoding (Encoding(..))
import           Node.FS (FS())
import           Node.FS.Sync (readTextFile)
import           Prelude

import ScienceQuestions.Token

-- | Transitions indicate the Tokens that can follow a given Token.
type Transitions = M.Map Token (Array Token)

-- | Load the Transitions from a serialized JSON file. We do it in two stages.
-- | First, we use `Data.JSON.decode` to get a M.Map String (Array String). Then
-- | we convert it to a List of Tuple String (Array String), convert each
-- | element to Tuple Token (Array Token), and then convert back to a M.Map.
loadTransitions :: forall eff.
  String ->  Eff (err :: EXCEPTION, fs :: FS | eff) (Maybe Transitions)
loadTransitions filename = do
  text <- readTextFile UTF8 filename
  case decode text of
    Nothing -> return Nothing
    Just ssmap -> return $ Just $ M.fromList $ map convert $ M.toList ssmap
  where
    convert (Tuple token tokens) = Tuple (read token) (map read tokens)

-- | Pick a random element from an array.
pick :: forall eff a. Array a -> Eff (random :: RANDOM | eff) a
pick xs = do
  idx <- randomInt 0 (length xs - 1)
  return $ unsafeIndex xs idx

-- | Implement a GetNextToken function using the provided Transitions (and a
-- | RANDOM effect)
randomNextToken :: forall eff.
  Transitions -> GetNextToken (random :: RANDOM | eff)
randomNextToken transitions token =
  case M.lookup token transitions of
    Just tokens -> pick tokens
    _           -> return Stop  -- this shouldn't happen, but let's be safe
