module ScienceQuestions.Token
  ( Token(..)
  , GetNextToken()
  , read
  , generateString
  ) where

import           Control.Monad.Eff
import           Data.Array (reverse, snoc, elemIndex, concatMap)
import           Data.Maybe (isJust)
import           Data.String (dropWhile, joinWith)
import           Prelude

-- | a Token is either the `Start` of a sentence, the `Stop` of a sentence,
-- | or some `Word` w
data Token = Start | Stop | Word String

-- | two `Token`s are equal exactly when you think they are
instance tokenEq :: Eq Token where
  eq Start Start = true
  eq Stop Stop = true
  eq (Word w1) (Word w2) = w1 == w2
  eq _ _ = false

-- | I don't actually want to compare tokens, but I would like to stick
-- | them in a Data.Map.Map, so I need an Ord instance
instance tokenOrd :: Ord Token where
  compare x y | x == y = EQ
  compare Start _ = LT
  compare Stop _ = LT
  compare (Word x) (Word y) = compare x y

instance tokenShow :: Show Token where
  show Start = "__START__"
  show Stop = "__STOP__"
  show (Word w) = w

-- | We'll need this to deserialize tokens from JSON, where they're strings.
read :: String -> Token
read "__START__" = Start
read "__STOP__"  = Stop
read w           = Word w

-- | Here a GetNextToken function is parameterized by the type of effect it
-- | requires to run. (Recall in the Haskell version that all of our effects
-- | just run in IO, so that one didn't need to be parameterized.)
type GetNextToken eff = Token -> Eff eff Token

-- | Given a starting Token, and an effectful GetNextToken function, return the
-- | array of Tokens by calling GetNextToken until Stop is reached. This is
-- | written differently from the Haskell version, which used lists like
-- |   token : (tokensFrom next getNext)
-- | It's more PureScript-y to use arrays, and then it's more array-y to `snoc`
-- | at the end, so instead we use a `loop` helper function.
tokensFrom :: forall eff. Token -> GetNextToken eff -> Eff eff (Array Token)
tokensFrom startToken getNext = loop startToken (pure [])
  where
    loop :: Token -> Eff eff (Array Token) -> Eff eff (Array Token)
    loop Stop tokens  = tokens
    loop token tokens = do
      next <- getNext token
      loop next (snoc <$> tokens <*> (pure token))

-- | Given an array of Tokens, concatenate them into a String with spaces before
-- | words but not before punctuation.
smartJoin :: Array Token -> String
smartJoin = dropWhile (== ' ') <<< joinWith "" <<< addSeparators
  where
    addSeparators = concatMap addSeparator
    addSeparator token = case token of
      Word w | w `elem` ["?", ",", "."] -> ["",  w]
      Word w                            -> [" ", w]
      _                                 -> []
    elem x xs = isJust $ elemIndex x xs

-- | Generate a String effectfully using the supplied GetNextToken function.
generateString :: forall eff. GetNextToken eff -> Eff eff String
generateString getNext = smartJoin <$> tokensFrom Start getNext
