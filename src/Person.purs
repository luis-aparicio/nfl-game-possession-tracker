module Component.Person where

import Prelude
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH

type Input = Int
type Output = Int
type State = { count :: Int }

type Slots :: ∀ k. Row k
type Slots = ()

type Query :: ∀ k. k -> Type
type Query = Const Void

data Action
  = Initialize
  | Finalize

component
  :: ∀ m
   . MonadAff m
  => H.Component Query Input Output m
component = H.mkComponent
  { initialState: \i -> { count: i }
  , render
  , eval: H.mkEval H.defaultEval
      { initialize = Just Initialize
      , finalize = Just Finalize
      }
  }
  where
  render :: State -> H.ComponentHTML Action Slots m
  render _ = HH.text "Stubbed out"