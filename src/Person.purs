module Component.Person where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (error)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Affjax as Ajax 
import Affjax.ResponseFormat as ResponseFormat
import Affjax.RequestBody as RequestBody
import Effect (Effect)
import Effect.Aff (launchAff_)
import Foreign (Foreign)

type InputKey = String
type Output = Int
type State = Int

data Action = Initialize | Tick

component :: forall query input output m. MonadAff m => H.Component query input output m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

initialState :: forall input. input -> State
initialState _ = 0

render :: forall m. State -> H.ComponentHTML Action () m
render seconds = HH.text ("You have been here for " <> show seconds <> " seconds")

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    _ <- H.subscribe =<< timer Tick
    pure unit

  Tick ->
    H.modify_ \state -> state + 1

timer :: forall m a. MonadAff m => a -> m (HS.Emitter a)
timer val = do
  { emitter, listener } <- H.liftEffect HS.create
  _ <- H.liftAff $ Aff.forkAff $ forever do
    Aff.delay $ Milliseconds 1000.0
    H.liftEffect $ HS.notify listener val
  pure emitter

superBowlGameId :: String 
superBowlGameId = "902a4753-72f9-4868-9d9b-193a5c41ea4d"
