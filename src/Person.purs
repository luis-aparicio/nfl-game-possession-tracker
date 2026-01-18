module Component.Person where

import Prelude

import Affjax as Ajax
import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web as AffjaxWeb
import Control.Monad.Rec.Class (forever)
import Data.Array (length, (!!), filter, cons)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Random (randomInt)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Parser (jsonParser)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick) as HP
import Halogen.HTML.Properties (style) as HP
import Halogen.Subscription as HS

type InputKey = String

type Output = Int

type State =
  { time :: Int
  , situation :: Maybe Situation
  , currentPerson :: String
  , allPeople :: Array String
  , winners :: Array String
  , ballPossessionHistory :: Array String
  , renderSurrenderButton :: Boolean
  }
  
data Action = Initialize | Tick | SurrenderPossession

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
initialState _ =
  { time: 0
  , situation: Nothing
  , currentPerson: "Nobody"
  , allPeople:
      [ -- All people here 
      ]
  , winners: []
  , ballPossessionHistory: []
  , renderSurrenderButton: true
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state = HH.div_
  [ HH.div
      [ HP.style "margin: auto; width: 50%; border: 3px solid green; padding: 10px; text-align: center;"
      ]
      [ HH.h1
          [ HP.style "font-size: 128px; text-align: center" ]
          [ HH.text state.currentPerson ]
      , if state.renderSurrenderButton then
          HH.button
            [ HP.style "font-size: 24px; margin: 0 auto; display: block;"
            , HP.onClick \_ -> SurrenderPossession
            ]
            [ HH.text "Surrender Possession" ]
        else
          HH.text ""
      ]
  , HH.div
      [ HP.style "margin: auto; width: 50%; border: 3px solid red; padding: 10px;" ]
      [ HH.ul [ HP.style "text-align: center" ] (cons (HH.div [ HP.style "font-size: 24px" ] [ HH.text "Waiting: " ]) (map renderItem (filter ((/=) state.currentPerson) state.allPeople))) ]
  , HH.div
      [ HP.style "margin: auto; width: 50%; border: 3px solid blue; padding: 10px;" ]
      [ HH.ul [ HP.style "text-align: center" ] (cons (HH.div [ HP.style "font-size: 24px" ] [ HH.text "Winners: " ]) (map renderItem state.winners)) ]
  , HH.div
      [ HP.style "margin: auto; width: 50%; padding: 10px;"
      ]
      [ HH.h1
          [ HP.style "font-size: 52px; text-align: center" ]
          [ HH.text $ "Next Update: " <> show (45 - state.time) ]
      ]
  ]
  where
  renderItem person = HH.div [ HP.style "font-size: 24px" ] [ HH.text person ]

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    currentState <- H.get
    _ <- H.subscribe =<< timer Tick
    maybeRes <- H.liftAff getSituation
    case maybeRes of
      Just (Response { situation }) -> do
        randPerson <- liftEffect $ pickRandomPerson currentState
        H.modify_ \state -> state
          { situation = Just situation
          , currentPerson = randPerson
          , ballPossessionHistory = cons randPerson state.ballPossessionHistory
          }
      Nothing -> pure unit
    pure unit

  Tick -> do
    currentState <- H.get
    when (length currentState.winners == 5) $
      H.modify_ \state -> state
        { time = 0
        , currentPerson = "Game Over"
        }

    if currentState.time == 45 then do
      liftEffect $ log $ show currentState
      maybeRes <- H.liftAff getSituation
      case maybeRes of
        Just (Response { situation }) -> do
          let
            oldName = maybe "" ((\(Possession { name }) -> name) <<< getPossession) currentState.situation
            (Possession { name }) = getPossession situation
          randPerson <- liftEffect $ pickRandomPerson currentState
          if oldName /= name then
            H.modify_ _
              { time = 0
              , situation = Just situation
              , currentPerson = randPerson
              , ballPossessionHistory = cons randPerson currentState.ballPossessionHistory
              , renderSurrenderButton = true
              }
          else
            H.modify_ _
              { time = 0
              , situation = Just situation
              }
          cs <- H.get
          when (map getClock cs.situation == Just "00:00" || map getClock cs.situation == Just "15:00") do
            H.modify_ \state -> state
              { currentPerson = randPerson
              , allPeople = filter ((/=) state.currentPerson) state.allPeople
              , winners = cons
                  ( "Q"
                      <> show ((length state.winners) + 1)
                      <> " "
                      <> state.currentPerson
                  )
                  state.winners
              , ballPossessionHistory = cons randPerson state.ballPossessionHistory
              , renderSurrenderButton = true
              }
        Nothing -> do
          liftEffect $ log $ "No API Response"
          H.modify_ _ { time = -60 }
          pure unit
    else H.modify_ \state -> state { time = state.time + 1 }

  SurrenderPossession -> do
    currentState <- H.get
    randPerson <- liftEffect $ pickRandomPerson currentState
    H.modify_ \state -> state 
      { renderSurrenderButton = false 
      , currentPerson = randPerson
      , ballPossessionHistory = cons randPerson state.ballPossessionHistory
      }

timer :: forall m a. MonadAff m => a -> m (HS.Emitter a)
timer val = do
  { emitter, listener } <- H.liftEffect HS.create
  _ <- H.liftAff $ Aff.forkAff $ forever do
    Aff.delay $ Milliseconds 1000.0
    H.liftEffect $ HS.notify listener val
  pure emitter

-- 2025
superBowlGameId :: String
superBowlGameId = "ca9d8f84-8e7b-4ee7-a310-54c2e3ca4edc"

testGameId :: String
testGameId = "7d06369a-382a-448a-b295-6da9eab53245"

getSituation :: Aff (Maybe Response)
getSituation = do
  result <- Ajax.get
    AffjaxWeb.driver
    ResponseFormat.string
    ("https://api.sportradar.us/nfl/official/trial/v7/en/games/" <> superBowlGameId <> "/boxscore.json?api_key=<API_KEY>")
  case result of
    Left err -> do
      liftEffect $ log $ "ERROR: " <> Ajax.printError err
      pure Nothing
    Right { body } ->
      case jsonParser body of
        Left err -> do
          liftEffect $ log $ "ERROR: JSON parse failed: " <> err <> " (" <> body <> ")"
          pure Nothing
        Right json ->
          case decodeJson json of
            Left err -> do
              liftEffect $ log $ "ERROR: Decode failed: " <> show err <> " (" <> body <> ")"
              pure Nothing
            Right res -> pure $ Just res

getSituation' :: String -> Aff (Maybe Response)
getSituation' body = do
  case jsonParser body of
    Left err -> do
      liftEffect $ log $ "ERROR: JSON parse failed: " <> err <> " (" <> body <> ")"
      pure Nothing
    Right json ->
      case decodeJson json of
        Left err -> do
          liftEffect $ log $ "ERROR: Decode failed: " <> show err <> " (" <> body <> ")"
          pure Nothing
        Right res -> pure $ Just res

pickRandomPerson :: State -> Effect String
pickRandomPerson state =
  case state.allPeople of
    [ x ] -> pure x
    _ -> do
      let maxIndex = length state.allPeople - 1
      index <- randomInt 0 maxIndex
      let randPers = fromMaybe "Nobody" $ state.allPeople !! index
      if state.currentPerson == randPers then pickRandomPerson state
      else pure randPers

data Response = Response
  { situation :: Situation
  }

instance Show Response where
  show = genericShow

derive instance Generic Response _
instance DecodeJson Response where
  decodeJson = genericDecodeJson

data Situation = Situation
  { clock :: String
  , down :: Int
  , yfd :: Int
  , possession :: Possession
  }

instance Show Situation where
  show = genericShow

derive instance Generic Situation _
instance DecodeJson Situation where
  decodeJson = genericDecodeJson

getPossession :: Situation -> Possession
getPossession (Situation { possession }) = possession

getClock :: Situation -> String
getClock (Situation { clock }) = clock

newtype Possession = Possession
  { id :: String
  , name :: String
  , market :: String
  , alias :: String
  , sr_id :: String
  }

instance showPossession :: Show Possession where
  show = genericShow

derive instance genericPossession :: Generic Possession _
instance DecodeJson Possession where
  decodeJson = genericDecodeJson

-- {
--   "situation": {
--       "clock": "00:00",
--       "down": 1,
--       "yfd": 10,
--       "possession": {
--           "id": "3d08af9e-c767-4f88-a7dc-b920c6d2b4a8",
--           "name": "Seahawks",
--           "market": "Seattle",
--           "alias": "SEA",
--           "sr_id": "sr:competitor:4430"
--       },
--       "location": {
--           "id": "3d08af9e-c767-4f88-a7dc-b920c6d2b4a8",
--           "name": "Seahawks",
--           "market": "Seattle",
--           "alias": "SEA",
--           "sr_id": "sr:competitor:4430",
--           "yardline": 40
--       }
--   }
-- }

testJSON1 :: String
testJSON1 =
  """
{ "situation": {
      "clock": "25:00",
      "down": 1,
      "yfd": 10,
      "possession": {
          "id": "3d08af9e-c767-4f88-a7dc-b920c6d2b4a8",
          "name": "Seahawks",
          "market": "Seattle",
          "alias": "SEA",
          "sr_id": "sr:competitor:4430"
      },
      "location": {
          "id": "3d08af9e-c767-4f88-a7dc-b920c6d2b4a8",
          "name": "Seahawks",
          "market": "Seattle",
          "alias": "SEA",
          "sr_id": "sr:competitor:4430",
          "yardline": 40
      }
  }
}
"""

testJSON2 :: String
testJSON2 =
  """
{ "situation": {
      "clock": "13:00",
      "down": 1,
      "yfd": 10,
      "possession": {
          "id": "3d08af9e-c767-4f88-a7dc-b920c6d2b4a8",
          "name": "Chiefs",
          "market": "Seattle",
          "alias": "SEA",
          "sr_id": "sr:competitor:4430"
      },
      "location": {
          "id": "3d08af9e-c767-4f88-a7dc-b920c6d2b4a8",
          "name": "Seahawks",
          "market": "Seattle",
          "alias": "SEA",
          "sr_id": "sr:competitor:4430",
          "yardline": 40
      }
  }
}
"""

testJSON3 :: String
testJSON3 =
  """
{ "situation": {
      "clock": "00:00",
      "down": 1,
      "yfd": 10,
      "possession": {
          "id": "3d08af9e-c767-4f88-a7dc-b920c6d2b4a8",
          "name": "Seahawks",
          "market": "Seattle",
          "alias": "SEA",
          "sr_id": "sr:competitor:4430"
      },
      "location": {
          "id": "3d08af9e-c767-4f88-a7dc-b920c6d2b4a8",
          "name": "Seahawks",
          "market": "Seattle",
          "alias": "SEA",
          "sr_id": "sr:competitor:4430",
          "yardline": 40
      }
  }
}
"""