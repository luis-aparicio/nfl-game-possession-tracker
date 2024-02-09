module Component.Person where

import Prelude

import Affjax as Ajax
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Except (runExcept)
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
import Foreign (F)
import Foreign.Generic (decodeJSON, genericDecode)
import Foreign.Generic.Class (class Decode, Options, SumEncoding(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS

type InputKey = String

type Output = Int

type State =
  { time :: Int
  , situation :: Maybe Situation
  , currentPerson :: String
  , allPeople :: Array String
  , winners :: Array String
  }

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
initialState _ =
  { time: 0
  , situation: Nothing
  , currentPerson: "Nobody"
  , allPeople: [ "Billy", "Bob", "Joe", "Jane", "Carl" ]
  , winners: []
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state = HH.div_
  [ HH.div
      [ HP.style "margin: auto; width: 50%; border: 3px solid green; padding: 10px;"
      ]
      [ HH.h1
          [ HP.style "font-size: 128px; text-align: center" ]
          [ HH.text state.currentPerson ]
      ]
  , HH.div
      [ HP.style "margin: auto; width: 50%; border: 3px solid red; padding: 10px;" ]
      [ HH.ul [ HP.style "" ] (cons (HH.div [ HP.style "font-size: 24px" ] [ HH.text "Waiting: " ]) (map renderItem (filter ((/=) state.currentPerson) state.allPeople))) ]
  , HH.div
      [ HP.style "margin: auto; width: 50%; border: 3px solid blue; padding: 10px;" ]
      [ HH.ul [ HP.style "" ] (cons (HH.div [ HP.style "font-size: 24px" ] [ HH.text "Winners: " ]) (map renderItem state.winners)) ]
  , HH.div
      [ HP.style "margin: auto; width: 50%; padding: 10px;"
      ]
      [ HH.h1
          [ HP.style "font-size: 52px; text-align: center" ]
          [ HH.text $ "Next Update: " <> show (30 - state.time) ]
      ]
  ]
  where
  renderItem person = HH.li [ HP.style "font-size: 24px; list-style-type: '- '" ] [ HH.text person ]

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    currentState <- H.get
    _ <- H.subscribe =<< timer Tick
    maybeRes <- H.liftAff $ getSituation' testJSON1
    case maybeRes of
      Just (Response { situation }) -> do
        randPerson <- liftEffect $ pickRandomPerson currentState
        H.modify_ \state ->
          { time: state.time
          , situation: Just situation
          , currentPerson: randPerson
          , allPeople: state.allPeople
          , winners: state.winners
          }
      Nothing -> pure unit
    pure unit

  Tick -> do
    currentState <- H.get
    when (length currentState.winners == 4) $
      H.modify_ \state ->
        { time: 0
        , situation: state.situation
        , currentPerson: "Nobody"
        , allPeople: state.allPeople
        , winners: state.winners
        }
    if currentState.time == 10 then do
      liftEffect $ log $ show currentState
      maybeRes <- H.liftAff $ getSituation' testJSON3
      case maybeRes of
        Just (Response { situation }) -> do
          let
            oldName = maybe "" ((\(Possession { name }) -> name) <<< getPossession) currentState.situation
            (Possession { name }) = getPossession situation
          randPerson <- liftEffect $ pickRandomPerson currentState
          if oldName /= name then
            H.modify_ \state ->
              { time: 0
              , situation: Just situation
              , currentPerson: randPerson
              , allPeople: state.allPeople
              , winners: state.winners
              }
          else
            H.modify_ \state ->
              { time: 0
              , situation: Just situation
              , currentPerson: state.currentPerson
              , allPeople: state.allPeople
              , winners: state.winners
              }
          cs <- H.get
          when (map getClock cs.situation == Just "00:00") do
            H.modify_ \state ->
              { time: state.time
              , situation: state.situation
              , currentPerson: randPerson
              , allPeople: filter ((/=) state.currentPerson) state.allPeople
              , winners: cons
                  ( "Q"
                      <> show ((length state.winners) + 1)
                      <> " "
                      <> state.currentPerson
                  )
                  state.winners
              }
        Nothing -> do
          liftEffect $ log $ "No API Response"
          H.modify_ \state ->
            { time: -60
            , situation: state.situation
            , currentPerson: state.currentPerson
            , allPeople: state.allPeople
            , winners: state.winners
            }
          pure unit
    else H.modify_ \state ->
      { time: state.time + 1
      , situation: state.situation
      , currentPerson: state.currentPerson
      , allPeople: state.allPeople
      , winners: state.winners
      }

timer :: forall m a. MonadAff m => a -> m (HS.Emitter a)
timer val = do
  { emitter, listener } <- H.liftEffect HS.create
  _ <- H.liftAff $ Aff.forkAff $ forever do
    Aff.delay $ Milliseconds 1000.0
    H.liftEffect $ HS.notify listener val
  pure emitter

superBowlGameId :: String
superBowlGameId = "902a4753-72f9-4868-9d9b-193a5c41ea4d"

testGameId :: String
testGameId = "7d06369a-382a-448a-b295-6da9eab53245"

getSituation :: Aff (Maybe Response)
getSituation = do
  result <- Ajax.get
    ResponseFormat.string
    ("https://api.sportradar.us/nfl/official/trial/v7/en/games/" <> superBowlGameId <> "/boxscore.json?api_key=<API_KEY>")
  case result of
    Left err -> do
      liftEffect $ log $ "ERROR: " <> Ajax.printError err
      pure Nothing
    Right { body } ->
      case runExcept (decodeJSON body :: F Response) of
        Left err -> do
          liftEffect $ log $ "ERROR: " <> show err <> " (" <> body <> ")"
          pure Nothing
        Right res -> pure $ Just res

getSituation' :: String -> Aff (Maybe Response)
getSituation' body = do
  case runExcept (decodeJSON body :: F Response) of
    Left err -> do
      liftEffect $ log $ "ERROR: " <> show err <> " (" <> body <> ")"
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

instance showResponse :: Show Response where
  show = genericShow

derive instance genericResponse :: Generic Response _
instance decodeResponse :: Decode Response where
  decode = genericDecode decodeOptions

data Situation = Situation
  { clock :: String
  , down :: Int
  , yfd :: Int
  , possession :: Possession
  }

instance showSituation :: Show Situation where
  show = genericShow

derive instance genericSituation :: Generic Situation _
instance decodeSituation :: Decode Situation where
  decode = genericDecode decodeOptions

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
instance decodePossession :: Decode Possession where
  decode = genericDecode decodeOptions

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

decodeOptions :: Options
decodeOptions =
  { sumEncoding:
      TaggedObject
        { tagFieldName: "tag"
        , contentsFieldName: "contents"
        , constructorTagTransform: identity
        }
  , unwrapSingleConstructors: true
  , unwrapSingleArguments: true
  , fieldTransform: identity
  }