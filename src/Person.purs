module Component.Person where

import Prelude

import Affjax as Ajax
import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web as AffjaxWeb
import Control.Monad.Rec.Class (forever)
import Data.Array (length, (!!), filter, cons, snoc, deleteAt, mapWithIndex, any)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (trim)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Random (randomInt)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Core (caseJsonObject)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick, onValueInput, onKeyDown) as HP
import Halogen.HTML.Properties (style, value, placeholder) as HP
import Web.UIEvent.KeyboardEvent (key)
import Halogen.Subscription as HS
import Config as Config

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
  , gameStarted :: Boolean
  , newParticipantName :: String
  }
  
data Action 
  = Initialize 
  | Tick 
  | SurrenderPossession
  | UpdateParticipantName String
  | AddParticipant
  | RemoveParticipant Int
  | StartGame

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
  , allPeople: []
  , winners: []
  , ballPossessionHistory: []
  , renderSurrenderButton: true
  , gameStarted: false
  , newParticipantName: ""
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state = 
  if state.gameStarted then
    renderGame state
  else
    renderSetup state
  where
  renderGame state' = HH.div_
    [ HH.div
        [ HP.style "margin: auto; width: 50%; border: 3px solid green; padding: 10px; text-align: center;"
        ]
        [ HH.h1
            [ HP.style "font-size: 128px; text-align: center" ]
            [ HH.text state'.currentPerson ]
        , if state'.renderSurrenderButton then
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
        [ HH.ul [ HP.style "text-align: center" ] (cons (HH.div [ HP.style "font-size: 24px" ] [ HH.text "Waiting: " ]) (map renderItem (filter ((/=) state'.currentPerson) state'.allPeople))) ]
    , HH.div
        [ HP.style "margin: auto; width: 50%; border: 3px solid blue; padding: 10px;" ]
        [ HH.ul [ HP.style "text-align: center" ] (cons (HH.div [ HP.style "font-size: 24px" ] [ HH.text "Winners: " ]) (map renderItem state'.winners)) ]
    , HH.div
        [ HP.style "margin: auto; width: 50%; padding: 10px;"
        ]
        [ HH.h1
            [ HP.style "font-size: 52px; text-align: center" ]
            [ HH.text $ "Next Update: " <> show (45 - state'.time) ]
        ]
    ]
  
  renderSetup state' = HH.div
    [ HP.style "margin: auto; width: 80%; max-width: 600px; padding: 20px; text-align: center;"
    ]
    [ HH.h1
        [ HP.style "font-size: 48px; margin-bottom: 30px;" ]
        [ HH.text "Game Setup" ]
    , HH.div
        [ HP.style "margin-bottom: 20px;" ]
        [ HH.input
            [ HP.placeholder "Enter participant name"
            , HP.value state'.newParticipantName
            , HP.onValueInput UpdateParticipantName
            , HP.onKeyDown \ev -> if key ev == "Enter" then AddParticipant else Initialize
            , HP.style "font-size: 20px; padding: 10px; width: 100%; max-width: 400px;"
            ]
        , HH.button
            [ HP.style "font-size: 20px; padding: 10px 20px; margin-left: 10px;"
            , HP.onClick \_ -> AddParticipant
            ]
            [ HH.text "Add" ]
        ]
    , HH.div
        [ HP.style "margin: 20px 0; min-height: 200px; border: 2px solid #ccc; padding: 20px; border-radius: 8px;"
        ]
        (if length state'.allPeople == 0 then
          [ HH.p
              [ HP.style "font-size: 18px; color: #666;" ]
              [ HH.text "No participants added yet. Add names above." ]
          ]
        else
          [ HH.h2
              [ HP.style "font-size: 24px; margin-bottom: 15px;" ]
              [ HH.text $ "Participants (" <> show (length state'.allPeople) <> ")" ]
          , HH.ul
              [ HP.style "list-style: none; padding: 0; text-align: left; max-width: 400px; margin: 0 auto;" ]
              (mapWithIndex (\idx name -> 
                HH.li
                  [ HP.style "font-size: 20px; padding: 8px; margin: 5px 0; background: #f0f0f0; border-radius: 4px; display: flex; justify-content: space-between; align-items: center;"
                  ]
                  [ HH.span [] [ HH.text name ]
                  , HH.button
                      [ HP.style "font-size: 16px; padding: 5px 10px; background: #ff4444; color: white; border: none; border-radius: 4px; cursor: pointer;"
                      , HP.onClick \_ -> RemoveParticipant idx
                      ]
                      [ HH.text "Remove" ]
                  ]
              ) state'.allPeople)
          ])
    , if length state'.allPeople >= 2 then
        HH.button
          [ HP.style "font-size: 24px; padding: 15px 40px; margin-top: 20px; background: #4CAF50; color: white; border: none; border-radius: 8px; cursor: pointer;"
          , HP.onClick \_ -> StartGame
          ]
          [ HH.text "Start Game" ]
      else
        HH.button
          [ HP.style "font-size: 24px; padding: 15px 40px; margin-top: 20px; background: #cccccc; color: #666; border: none; border-radius: 8px; cursor: not-allowed;"
          ]
          [ HH.text "Start Game (Need 2+ participants)" ]
    , if length state'.allPeople < 2 then
        HH.p
          [ HP.style "font-size: 16px; color: #ff4444; margin-top: 10px;" ]
          [ HH.text "Add at least 2 participants to start the game" ]
      else
        HH.text ""
    ]
  
  renderItem person = HH.div [ HP.style "font-size: 24px" ] [ HH.text person ]
  

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    currentState <- H.get
    when currentState.gameStarted do
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

  UpdateParticipantName name -> do
    H.modify_ \state -> state { newParticipantName = name }

  AddParticipant -> do
    currentState <- H.get
    let trimmedName = trim currentState.newParticipantName
    when (trimmedName /= "" && not (any (_ == trimmedName) currentState.allPeople)) do
      H.modify_ \state -> state
        { allPeople = snoc state.allPeople trimmedName
        , newParticipantName = ""
        }

  RemoveParticipant idx -> do
    H.modify_ \state -> state
      { allPeople = fromMaybe state.allPeople $ deleteAt idx state.allPeople
      }

  StartGame -> do
    currentState <- H.get
    when (length currentState.allPeople >= 2) do
      H.modify_ \state -> state { gameStarted = true }
      -- Trigger Initialize to start the game
      handleAction Initialize

  Tick -> do
    currentState <- H.get
    unless currentState.gameStarted $ pure unit
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

getSituation :: Aff (Maybe Response)
getSituation = do
  config <- liftEffect Config.getConfig
  let gameId = Config.getGameId config
  let apiUrl = "/api/nfl/official/trial/v7/en/games/" <> gameId <> "/boxscore.json?api_key=" <> config.apiKey
  result <- Ajax.get
    AffjaxWeb.driver
    ResponseFormat.string
    apiUrl
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
derive instance Generic Response _

instance Show Response where
  show = genericShow

instance DecodeJson Response where
  decodeJson json = caseJsonObject
    (Left $ TypeMismatch "object")
    (\obj -> do
      situation <- obj .: "situation"
      pure $ Response { situation })
    json

data Situation = Situation
  { clock :: String
  , down :: Int
  , yfd :: Int
  , possession :: Possession
  }
derive instance Generic Situation _

instance Show Situation where
  show = genericShow

instance DecodeJson Situation where
  decodeJson json = caseJsonObject
    (Left $ TypeMismatch "object")
    (\obj -> do
      clock <- obj .: "clock"
      down <- obj .: "down"
      yfd <- obj .: "yfd"
      possession <- obj .: "possession"
      pure $ Situation { clock, down, yfd, possession })
    json

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

instance Show Possession where
  show = genericShow

derive instance Generic Possession _
instance DecodeJson Possession where
  decodeJson json = caseJsonObject
    (Left $ TypeMismatch "object")
    (\obj -> do
      id <- obj .: "id"
      name <- obj .: "name"
      market <- obj .: "market"
      alias <- obj .: "alias"
      sr_id <- obj .: "sr_id"
      pure $ Possession { id, name, market, alias, sr_id })
    json

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