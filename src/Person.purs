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
  , quarter :: Int
  , status :: String
  , currentPerson :: String
  , allPeople :: Array String
  , winners :: QuarterWinners
  , ballPossessionHistory :: Array String
  , renderSurrenderButton :: Boolean
  , showGamePage :: Boolean
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
  , status: "scheduled"
  , quarter: 1
  , currentPerson: "Nobody"
  , allPeople: []
  , winners: { q1: Nothing, q2: Nothing, q3: Nothing, q4: Nothing, ot: Nothing }
  , ballPossessionHistory: []
  , renderSurrenderButton: true
  , showGamePage: false
  , newParticipantName: ""
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state = 
  if state.showGamePage then
    renderGame state
  else
    renderSetup state
  where
  renderGame state'@{ winners: { q1, q2, q3, q4, ot } } = HH.div_
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
        [ HH.ul [ HP.style "text-align: center" ] (cons (HH.div [ HP.style "font-size: 24px" ] [ HH.text "Winners: " ]) (map renderItem' [ q1, q2, q3, q4, ot ])) ]
    , HH.div
        [ HP.style "margin: auto; width: 50%; padding: 10px;"
        ]
        [ HH.h1
            [ HP.style "font-size: 52px; text-align: center" ]
            [ HH.text $ 
                if state.status == "scheduled" || state.status == "closed"
                then "Not Running" 
                else "Next Update: " <> show (45 - state'.time) 
            ]
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
  
  renderItem item = HH.div [ HP.style "font-size: 24px" ] [ HH.text item ]
  renderItem' item' = case item' of
    Just item -> HH.div [ HP.style "font-size: 24px" ] [ HH.text item ]
    Nothing -> HH.div_ []

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    currentState <- H.get
    when currentState.showGamePage do
      _ <- H.subscribe =<< timer Tick
      maybeRes <- H.liftAff getResponse
      case maybeRes of
        Just { situation, status } -> do
          randPerson <- liftEffect $ pickRandomPerson currentState
          H.modify_ \state -> state
            { situation = Just situation
            , currentPerson = randPerson
            , ballPossessionHistory = cons randPerson state.ballPossessionHistory
            , status = status
            }
        Nothing -> pure unit

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
      H.modify_ \state -> state { showGamePage = true }
      handleAction Initialize

  Tick -> do
    oldState <- H.get
    if oldState.status == "scheduled" || oldState.status == "closed"
    then pure unit
    else if oldState.time == 45 then do
      liftEffect $ log $ show oldState
      maybeRes <- H.liftAff getResponse
      case maybeRes of
        Just { situation, status, quarter } -> do
          let
            oldName = maybe "" ((\(Possession { name }) -> name) <<< getPossession) oldState.situation
            (Possession { name }) = getPossession situation
          randPerson <- liftEffect $ pickRandomPerson oldState
          if oldName /= name then
            H.modify_ _
              { time = 0
              , situation = Just situation
              , quarter = quarter
              , status = status 
              , currentPerson = randPerson
              , ballPossessionHistory = cons randPerson oldState.ballPossessionHistory
              , renderSurrenderButton = true
              }
          else
            H.modify_ _
              { time = 0
              , situation = Just situation
              , quarter = quarter
              , status = status
              }
          when ((oldState.quarter /= quarter) && checkQuarter quarter oldState.winners) do
            H.modify_ \state -> state
              { currentPerson = randPerson
              , allPeople = filter ((/=) state.currentPerson) state.allPeople
              , winners = setWinner state.currentPerson state.winners
              , ballPossessionHistory = cons randPerson state.ballPossessionHistory
              , renderSurrenderButton = true
              }
          when (status == "closed") do
            H.modify_ \state -> state
              { currentPerson = "Game Over"
              , allPeople = filter ((/=) state.currentPerson) state.allPeople
              , winners = setWinner state.currentPerson state.winners
              , ballPossessionHistory = cons randPerson state.ballPossessionHistory
              , renderSurrenderButton = false
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

getResponse :: Aff (Maybe { situation :: Situation, clock :: String, status :: String, quarter :: Int })
getResponse = do
  config <- liftEffect Config.getConfig
  let gameId = Config.getGameId config
      apiUrl = "/api/nfl/official/trial/v7/en/games/" <> gameId <> "/boxscore.json?api_key=" <> config.apiKey
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
            Right (Response { situation, clock, status, quarter }) -> pure $ Just { situation, clock, status, quarter }

getResponse' :: String -> Aff (Maybe Response)
getResponse' body = do
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

setWinner :: String -> QuarterWinners -> QuarterWinners
setWinner winner winners = case winners of
  { q1: Nothing, q2: _, q3: _, q4: _, ot: _ } -> winners { q1 = Just winner }
  { q1: Just _, q2: Nothing, q3: _, q4: _, ot: _ } -> winners { q2 = Just winner }
  { q1: Just _, q2: Just _, q3: Nothing, q4: _, ot: _ } -> winners { q3 = Just winner }
  { q1: Just _, q2: Just _, q3: Just _, q4: Nothing, ot: _ } -> winners { q4 = Just winner }
  { q1: Just _, q2: Just _, q3: Just _, q4: Just _, ot: Nothing } -> winners { ot = Just winner }
  _ -> winners

checkQuarter :: Int -> QuarterWinners -> Boolean
checkQuarter quarter winners = case quarter of
  2 -> winners.q1 /= Nothing
  3 -> winners.q2 /= Nothing
  4 -> winners.q3 /= Nothing
  5 -> winners.q4 /= Nothing
  _ -> false

data Response = Response
  { clock :: String
  , situation :: Situation
  , quarter :: Int
  , status :: String
  }
derive instance Generic Response _

instance Show Response where
  show = genericShow

instance DecodeJson Response where
  decodeJson json = caseJsonObject
    (Left $ TypeMismatch "object")
    (\obj -> do
      situation <- obj .: "situation"
      clock <- obj .: "clock"
      quarter <- obj .: "quarter"
      status <- obj .: "status"
      pure $ Response { situation, clock, quarter, status })
    json

data Situation = Situation
  { situationClock :: String
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
      situationClock <- obj .: "clock"
      down <- obj .: "down"
      yfd <- obj .: "yfd"
      possession <- obj .: "possession"
      pure $ Situation { situationClock, down, yfd, possession }
    )
    json

getPossession :: Situation -> Possession
getPossession (Situation { possession }) = possession

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
      pure $ Possession { id, name, market, alias, sr_id }
    )
    json

type QuarterWinners = 
  { q1 :: Maybe String
  , q2 :: Maybe String
  , q3 :: Maybe String
  , q4 :: Maybe String
  , ot :: Maybe String
  }