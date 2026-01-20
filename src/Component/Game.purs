module Component.Game where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Array (length, (!!), filter, cons, snoc, deleteAt, mapWithIndex, any)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (trim)
import Effect (Effect)
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Random (randomInt)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick, onValueInput, onKeyDown) as HP
import Halogen.HTML.Properties (style, value, placeholder) as HP
import Web.UIEvent.KeyboardEvent (key)
import Halogen.Subscription as HS
import NFL.Data as NFL
import NFL.API as NFLAPI
import Game.Winners as Winners

type State =
  { time :: Int
  , situation :: Maybe NFL.Situation
  , quarter :: Int
  , status :: String
  , currentPerson :: String
  , allPeople :: Array String
  , winners :: Winners.QuarterWinners
  , ballPossessionHistory :: Array String
  , renderSurrenderButton :: Boolean
  , showGamePage :: Boolean
  , newParticipantName :: String
  , gameInfo :: Maybe NFL.GameInfo
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
  , gameInfo: Nothing
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state = 
  if state.showGamePage then
    renderGame state
  else
    renderSetup state
  where
  renderGame state'@{ winners: { q1, q2, q3, q4, ot }, gameInfo, situation } = 
    HH.div
      [ HP.style $ baseContainerStyle <> "background: #E8E8E8; min-height: 100vh; padding: 20px;"
      ]
      [ renderGameHeader gameInfo state'.quarter state'.status (NFLAPI.getClock gameInfo situation)
      , HH.div
          [ HP.style "max-width: 1400px; margin: 0 auto; display: grid; grid-template-columns: 1fr 1fr; gap: 15px; margin-top: 15px; align-items: stretch;"
          ]
          [ renderCurrentPerson state'.currentPerson state'.renderSurrenderButton
          , renderWaitingList state'.currentPerson state'.allPeople
          ]
      , renderWinnersFooter { q1, q2, q3, q4, ot }
      , renderUpdateTimer state'.status state'.time
      ]
  
  renderSetup state' = 
    HH.div
      [ HP.style $ baseContainerStyle <> "background: #E8E8E8; min-height: 100vh; padding: 40px 20px; display: flex; align-items: center; justify-content: center;"
      ]
      [ HH.div
          [ HP.style "max-width: 700px; width: 100%; background: rgba(255, 255, 255, 0.95); border-radius: 30px; padding: 50px; box-shadow: 0 8px 32px rgba(0, 0, 0, 0.15); border: 2px solid rgba(192, 192, 192, 0.6);"
          ]
          [ HH.h1
              [ HP.style "font-size: 56px; margin: 0 0 40px 0; text-align: center; font-weight: bold; text-shadow: 2px 2px 4px rgba(0,0,0,0.2); color: #8B00FF;"
              ]
              [ HH.text "🏈 Game Setup" ]
          , HH.div
              [ HP.style "margin-bottom: 30px; display: flex; gap: 10px;"
              ]
              [ HH.input
                  [ HP.placeholder "Enter participant name"
                  , HP.value state'.newParticipantName
                  , HP.onValueInput UpdateParticipantName
                  , HP.onKeyDown \ev -> if key ev == "Enter" then AddParticipant else Initialize
                  , HP.style "flex: 1; font-size: 20px; padding: 15px 20px; border: 2px solid rgba(192, 192, 192, 0.6); border-radius: 15px; background: rgba(255,255,255,0.9); color: #1a1a1a; font-weight: bold; ::placeholder { color: rgba(0,0,0,0.5); }"
                  ]
              , HH.button
                  [ HP.style "font-size: 20px; padding: 15px 30px; background: #32CD32; color: white; border: 2px solid rgba(255, 255, 255, 0.3); border-radius: 15px; cursor: pointer; font-weight: bold; box-shadow: 0 4px 15px rgba(0,0,0,0.3); transition: transform 0.2s;"
                  , HP.onClick \_ -> AddParticipant
                  ]
                  [ HH.text "Add" ]
              ]
          , HH.div
              [ HP.style "margin: 30px 0; min-height: 200px; background: rgba(255, 255, 255, 0.7); border: 2px solid rgba(74, 144, 226, 0.5); padding: 30px; border-radius: 20px;"
              ]
              (if length state'.allPeople == 0 then
                [ HH.p
                    [ HP.style "font-size: 20px; text-align: center; opacity: 0.8; margin: 0; color: #1a1a1a;"
                    ]
                    [ HH.text "No participants added yet. Add names above." ]
                ]
              else
                [ HH.h2
                    [ HP.style "font-size: 28px; margin: 0 0 20px 0; text-align: center; font-weight: bold; text-shadow: 1px 1px 2px rgba(0,0,0,0.2); color: #1a1a1a;"
                    ]
                    [ HH.text $ "Participants (" <> show (length state'.allPeople) <> ")" ]
                , HH.div
                    [ HP.style "display: flex; flex-direction: column; gap: 12px;"
                    ]
                    (mapWithIndex (\idx name -> 
                      HH.div
                        [ HP.style "background: rgba(255, 255, 255, 0.8); border-radius: 12px; padding: 15px 20px; display: flex; justify-content: space-between; align-items: center; border: 1px solid rgba(192, 192, 192, 0.5);"
                        ]
                        [ HH.span 
                            [ HP.style "font-size: 22px; font-weight: bold;"
                            ]
                            [ HH.text name ]
                        , HH.button
                            [ HP.style "font-size: 16px; padding: 8px 16px; background: #FF1493; color: white; border: 1px solid rgba(255, 255, 255, 0.3); border-radius: 8px; cursor: pointer; font-weight: bold; box-shadow: 0 2px 8px rgba(0,0,0,0.4);"
                            , HP.onClick \_ -> RemoveParticipant idx
                            ]
                            [ HH.text "Remove" ]
                        ]
                    ) state'.allPeople)
                ])
          , if length state'.allPeople >= 2 then
              HH.button
                [ HP.style "font-size: 28px; padding: 20px 50px; margin-top: 30px; width: 100%; background: #FFD700; color: #1a1a1a; border: 3px solid rgba(255, 255, 255, 0.5); border-radius: 20px; cursor: pointer; font-weight: bold; box-shadow: 0 6px 20px rgba(0,0,0,0.4); text-transform: uppercase; letter-spacing: 2px; transition: transform 0.2s;"
                , HP.onClick \_ -> StartGame
                ]
                [ HH.text "🚀 Start Game" ]
            else
              HH.div_
                [ HH.button
                    [ HP.style "font-size: 28px; padding: 20px 50px; margin-top: 30px; width: 100%; background: rgba(255,255,255,0.2); color: rgba(255,255,255,0.5); border: 2px solid rgba(255,255,255,0.3); border-radius: 20px; cursor: not-allowed; font-weight: bold;"
                    ]
                    [ HH.text "Start Game (Need 2+ participants)" ]
                , HH.p
                    [ HP.style "font-size: 18px; color: #1a1a1a; margin-top: 15px; text-align: center; font-weight: bold;"
                    ]
                    [ HH.text "Add at least 2 participants to start the game" ]
                ]
          ]
      ]
  
  baseContainerStyle = "font-family: 'Arial', sans-serif; color: #1a1a1a; "
  
  renderGameHeader gameInfo quarter status clock = 
    HH.div
      [ HP.style "background: rgba(255, 255, 255, 0.9); border-radius: 20px; padding: 20px; margin-bottom: 15px; box-shadow: 0 8px 32px rgba(0, 0, 0, 0.15); border: 2px solid rgba(192, 192, 192, 0.5);"
      ]
      [ case gameInfo of
          Just { homeTeam, awayTeam, homeScore, awayScore, quarterScores } ->
            HH.div_
              [ HH.div
                  [ HP.style "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px; flex-wrap: wrap;"
                  ]
                  [ HH.div
                      [ HP.style "flex: 1; text-align: center; min-width: 180px;"
                      ]
                      [ HH.h2
                          [ HP.style "font-size: 28px; font-weight: bold; margin: 0; text-shadow: 1px 1px 2px rgba(0,0,0,0.2); color: #1a1a1a;"
                          ]
                          [ HH.text awayTeam ]
                      , HH.div
                          [ HP.style "font-size: 56px; font-weight: bold; margin: 8px 0; text-shadow: 2px 2px 4px rgba(0,0,0,0.2); color: #8B00FF;"
                          ]
                          [ HH.text $ show awayScore ]
                      ]
                  , HH.div
                      [ HP.style "padding: 0 30px; text-align: center;"
                      ]
                      [ HH.div
                          [ HP.style "font-size: 20px; font-weight: bold; margin-bottom: 6px; color: #FFD700;"
                          ]
                          [ HH.text $ "Q" <> show quarter ]
                      , HH.div
                          [ HP.style "font-size: 26px; font-weight: bold; color: #1a1a1a;"
                          ]
                          [ HH.text clock ]
                      , HH.div
                          [ HP.style "font-size: 14px; margin-top: 4px; opacity: 0.9; color: #FF1493; font-weight: bold;"
                          ]
                          [ HH.text $ if status == "inprogress" then "LIVE" else status ]
                      ]
                  , HH.div
                      [ HP.style "flex: 1; text-align: center; min-width: 180px;"
                      ]
                      [ HH.h2
                          [ HP.style "font-size: 28px; font-weight: bold; margin: 0; text-shadow: 1px 1px 2px rgba(0,0,0,0.2); color: #1a1a1a;"
                          ]
                          [ HH.text homeTeam ]
                      , HH.div
                          [ HP.style "font-size: 56px; font-weight: bold; margin: 8px 0; text-shadow: 2px 2px 4px rgba(0,0,0,0.2); color: #8B00FF;"
                          ]
                          [ HH.text $ show homeScore ]
                      ]
                  ]
              , HH.div
                  [ HP.style "display: flex; justify-content: center; gap: 20px; margin-top: 15px; padding-top: 15px; border-top: 2px solid rgba(192, 192, 192, 0.6); flex-wrap: wrap;"
                  ]
                  (map renderQuarterScore quarterScores)
              ]
          Nothing ->
            HH.div
              [ HP.style "text-align: center;"
              ]
              [ HH.h2
                  [ HP.style "font-size: 32px; margin: 0; color: #1a1a1a;"
                  ]
                  [ HH.text $ "Quarter " <> show quarter ]
              , HH.div
                  [ HP.style "font-size: 24px; margin-top: 10px; color: #1a1a1a;"
                  ]
                  [ HH.text clock ]
              ]
      ]
  
  renderQuarterScore { quarter, home, away } =
    HH.div
      [ HP.style "text-align: center;"
      ]
      [ HH.div
          [ HP.style "font-size: 14px; opacity: 0.8; margin-bottom: 5px; color: #1a1a1a;"
          ]
          [ HH.text $ "Q" <> show quarter ]
      , HH.div
          [ HP.style "font-size: 20px; font-weight: bold; color: #1a1a1a;"
          ]
          [ HH.text $ show away <> " - " <> show home ]
      ]
  
  renderCurrentPerson currentPerson renderSurrenderButton =
    HH.div
      [ HP.style "background: rgba(255, 255, 255, 0.95); border-radius: 20px; padding: 30px 25px; text-align: center; box-shadow: 0 8px 32px rgba(0, 0, 0, 0.15); border: 3px solid #FFD700; display: flex; flex-direction: column; justify-content: space-between;"
      ]
      [ HH.div
          [ HP.style "flex: 1; display: flex; flex-direction: column; justify-content: center;"
          ]
          [ HH.div
              [ HP.style "font-size: 20px; font-weight: bold; margin-bottom: 15px; opacity: 0.95; text-transform: uppercase; letter-spacing: 2px; text-shadow: 1px 1px 2px rgba(0,0,0,0.2); color: #8B00FF;"
              ]
              [ HH.text "Current Possession" ]
          , HH.h1
              [ HP.style "font-size: 110px; font-weight: bold; margin: 15px 0; text-shadow: 4px 4px 8px rgba(0,0,0,0.5), 0 0 20px rgba(255, 215, 0, 0.5); color: #FFD700; word-break: break-word; line-height: 1.1;"
              ]
              [ HH.text currentPerson ]
          ]
      , if renderSurrenderButton then
          HH.button
            [ HP.style "font-size: 18px; padding: 15px 30px; margin-top: 20px; background: #FF1493; color: white; border: 2px solid rgba(255, 255, 255, 0.4); border-radius: 50px; cursor: pointer; font-weight: bold; box-shadow: 0 4px 15px rgba(0,0,0,0.4); transition: transform 0.2s; text-transform: uppercase; letter-spacing: 1px;"
            , HP.onClick \_ -> SurrenderPossession
            ]
            [ HH.text "🏈 Surrender Possession" ]
        else
          HH.text ""
      ]
  
  renderWinnersFooter { q1, q2, q3, q4, ot } =
    HH.div
      [ HP.style "max-width: 1400px; margin: 15px auto 0; background: rgba(255, 255, 255, 0.9); border-radius: 15px; padding: 12px 20px; box-shadow: 0 4px 16px rgba(0, 0, 0, 0.1); border: 1px solid rgba(192, 192, 192, 0.5);"
      ]
      [ HH.div
          [ HP.style "display: flex; align-items: center; justify-content: center; gap: 20px; flex-wrap: wrap;"
          ]
          [ HH.span
              [ HP.style "font-size: 18px; font-weight: bold; color: #8B00FF; margin-right: 10px;"
              ]
              [ HH.text "🏆 Quarter Winners:" ]
          , renderWinnerCard "Q1" q1
          , renderWinnerCard "Q2" q2
          , renderWinnerCard "Q3" q3
          , renderWinnerCard "Q4" q4
          , renderWinnerCard "OT" ot
          ]
      ]
  
  renderWaitingList currentPerson allPeople =
    HH.div
      [ HP.style "background: rgba(255, 255, 255, 0.9); border-radius: 20px; padding: 20px; box-shadow: 0 8px 32px rgba(0, 0, 0, 0.15); border: 2px solid rgba(192, 192, 192, 0.6); display: flex; flex-direction: column; max-height: calc(100vh - 200px);"
      ]
      [ HH.h2
          [ HP.style "font-size: 24px; font-weight: bold; margin: 0 0 12px 0; text-align: center; text-shadow: 1px 1px 2px rgba(0,0,0,0.2); color: #4A90E2;"
          ]
          [ HH.text "⏳ Waiting" ]
      , HH.div
          [ HP.style "flex: 1; overflow-y: auto; min-height: 0; display: flex; flex-direction: column; gap: 8px;"
          ]
          (if length (filter ((/=) currentPerson) allPeople) == 0 then
            [ HH.div
                [ HP.style "text-align: center; padding: 15px; opacity: 0.6; font-size: 16px; color: #1a1a1a;"
                ]
                [ HH.text "Everyone has had a turn!" ]
            ]
          else
            map renderWaitingItem (filter ((/=) currentPerson) allPeople))
      ]
  
  renderWinnerCard label winner =
    HH.div
      [ HP.style "background: rgba(255, 255, 255, 0.8); border-radius: 8px; padding: 8px 12px; text-align: center; border: 1px solid rgba(192, 192, 192, 0.5); display: inline-flex; flex-direction: column; align-items: center; min-width: 80px;"
      ]
      [ HH.div
          [ HP.style "font-size: 11px; opacity: 0.8; margin-bottom: 4px; font-weight: bold; color: #1a1a1a;"
          ]
          [ HH.text label ]
      , case winner of
          Just name ->
            HH.div
              [ HP.style "font-size: 14px; font-weight: bold; color: #FFD700; text-shadow: 1px 1px 2px rgba(0,0,0,0.2); line-height: 1.2;"
              ]
              [ HH.text name ]
          Nothing ->
            HH.div
              [ HP.style "font-size: 12px; opacity: 0.5; color: #1a1a1a;"
              ]
              [ HH.text "—" ]
      ]
  
  renderWaitingItem item =
    HH.div
      [ HP.style "background: rgba(255, 255, 255, 0.8); border-radius: 8px; padding: 12px; font-size: 18px; font-weight: bold; text-align: center; border: 1px solid rgba(192, 192, 192, 0.5); color: #1a1a1a;"
      ]
      [ HH.text item ]
  
  renderUpdateTimer status time =
    HH.div
      [ HP.style "max-width: 1400px; margin: 15px auto 0; background: rgba(255, 255, 255, 0.9); border-radius: 15px; padding: 12px 20px; text-align: center; border: 1px solid rgba(192, 192, 192, 0.5); box-shadow: 0 4px 16px rgba(0, 0, 0, 0.1); color: #1a1a1a;"
      ]
      [ HH.div
          [ HP.style "font-size: 24px; font-weight: bold;"
          ]
          [ HH.text $ 
              if status == "scheduled" || status == "closed"
              then "⏸️ Game " <> (if status == "closed" then "Ended" else "Not Running")
              else "⏱️ Next Update: " <> show (45 - time) <> "s"
          ]
      ]

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    currentState <- H.get
    when currentState.showGamePage do
      _ <- H.subscribe =<< timer Tick
      maybeRes <- H.liftAff NFLAPI.getResponse
      case maybeRes of
        Just { situation, status, gameInfo } -> do
          randPerson <- liftEffect $ pickRandomPerson currentState
          H.modify_ \state -> state
            { situation = Just situation
            , currentPerson = randPerson
            , ballPossessionHistory = cons randPerson state.ballPossessionHistory
            , status = status
            , gameInfo = gameInfo
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
      maybeRes <- H.liftAff NFLAPI.getResponse
      case maybeRes of
        Just { situation, status, quarter, gameInfo } -> do
          let
            oldName = maybe "" ((\(NFL.Possession { name }) -> name) <<< NFLAPI.getPossession) oldState.situation
            (NFL.Possession { name }) = NFLAPI.getPossession situation
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
              , gameInfo = gameInfo
              }
          else
            H.modify_ _
              { time = 0
              , situation = Just situation
              , quarter = quarter
              , status = status
              , gameInfo = gameInfo
              }
          when ((oldState.quarter /= quarter) && Winners.checkQuarter quarter oldState.winners) do
            H.modify_ \state -> state
              { currentPerson = randPerson
              , allPeople = filter ((/=) state.currentPerson) state.allPeople
              , winners = Winners.setWinner state.currentPerson state.winners
              , ballPossessionHistory = cons randPerson state.ballPossessionHistory
              , renderSurrenderButton = true
              }
          when (status == "closed") do
            H.modify_ \state -> state
              { currentPerson = "Game Over"
              , allPeople = filter ((/=) state.currentPerson) state.allPeople
              , winners = Winners.setWinner state.currentPerson state.winners
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
