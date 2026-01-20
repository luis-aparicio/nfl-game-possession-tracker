module NFL.API where

import Prelude

import Affjax as Ajax
import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web as AffjaxWeb
import Config as Config
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import NFL.Types as Types

getResponse :: Aff (Maybe { situation :: Types.Situation, clock :: String, status :: String, quarter :: Int, gameInfo :: Maybe Types.GameInfo })
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
            Right (Types.Response { situation, clock, status, quarter, home, away, scoring }) -> do
              let gameInfo = buildGameInfo home away scoring clock
              pure $ Just { situation, clock, status, quarter, gameInfo }

buildGameInfo :: Maybe Types.Team -> Maybe Types.Team -> Maybe Types.Scoring -> String -> Maybe Types.GameInfo
buildGameInfo home away scoring clock = do
  (Types.Team { name: homeName }) <- home
  (Types.Team { name: awayName }) <- away
  (Types.Scoring { home_points, away_points, periods }) <- scoring
  let quarterScores = map (\(Types.Period { number, home_points: h, away_points: a }) -> { quarter: number, home: h, away: a }) periods
  pure { homeTeam: homeName, awayTeam: awayName, homeScore: home_points, awayScore: away_points, clock, quarterScores }

getResponse' :: String -> Aff (Maybe Types.Response)
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

getPossession :: Types.Situation -> Types.Possession
getPossession (Types.Situation { possession }) = possession

getClock :: Maybe Types.GameInfo -> Maybe Types.Situation -> String
getClock gameInfo situation = 
  case gameInfo of
    Just { clock } -> clock
    Nothing -> case situation of
      Just (Types.Situation { situationClock }) -> situationClock
      Nothing -> "00:00"
