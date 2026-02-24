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
import NFL.Data as Data

getResponse :: Aff (Maybe { situation :: Data.Situation, clock :: String, status :: String, quarter :: Int })
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
            Right (Data.Response { situation, clock, status, quarter, home, away }) -> do
              pure $ Just { situation, clock, status, quarter }

getResponse' :: String -> Aff (Maybe Data.Response)
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

getPossession :: Data.Situation -> Data.Possession
getPossession (Data.Situation { possession }) = possession

getClock :: Maybe Data.GameInfo -> Maybe Data.Situation -> String
getClock gameInfo situation = 
  case gameInfo of
    Just { clock } -> clock
    Nothing -> case situation of
      Just (Data.Situation { situationClock }) -> situationClock
      Nothing -> "00:00"
