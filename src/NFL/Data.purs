module NFL.Data where

import Prelude

import Data.Argonaut.Core (caseJsonObject)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:), (.:?))
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)

data Response = Response
  { clock :: String
  , situation :: Situation
  , quarter :: Int
  , status :: String
  , home :: Maybe Team
  , away :: Maybe Team
  , scoring :: Maybe Scoring
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
      home <- obj .:? "home"
      away <- obj .:? "away"
      scoring <- obj .:? "scoring"
      pure $ Response { situation, clock, quarter, status, home, away, scoring })
    json

newtype Team = Team
  { name :: String
  , alias :: String
  }
derive instance Generic Team _
instance Show Team where
  show = genericShow
instance DecodeJson Team where
  decodeJson json = caseJsonObject
    (Left $ TypeMismatch "object")
    (\obj -> do
      name <- obj .: "name"
      alias <- obj .: "alias"
      pure $ Team { name, alias })
    json

newtype Scoring = Scoring
  { home_points :: Int
  , away_points :: Int
  , periods :: Array Period
  }
derive instance Generic Scoring _
instance Show Scoring where
  show = genericShow
instance DecodeJson Scoring where
  decodeJson json = caseJsonObject
    (Left $ TypeMismatch "object")
    (\obj -> do
      home_points <- obj .: "home_points"
      away_points <- obj .: "away_points"
      periodsMaybe <- obj .:? "periods"
      periods <- case periodsMaybe of
        Just periodsJson -> case decodeJson periodsJson of
          Left _ -> pure []
          Right arr -> pure arr
        Nothing -> pure []
      pure $ Scoring { home_points, away_points, periods })
    json

newtype Period = Period
  { period_type :: String
  , number :: Int
  , home_points :: Int
  , away_points :: Int
  }
derive instance Generic Period _
instance Show Period where
  show = genericShow
instance DecodeJson Period where
  decodeJson json = caseJsonObject
    (Left $ TypeMismatch "object")
    (\obj -> do
      period_type <- obj .: "period_type"
      number <- obj .: "number"
      home_points <- obj .: "home_points"
      away_points <- obj .: "away_points"
      pure $ Period { period_type, number, home_points, away_points })
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
      pure $ Situation { situationClock, down, yfd, possession })
    json

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

type GameInfo =
  { homeTeam :: String
  , awayTeam :: String
  , homeScore :: Int
  , awayScore :: Int
  , clock :: String
  , quarterScores :: Array { quarter :: Int, home :: Int, away :: Int }
  }
