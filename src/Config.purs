module Config where

import Effect (Effect)

type Config =
  { apiKey :: String
  , testGameId :: String
  , superBowlGameId :: String
  , testMode :: Boolean
  }

-- Read config from window.__APP_CONFIG__ or use defaults
-- For browser: set window.__APP_CONFIG__ = { API_KEY: "...", TEST_MODE: "false", ... } before loading
-- For Node.js tests: environment variables are used via FFI
foreign import getConfigImpl :: Effect Config

getConfig :: Effect Config
getConfig = getConfigImpl

getGameId :: Config -> String
getGameId config = if config.testMode then config.testGameId else config.superBowlGameId
