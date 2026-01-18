// Browser implementation: reads from window.__APP_CONFIG__ or uses defaults
// Node.js implementation: reads from process.env
// PureScript Effect type means this should be a function that returns the value directly
export const getConfigImpl = function() {
  var config = {};
  
  // Try to read from window.__APP_CONFIG__ (browser)
  if (typeof window !== "undefined") {
    if (window.__APP_CONFIG__) {
      config = window.__APP_CONFIG__;
      console.log("Config: Loaded from window.__APP_CONFIG__", {
        hasApiKey: !!config.API_KEY,
        apiKeyLength: config.API_KEY ? config.API_KEY.length : 0,
        apiKeyPreview: config.API_KEY ? config.API_KEY.substring(0, 10) + "..." : "undefined"
      });
    } else {
      console.warn("Config: window.__APP_CONFIG__ is not defined. Available keys:", Object.keys(window).filter(k => k.includes("CONFIG")));
    }
  }
  // Try to read from process.env (Node.js)
  else if (typeof process !== "undefined" && process.env) {
    config = process.env;
    console.log("Config: Loaded from process.env");
  }
  
  var result = {
    apiKey: config.API_KEY || "",
    testGameId: config.TEST_GAME_ID || "7d06369a-382a-448a-b295-6da9eab53245",
    superBowlGameId: config.SUPER_BOWL_GAME_ID || "ca9d8f84-8e7b-4ee7-a310-54c2e3ca4edc",
    testMode: (config.TEST_MODE === "true" || config.TEST_MODE === "1")
  };
  
  if (!result.apiKey) {
    console.error("Config: API_KEY is empty! Check window.__APP_CONFIG__ or .env file");
    console.error("Config object received:", config);
  } else {
    console.log("Config loaded successfully:", {
      hasApiKey: true,
      testMode: result.testMode,
      gameId: result.testMode ? result.testGameId : result.superBowlGameId
    });
  }
  
  return result;
};
