module Test.Main where

import Prelude

import NFL.API as API
import NFL.Data as NFL
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.TestData as TestData

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "JSON Decoding Tests" do
    it "should decode testJSON1 (Quarter 1) successfully" do
      result <- API.getResponse' TestData.testJSON1
      result `shouldSatisfy` case _ of
        Just _ -> true
        Nothing -> false
      case result of
        Just (NFL.Response { situation, quarter }) -> do
          quarter `shouldEqual` 1
          let (NFL.Possession { name }) = API.getPossession situation
          name `shouldEqual` "Seahawks"
        Nothing -> log "Failed to decode testJSON1" *> pure unit

    it "should decode testJSON2 (Quarter 2) successfully" do
      result <- API.getResponse' TestData.testJSON2
      result `shouldSatisfy` case _ of
        Just _ -> true
        Nothing -> false
      case result of
        Just (NFL.Response { situation, quarter }) -> do
          quarter `shouldEqual` 2
          let (NFL.Possession { name }) = API.getPossession situation
          name `shouldEqual` "Chiefs"
        Nothing -> log "Failed to decode testJSON2" *> pure unit

    it "should decode testJSON3 (Quarter 3) successfully" do
      result <- API.getResponse' TestData.testJSON3
      result `shouldSatisfy` case _ of
        Just _ -> true
        Nothing -> false
      case result of
        Just (NFL.Response { situation, quarter }) -> do
          quarter `shouldEqual` 3
          let (NFL.Possession { name }) = API.getPossession situation
          name `shouldEqual` "Seahawks"
        Nothing -> log "Failed to decode testJSON3" *> pure unit

    it "should decode testJSON4 (Quarter 4) successfully" do
      result <- API.getResponse' TestData.testJSON4
      result `shouldSatisfy` case _ of
        Just _ -> true
        Nothing -> false
      case result of
        Just (NFL.Response { situation, quarter }) -> do
          quarter `shouldEqual` 4
          let (NFL.Possession { name }) = API.getPossession situation
          name `shouldEqual` "Seahawks"
        Nothing -> log "Failed to decode testJSON4" *> pure unit

  describe "Quarter Transition Tests" do
    it "should detect quarter change from Quarter 1 to Quarter 2" do
      result1 <- API.getResponse' TestData.testJSON1
      result2 <- API.getResponse' TestData.testJSON2
      case result1, result2 of
        Just (NFL.Response { quarter: quarter1 }), Just (NFL.Response { quarter: quarter2 }) -> do
          quarter1 `shouldEqual` 1
          quarter2 `shouldEqual` 2
          (quarter1 /= quarter2) `shouldEqual` true
        Nothing, _ -> log "Failed to decode testJSON1 for quarter change test" *> pure unit
        _, Nothing -> log "Failed to decode testJSON2 for quarter change test" *> pure unit

    it "should detect quarter change from Quarter 2 to Quarter 3" do
      result2 <- API.getResponse' TestData.testJSON2
      result3 <- API.getResponse' TestData.testJSON3
      case result2, result3 of
        Just (NFL.Response { quarter: quarter2 }), Just (NFL.Response { quarter: quarter3 }) -> do
          quarter2 `shouldEqual` 2
          quarter3 `shouldEqual` 3
          (quarter2 /= quarter3) `shouldEqual` true
        Nothing, _ -> log "Failed to decode testJSON2 for quarter change test" *> pure unit
        _, Nothing -> log "Failed to decode testJSON3 for quarter change test" *> pure unit

    it "should detect quarter change from Quarter 3 to Quarter 4" do
      result3 <- API.getResponse' TestData.testJSON3
      result4 <- API.getResponse' TestData.testJSON4
      case result3, result4 of
        Just (NFL.Response { quarter: quarter3 }), Just (NFL.Response { quarter: quarter4 }) -> do
          quarter3 `shouldEqual` 3
          quarter4 `shouldEqual` 4
          (quarter3 /= quarter4) `shouldEqual` true
        Nothing, _ -> log "Failed to decode testJSON3 for quarter change test" *> pure unit
        _, Nothing -> log "Failed to decode testJSON4 for quarter change test" *> pure unit

    it "should detect no quarter change when both are Quarter 1" do
      result1 <- API.getResponse' TestData.testJSON1
      result1Again <- API.getResponse' TestData.testJSON1
      case result1, result1Again of
        Just (NFL.Response { quarter: quarter1 }), Just (NFL.Response { quarter: quarter1Again }) -> do
          quarter1 `shouldEqual` 1
          quarter1Again `shouldEqual` 1
          (quarter1 /= quarter1Again) `shouldEqual` false
        Nothing, _ -> log "Failed to decode testJSON1 for no quarter change test" *> pure unit
        _, Nothing -> log "Failed to decode testJSON1 again for no quarter change test" *> pure unit
