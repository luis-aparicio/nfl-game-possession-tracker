module Test.Main where

import Prelude

import Component.Person as Person
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
    it "should decode testJSON1 successfully" do
      result <- Person.getSituation' TestData.testJSON1
      result `shouldSatisfy` case _ of
        Just _ -> true
        Nothing -> false
      case result of
        Just (Person.Response { situation }) -> do
          let clock = Person.getClock situation
          clock `shouldEqual` "25:00"
          let (Person.Possession { name }) = Person.getPossession situation
          name `shouldEqual` "Seahawks"
        Nothing -> log "Failed to decode testJSON1" *> pure unit

    it "should decode testJSON2 successfully" do
      result <- Person.getSituation' TestData.testJSON2
      result `shouldSatisfy` case _ of
        Just _ -> true
        Nothing -> false
      case result of
        Just (Person.Response { situation }) -> do
          let clock = Person.getClock situation
          clock `shouldEqual` "13:00"
          let (Person.Possession { name }) = Person.getPossession situation
          name `shouldEqual` "Chiefs"
        Nothing -> log "Failed to decode testJSON2" *> pure unit

    it "should decode testJSON3 successfully" do
      result <- Person.getSituation' TestData.testJSON3
      result `shouldSatisfy` case _ of
        Just _ -> true
        Nothing -> false
      case result of
        Just (Person.Response { situation }) -> do
          let clock = Person.getClock situation
          clock `shouldEqual` "00:00"
          let (Person.Possession { name }) = Person.getPossession situation
          name `shouldEqual` "Seahawks"
        Nothing -> log "Failed to decode testJSON3" *> pure unit

    it "should decode testJSON4 (15:00) successfully" do
      result <- Person.getSituation' TestData.testJSON4
      result `shouldSatisfy` case _ of
        Just _ -> true
        Nothing -> false
      case result of
        Just (Person.Response { situation }) -> do
          let clock = Person.getClock situation
          clock `shouldEqual` "15:00"
          let (Person.Possession { name }) = Person.getPossession situation
          name `shouldEqual` "Seahawks"
        Nothing -> log "Failed to decode testJSON4" *> pure unit

  describe "Situation Transition Tests" do
    it "should detect possession change from Seahawks to Chiefs" do
      result1 <- Person.getSituation' TestData.testJSON1
      result2 <- Person.getSituation' TestData.testJSON2
      case result1, result2 of
        Just (Person.Response { situation: sit1 }), Just (Person.Response { situation: sit2 }) -> do
          let (Person.Possession { name: name1 }) = Person.getPossession sit1
          let (Person.Possession { name: name2 }) = Person.getPossession sit2
          name1 `shouldEqual` "Seahawks"
          name2 `shouldEqual` "Chiefs"
          (name1 /= name2) `shouldEqual` true
        Nothing, _ -> log "Failed to decode testJSON1 for possession change test" *> pure unit
        _, Nothing -> log "Failed to decode testJSON2 for possession change test" *> pure unit

    it "should detect no possession change when both are Seahawks" do
      result1 <- Person.getSituation' TestData.testJSON1
      result3 <- Person.getSituation' TestData.testJSON3
      case result1, result3 of
        Just (Person.Response { situation: sit1 }), Just (Person.Response { situation: sit3 }) -> do
          let (Person.Possession { name: name1 }) = Person.getPossession sit1
          let (Person.Possession { name: name3 }) = Person.getPossession sit3
          name1 `shouldEqual` "Seahawks"
          name3 `shouldEqual` "Seahawks"
          (name1 /= name3) `shouldEqual` false
        Nothing, _ -> log "Failed to decode testJSON1 for no possession change test" *> pure unit
        _, Nothing -> log "Failed to decode testJSON3 for no possession change test" *> pure unit

    it "should detect clock at 00:00" do
      result <- Person.getSituation' TestData.testJSON3
      case result of
        Just (Person.Response { situation }) -> do
          let clock = Person.getClock situation
          (clock == "00:00") `shouldEqual` true
        Nothing -> log "Failed to decode testJSON3 for clock test" *> pure unit

    it "should detect clock at 15:00" do
      result <- Person.getSituation' TestData.testJSON4
      case result of
        Just (Person.Response { situation }) -> do
          let clock = Person.getClock situation
          (clock == "15:00") `shouldEqual` true
        Nothing -> log "Failed to decode testJSON4 for clock test" *> pure unit

    it "should detect clock NOT at 00:00 or 15:00" do
      result1 <- Person.getSituation' TestData.testJSON1
      result2 <- Person.getSituation' TestData.testJSON2
      case result1, result2 of
        Just (Person.Response { situation: sit1 }), Just (Person.Response { situation: sit2 }) -> do
          let clock1 = Person.getClock sit1
          let clock2 = Person.getClock sit2
          (clock1 == "00:00" || clock1 == "15:00") `shouldEqual` false
          (clock2 == "00:00" || clock2 == "15:00") `shouldEqual` false
        Nothing, _ -> log "Failed to decode testJSON1 for clock test" *> pure unit
        _, Nothing -> log "Failed to decode testJSON2 for clock test" *> pure unit
