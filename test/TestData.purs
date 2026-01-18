module Test.TestData where

-- Test JSON data moved from Component.Person

testJSON1 :: String
testJSON1 =
  """
{ "situation": {
      "clock": "25:00",
      "down": 1,
      "yfd": 10,
      "possession": {
          "id": "3d08af9e-c767-4f88-a7dc-b920c6d2b4a8",
          "name": "Seahawks",
          "market": "Seattle",
          "alias": "SEA",
          "sr_id": "sr:competitor:4430"
      },
      "location": {
          "id": "3d08af9e-c767-4f88-a7dc-b920c6d2b4a8",
          "name": "Seahawks",
          "market": "Seattle",
          "alias": "SEA",
          "sr_id": "sr:competitor:4430",
          "yardline": 40
      }
  }
}
"""

testJSON2 :: String
testJSON2 =
  """
{ "situation": {
      "clock": "13:00",
      "down": 1,
      "yfd": 10,
      "possession": {
          "id": "3d08af9e-c767-4f88-a7dc-b920c6d2b4a8",
          "name": "Chiefs",
          "market": "Seattle",
          "alias": "SEA",
          "sr_id": "sr:competitor:4430"
      },
      "location": {
          "id": "3d08af9e-c767-4f88-a7dc-b920c6d2b4a8",
          "name": "Seahawks",
          "market": "Seattle",
          "alias": "SEA",
          "sr_id": "sr:competitor:4430",
          "yardline": 40
      }
  }
}
"""

testJSON3 :: String
testJSON3 =
  """
{ "situation": {
      "clock": "00:00",
      "down": 1,
      "yfd": 10,
      "possession": {
          "id": "3d08af9e-c767-4f88-a7dc-b920c6d2b4a8",
          "name": "Seahawks",
          "market": "Seattle",
          "alias": "SEA",
          "sr_id": "sr:competitor:4430"
      },
      "location": {
          "id": "3d08af9e-c767-4f88-a7dc-b920c6d2b4a8",
          "name": "Seahawks",
          "market": "Seattle",
          "alias": "SEA",
          "sr_id": "sr:competitor:4430",
          "yardline": 40
      }
  }
}
"""

-- Additional test JSON for 15:00 clock transition
testJSON4 :: String
testJSON4 =
  """
{ "situation": {
      "clock": "15:00",
      "down": 1,
      "yfd": 10,
      "possession": {
          "id": "3d08af9e-c767-4f88-a7dc-b920c6d2b4a8",
          "name": "Seahawks",
          "market": "Seattle",
          "alias": "SEA",
          "sr_id": "sr:competitor:4430"
      },
      "location": {
          "id": "3d08af9e-c767-4f88-a7dc-b920c6d2b4a8",
          "name": "Seahawks",
          "market": "Seattle",
          "alias": "SEA",
          "sr_id": "sr:competitor:4430",
          "yardline": 40
      }
  }
}
"""
