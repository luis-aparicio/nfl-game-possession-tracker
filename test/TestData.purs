module Test.TestData where

-- Test JSON data moved from Component.Person

testJSON1 :: String
testJSON1 =
  """
{ "clock": "25:00",
  "quarter": 1,
  "status": "inprogress",
  "situation": {
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
{ "clock": "15:00",
  "quarter": 2,
  "status": "inprogress",
  "situation": {
      "clock": "15:00",
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
{ "clock": "15:00",
  "quarter": 3,
  "status": "inprogress",
  "situation": {
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

-- Additional test JSON for quarter 4
testJSON4 :: String
testJSON4 =
  """
{ "clock": "15:00",
  "quarter": 4,
  "status": "inprogress",
  "situation": {
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
