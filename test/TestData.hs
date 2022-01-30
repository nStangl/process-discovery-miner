module TestData where

import Types

-- Test data for HUnit tests
-- Data extracted and seperated from either [1] or [2]
-- For every log xL and yL are defined, other partial results optional.
-- [1]: https://lehre.bpm.in.tum.de/~pm-prak/
-- [2]: Process Mining, Wil van der Aalst, Data Science in Action (2nd. Edit)

{------ Log 1 -----}
getLog1 :: EventLog
getLog1 = [["a", "b", "c", "d"], ["a", "c", "b", "d"], ["a", "e", "d"]]

getLog1OrdFollowDir :: [(Activity, Activity)]
getLog1OrdFollowDir = [("a", "b"), ("a", "c"), ("a", "e"), ("b", "c"), ("c", "b"), ("b", "d"), ("c", "d"), ("e", "d")]

getLog1OrdCausal :: [(Activity, Activity)]
getLog1OrdCausal = [("a", "b"), ("a", "c"), ("a", "e"), ("b", "d"), ("c", "d"), ("e", "d")]

getLog1OrdChoice :: [(Activity, Activity)]
getLog1OrdChoice = [("a", "a"), ("a", "d"), ("b", "b"), ("b", "e"), ("c", "c"), ("c", "e"), ("d", "a"), ("d", "d"), ("e", "b"), ("e", "c"), ("e", "e")]

getLog1OrdParallel :: [(Activity, Activity)]
getLog1OrdParallel = [("b", "c"), ("c", "b")]

getLog1xL :: [Transition]
getLog1xL = [(["a"], ["b", "e"]), (["a"], ["b"]), (["a"], ["c", "e"]), (["a"], ["c"]), (["a"], ["e"]), (["b", "e"], ["d"]), (["b"], ["d"]), (["c", "e"], ["d"]), (["c"], ["d"]), (["e"], ["d"])]

getLog1yL :: [Transition]
getLog1yL = [(["c", "e"], ["d"]), (["b", "e"], ["d"]), (["a"], ["c", "e"]), (["a"], ["b", "e"])]

{----- Log 2 -----}
getLog2 :: EventLog
getLog2 = [["a", "b", "c", "d"], ["a", "c", "b", "d"], ["a", "b", "c", "e", "f", "b", "c", "d"], ["a", "b", "c", "e", "f", "c", "b", "d"], ["a", "c", "b", "e", "f", "b", "c", "d"], ["a", "c", "b", "e", "f", "b", "c", "e", "f", "c", "b", "d"]]

getLog2OrdFollowDir :: [(Activity, Activity)]
getLog2OrdFollowDir = [("a", "b"), ("a", "c"), ("b", "c"), ("b", "d"), ("b", "e"), ("c", "b"), ("c", "d"), ("c", "e"), ("e", "f"), ("f", "b"), ("f", "c")]

getLog2OrdCausal :: [(Activity, Activity)]
getLog2OrdCausal = [("a", "b"), ("a", "c"), ("b", "d"), ("b", "e"), ("c", "d"), ("c", "e"), ("e", "f"), ("f", "b"), ("f", "c")]

getLog2OrdChoice :: [(Activity, Activity)]
getLog2OrdChoice = [("a", "a"), ("a", "d"), ("a", "e"), ("a", "f"), ("b", "b"), ("c", "c"), ("d", "a"), ("d", "d"), ("d", "e"), ("d", "f"), ("e", "a"), ("e", "d"), ("e", "e"), ("f", "a"), ("f", "d"), ("f", "f")]

getLog2OrdParallel :: [(Activity, Activity)]
getLog2OrdParallel = [("b", "c"), ("c", "b")]

getLog2xL :: [Transition]
getLog2xL = [(["a", "f"], ["b"]), (["a", "f"], ["c"]), (["a"], ["b"]), (["a"], ["c"]), (["b"], ["d", "e"]), (["b"], ["d"]), (["b"], ["e"]), (["c"], ["d", "e"]), (["c"], ["d"]), (["c"], ["e"]), (["e"], ["f"]), (["f"], ["b"]), (["f"], ["c"])]

getLog2yL :: [Transition]
getLog2yL = [(["a", "f"], ["b"]), (["a", "f"], ["c"]), (["b"], ["d", "e"]), (["c"], ["e", "d"]), (["e"], ["f"])]

{----- Log 3 ------}
getLog3 :: EventLog
getLog3 = [["a", "b", "c", "d", "e", "f", "b", "d", "c", "e", "g"], ["a", "b", "d", "c", "e", "g"], ["a", "b", "c", "d", "e", "f", "b", "c", "d", "e", "f", "b", "d", "c", "e", "g"]]

getLog3xL :: [Transition]
getLog3xL = [(["a", "f"], ["b"]), (["a"], ["b"]), (["b"], ["c"]), (["b"], ["d"]), (["c"], ["e"]), (["d"], ["e"]), (["e"], ["f", "g"]), (["e"], ["f"]), (["e"], ["g"]), (["f"], ["b"])]

getLog3yL :: [Transition]
getLog3yL = [(["a", "f"], ["b"]), (["b"], ["c"]), (["b"], ["d"]), (["c"], ["e"]), (["d"], ["e"]), (["e"], ["f", "g"])]

{----- Log 4 ------}
getLog4 :: EventLog
getLog4 = [["a", "c", "d"], ["b", "c", "d"], ["a", "c", "e"], ["b", "c", "e"]]

getLog4xL :: [Transition]
getLog4xL = [(["a", "b"], ["c"]), (["a"], ["c"]), (["b"], ["c"]), (["c"], ["d", "e"]), (["c"], ["d"]), (["c"], ["e"])]

getLog4yL :: [Transition]
getLog4yL = [(["a", "b"], ["c"]), (["c"], ["d", "e"])]

{----- Log 5 ------}
getLog5 :: EventLog
getLog5 = [["a", "b", "e", "f"], ["a", "b", "e", "c", "d", "b", "f"], ["a", "b", "c", "e", "d", "b", "f"], ["a", "b", "c", "d", "e", "b", "f"], ["a", "e", "b", "c", "d", "b", "f"]]

getLog5xL :: [Transition]
getLog5xL = [(["a", "d"], ["b"]), (["a"], ["b"]), (["a"], ["e"]), (["b"], ["c", "f"]), (["b"], ["c"]), (["b"], ["f"]), (["c"], ["d"]), (["d"], ["b"]), (["e"], ["f"])]

getLog5yL :: [Transition]
getLog5yL = [(["e"], ["f"]), (["c"], ["d"]), (["a"], ["e"]), (["b"], ["c", "f"]), (["a", "d"], ["b"])]

{----- Log 6 ------}
getLog6 :: EventLog
getLog6 = [["a", "c", "e", "g"], ["a", "e", "c", "g"], ["b", "d", "f", "g"], ["b", "f", "d", "g"]]

getLog6xL :: [Transition]
getLog6xL = [(["a"], ["c"]), (["a"], ["e"]), (["b"], ["d"]), (["b"], ["f"]), (["c", "d"], ["g"]), (["c", "f"], ["g"]), (["c"], ["g"]), (["d", "e"], ["g"]), (["d"], ["g"]), (["e", "f"], ["g"]), (["e"], ["g"]), (["f"], ["g"])]

getLog6yL :: [Transition]
getLog6yL = [(["a"], ["c"]), (["a"], ["e"]), (["b"], ["d"]), (["b"], ["f"]), (["c", "d"], ["g"]), (["c", "f"], ["g"]), (["d", "e"], ["g"]), (["e", "f"], ["g"])]

{----- Log 7 ------}
getLog7 :: EventLog
getLog7 =
  [ ["a", "b", "c"],
    ["a", "b", "b", "c"],
    ["a", "c"],
    ["a", "b", "b", "b", "b", "c"]
  ]

getLog7Preprocessed :: EventLog
getLog7Preprocessed =
  [ ["a", "c"],
    ["a", "c"],
    ["a", "c"],
    ["a", "c"]
  ]

getLog7L1LWithNeighbours :: [(Activity, Activity, Activity)]
getLog7L1LWithNeighbours = [("a", "b", "c")]

getLog7xL :: [Transition]
getLog7xL = [(["a"], ["b"]), (["a"], ["c"]), (["b"], ["c"])]

getLog7yL :: [Transition]
getLog7yL = [(["a"], ["b"]), (["a"], ["c"]), (["b"], ["c"])]

getLog7PostProcessCytoElem :: (CytoNode, [CytoEdge])
getLog7PostProcessCytoElem = (cytonode, cytoedges)
  where 
    cytonode = CytoNode "b" "rectangle"
    cytoedges = [CytoEdge "" "p1" "b" "triangle", CytoEdge "" "b" "p1" "triangle"]

{----- billinstances ------}
getBill :: EventLog
getBill = [["write bill", "print bill", "deliver bill"]]

getBillxL :: [Transition]
getBillxL = [(["print bill"], ["deliver bill"]), (["write bill"], ["print bill"])]

getBillyL :: [Transition]
getBillyL = [(["print bill"], ["deliver bill"]), (["write bill"], ["print bill"])]

{----- posterinstances ------}
getPoster :: EventLog
getPoster = [["receive order and photo", "design photo poster", "print poster", "deliver poster"]]

getPosterxL :: [Transition]
getPosterxL = [(["design photo poster"], ["print poster"]), (["print poster"], ["deliver poster"]), (["receive order and photo"], ["design photo poster"])]

getPosteryL :: [Transition]
getPosteryL = [(["design photo poster"], ["print poster"]), (["print poster"], ["deliver poster"]), (["receive order and photo"], ["design photo poster"])]

{----- flyerinstances ------}
getFlyer :: EventLog
getFlyer =
  [ [ "receive flyer order",
      "design flyer",
      "send draft to customer",
      "design flyer",
      "send draft to customer",
      "design flyer",
      "send draft to customer",
      "print flyer",
      "deliver flyer"
    ],
    ["receive flyer order", "design flyer", "send draft to customer", "design flyer", "send draft to customer", "print flyer", "deliver flyer"],
    ["receive flyer order", "design flyer", "send draft to customer", "print flyer", "deliver flyer"]
  ]

getFlyerxL :: [Transition]
getFlyerxL = [(["print flyer"], ["deliver flyer"]), (["receive flyer order"], ["design flyer"]), (["send draft to customer"], ["print flyer"])]

getFlyeryL :: [Transition]
getFlyeryL = [(["print flyer"], ["deliver flyer"]), (["receive flyer order"], ["design flyer"]), (["send draft to customer"], ["print flyer"])]

{----- posterinstances ------}
getRunEx :: EventLog
getRunEx =
  [ ["register request", "examine casually", "check ticket", "decide", "reinitiate request", "examine thoroughly", "check ticket", "decide", "pay compensation"],
    ["register request", "check ticket", "examine casually", "decide", "pay compensation"],
    ["register request", "examine thoroughly", "check ticket", "decide", "reject request"],
    ["register request", "examine casually", "check ticket", "decide", "pay compensation"],
    ["register request", "examine casually", "check ticket", "decide", "reinitiate request", "check ticket", "examine casually", "decide", "reinitiate request", "examine casually", "check ticket", "decide", "reject request"],
    ["register request", "check ticket", "examine thoroughly", "decide", "reject request"]
  ]

getRunExxL :: [Transition]
getRunExxL =
  [ (["check ticket"], ["decide"]),
    (["decide"], ["pay compensation", "reinitiate request", "reject request"]),
    (["decide"], ["pay compensation", "reinitiate request"]),
    (["decide"], ["pay compensation", "reject request"]),
    (["decide"], ["pay compensation"]),
    (["decide"], ["reinitiate request", "reject request"]),
    (["decide"], ["reinitiate request"]),
    (["decide"], ["reject request"]),
    (["examine casually", "examine thoroughly"], ["decide"]),
    (["examine casually"], ["decide"]),
    (["examine thoroughly"], ["decide"]),
    (["register request", "reinitiate request"], ["check ticket"]),
    (["register request", "reinitiate request"], ["examine casually", "examine thoroughly"]),
    (["register request", "reinitiate request"], ["examine casually"]),
    (["register request", "reinitiate request"], ["examine thoroughly"]),
    (["register request"], ["check ticket"]),
    (["register request"], ["examine casually", "examine thoroughly"]),
    (["register request"], ["examine casually"]),
    (["register request"], ["examine thoroughly"]),
    (["reinitiate request"], ["check ticket"]),
    (["reinitiate request"], ["examine casually", "examine thoroughly"]),
    (["reinitiate request"], ["examine casually"]),
    (["reinitiate request"], ["examine thoroughly"])
  ]

getRunExyL :: [Transition]
getRunExyL =
  [ (["check ticket"], ["decide"]),
    (["decide"], ["pay compensation", "reinitiate request", "reject request"]),
    (["examine casually", "examine thoroughly"], ["decide"]),
    (["register request", "reinitiate request"], ["check ticket"]),
    (["register request", "reinitiate request"], ["examine casually", "examine thoroughly"])
  ]

{----- Log 8 ------}
getLog8 :: EventLog
getLog8 = [["a", "b", "d"], ["a", "b", "c", "b", "d"], ["a", "b", "c", "b", "c", "b", "d"]]

getLog8Preprocessed :: EventLog
getLog8Preprocessed = getLog8

getLog8L1LWithNeighbours :: [(Activity, Activity, Activity)]
getLog8L1LWithNeighbours = []

getLog8AlphaPlusResult :: [Transition]
getLog8AlphaPlusResult = [(["start"],["a"]),(["a","c"],["b"]),(["b"],["d","c"]),(["d"],["end"])]

{----- Log 9 ------}
getLog9 :: EventLog
getLog9 = [["a", "c", "d"], ["b", "c", "e"]]
