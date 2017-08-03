// ----------------------------------------------------------------------------------------------
// Copyright 2017 Mårten Rånge
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
open Railroads.Core
open Railroads.Test
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
module TestMaybe =
  open Railroads.Core.Maybe

  let basicTests () =
    info "basicTests"
    let _n                  = Nothing
    let _0    : Maybe<int>  = Just 0
    let _1    : int maybe   = Just 1
    let _3                  = Just 3
    let _4                  = Just 4
    let _dn                 = fun _ -> Nothing
    let _dp2                = fun v -> Just (v + 2)

    check_eq _n (nothing)     "nothing"
    check_eq _1 (just 1 )     "just"

    check_eq _n (bind _n _dn ) "bind - Nothing - Nothing"
    check_eq _n (bind _1 _dn ) "bind - Just 1  - Nothing"
    check_eq _n (bind _n _dp2) "bind - Nothing - +2"
    check_eq _3 (bind _1 _dp2) "bind - Just 1  - +2"
    check_eq _3 (_1 >>= _dp2)  ">>= - Just 1  - +2"

    do
      let a = arrow ((+) 2)
      check_eq _3 (a 1) "arrow"

      let b = kleisli a _dp2
      check_eq _3 (b -1) "kleisli"

    do
      let f = just ((+) 1)

      check_eq _n (apply _n _n) "apply - Nothing - Nothing"
      check_eq _n (apply _n _3) "apply - Nothing - Just 3"
      check_eq _n (apply f  _n) "apply - +1      - Nothing"
      check_eq _4 (apply f  _3) "apply - +1      - Just 3"

      check_eq _4 ((just (+)) <*> _1 <*> _3) "<*> - + - Just 1 - Just 2"

    do
      let f = (=) 1

      check_eq _n (filter f _n) "filter - =1 - Nothing"
      check_eq _1 (filter f _1) "filter - =1 - Just 1"
      check_eq _n (filter f _3) "filter - =1 - Just 2"

    do
      let m = (+) 1

      check_eq _n (map m _n) "map - +1 - Nothing"
      check_eq _4 (map m _3) "map - +1 - Just 3"
      check_eq _4 (_3 |>> m) "|>> - +1 - Just 3"

    check_eq _n (flatten _n)        "flatten - Nothing"
    check_eq _n (flatten (just _n)) "flatten - Just Nothing"
    check_eq _1 (flatten (just _1)) "flatten - Just Just 1"

    check_eq _n (andAlso _n _n)                   "andAlso - Nothing - Nothing"
    check_eq _n (andAlso _1 _n)                   "andAlso - Just 1  - Nothing"
    check_eq _n (andAlso _n _3)                   "andAlso - Nothing - Just 3"
    check_eq (just struct (1, 3)) (andAlso _1 _3) "andAlso - Just 1  - Just 3"
    check_eq (just struct (3, 1)) (_3 <&> _1)     "<&> - Just 3  - Just 1"
    check_eq (just struct (3, 4)) (_3 .>>. _4)    ".>>. - Just 3  - Just 4"

    check_eq _n (orElse _n _n) "orElse - Nothing - Nothing"
    check_eq _1 (orElse _1 _n) "orElse - Just 1  - Nothing"
    check_eq _3 (orElse _n _3) "orElse - Nothing - Just 3"
    check_eq _1 (orElse _1 _3) "orElse - Just 1  - Just 3"
    check_eq _1 (_1 <|> _3)    "<|> - Just 1  - Just 3"

    check_eq _n (trace "T" _n) "trace - Nothing"
    check_eq _1 (trace "T" _1) "trace - Nothing"

    check_eq _n (fromObj null) "fromObj - null"
    check_eq _1 (fromObj 1)    "fromObj - 1"

    check_eq _n (fromOption None)     "fromOption - None"
    check_eq _1 (fromOption (Some 1)) "fromOption - Some 1"

    check_eq _n (fromResult (Error ())) "fromResult - Error ()"
    check_eq _1 (fromResult (Ok 1))     "fromResult - Ok 1"

    check_eq 2 (valueOr 2 _n) "valueOr - 2 - Nothing"
    check_eq 1 (valueOr 2 _1) "valueOr - 2 - Just 1"

    do
      let tc1 =
        maybe {
          let! x = _1 // Tests Bind
          let! y = _3
          return x + y    // Tests Return
        }
      let tc2 =
        maybe {
          let! x = _1
          let! y = _3
          return! just (x + y)  // Tests ReturnFrom
        }
      let tc3 =
        maybe {
          if false then
            return 1
          // Tests Zero
        }
      let tc4 =
        maybe {
          let! x = _n
          let! y = _3
          return x + y
        }

      check_eq _4 tc1 "maybe - first case"
      check_eq _4 tc2 "maybe - second case"
      check_eq _0 tc3 "maybe - third case"
      check_eq _n tc4 "maybe - fourth case"

  let functionalTests () =
    highlight "TestMaybe.functionalTests"
    indent basicTests
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
module TestOutcome =
  open Railroads.Core.Outcome

  exception TestException

  let basicTests () =
    info "basicTests"
    let _e    = Outcome (Nothing, BadTree.Empty)
    let _1    = Outcome (Just 1 , BadTree.Empty)
    let _2    = Outcome (Just 2 , BadTree.Empty)
    let _3    = Outcome (Just 3 , BadTree.Empty)
    let _4    = Outcome (Just 4 , BadTree.Empty)
    let _bm   = Outcome (Nothing, BadTree.Leaf (MessageBadOutcome "Bad"))
    let _be   = Outcome (Nothing, BadTree.Leaf (ExceptionBadOutcome TestException))
    let _bv   = Outcome (Nothing, BadTree.Leaf (BoxedBadOutcome<_> 123))
    let _bv1  = Outcome (Just 1 , BadTree.Leaf (BoxedBadOutcome<_> 123))
    let _bm1  = Outcome (Just 1 , BadTree.Leaf (MessageBadOutcome "Bad"))
    let _bm2  = Outcome (Just 2 , BadTree.Leaf (MessageBadOutcome "Bad"))
    let _bm3  = Outcome (Just 3 , BadTree.Leaf (MessageBadOutcome "Bad"))
    let _bf   = Outcome (Nothing, BadTree.Fork (BadTree.Leaf (MessageBadOutcome "Bad"), BadTree.Leaf (BoxedBadOutcome<_> 123)))
    let _bf1  = Outcome (Just 1 , BadTree.Fork (BadTree.Leaf (MessageBadOutcome "Bad"), BadTree.Leaf (BoxedBadOutcome<_> 123)))
    let _bf2  = Outcome (Just 2 , BadTree.Fork (BadTree.Leaf (MessageBadOutcome "Bad"), BadTree.Leaf (BoxedBadOutcome<_> 123)))
    let _bf3  = Outcome (Just 3 , BadTree.Fork (BadTree.Leaf (MessageBadOutcome "Bad"), BadTree.Leaf (BoxedBadOutcome<_> 123)))
    let _de   = fun _ -> _e
    let _dp2  = fun v -> good (v + 2)
    let _dbm  = fun v -> _bm
    let _dbm1 = fun v -> _bm1
    let _dbv  = fun v -> _bv

    check_eq _e   (empty                      ) "empty"
    check_eq _1   (good 1                     ) "good"
    check_eq _bm  (badMessage "Bad"           ) "badMessage"
    check_eq _bm  (badMessagef "Ba%s" "d"     ) "badMessagef"
    check_eq _be  (badException TestException ) "badException"
    check_eq _bv  (badValue 123)                "badValue"

    check_eq _e   (bind _e _de )    "bind - empty  - empty"
    check_eq _e   (bind _e _dp2)    "bind - empty  - +2"
    check_eq _e   (bind _e _dbm)    "bind - good 1 - bad"
    check_eq _e   (bind _1 _de)     "bind - good 1 - empty"
    check_eq _3   (bind _1 _dp2)    "bind - good 1 - +2"
    check_eq _bm  (bind _1 _dbm)    "bind - good 1 - bad"
    check_eq _bm  (bind _bm1 _de)   "bind - bad 1  - empty"
    check_eq _bm  (bind _bm1 _dp2)  "bind - bad 1  - +2"
    check_eq _bm  (bind _bm1 _dbm)  "bind - bad 1  - bad"
    check_eq _3   (_1 >>= _dp2  )   ">>= - good 1 - +2"

    check_eq _e   (forceBind _e _de )   "forceBind - empty  - empty"
    check_eq _e   (forceBind _e _dp2)   "forceBind - empty  - +2"
    check_eq _e   (forceBind _e _dbm)   "forceBind - good 1 - bad"
    check_eq _e   (forceBind _1 _de)    "forceBind - good 1 - empty"
    check_eq _3   (forceBind _1 _dp2)   "forceBind - good 1 - +2"
    check_eq _bm  (forceBind _1 _dbm)   "forceBind - good 1 - bad"
    check_eq _bm  (forceBind _bm1 _de)  "forceBind - bad 1  - empty"
    check_eq _bm3 (forceBind _bm1 _dp2) "forceBind - bad 1  - +2"
    check_eq _bf  (forceBind _bm1 _dbv) "forceBind - bad 1  - bad value"
    check_eq _3   (_1 >>=! _dp2  )      ">>=! - good 1 - +2"

    do
      let a = arrow ((+) 2)
      check_eq _3 (a 1) "arrow"

      let b = kleisli a _dp2
      check_eq _3 (b -1) "kleisli"

      let c = forceKleisli a _dp2
      check_eq _3 (c -1) "forceKleisli"

    check_eq _1   (keepLeft _1 _1)      "keepLeft - good 1 - good 1"
    check_eq _bm1 (keepLeft _1 _bm2)    "keepLeft - good 1 - bad 2"
    check_eq _bm2 (keepLeft _bm2 _1)    "keepLeft - bad 2 - good 1"
    check_eq _bf2 (keepLeft _bm2 _bv1)  "keepLeft - bad 2 - bad value"
    check_eq _bm1 (_1 .>> _bm2) ".>> - good 1 - bad 2"
    check_eq _bm1 (_1 <?> _bm2) "<?> - good 1 - bad 2"

    check_eq _1   (keepRight _1 _1)     "keepRight - good 1 - good 1"
    check_eq _bm2 (keepRight _1 _bm2)   "keepRight - good 1 - bad 2"
    check_eq _bm1 (keepRight _bm2 _1)   "keepRight - bad 2 - good 1"
    check_eq _bf1 (keepRight _bm2 _bv1) "keepRight - bad 2 - bad 3"
    check_eq _bm1 (_bm2 >>. _1)   ">>. - bad 2 - good 1"

    do
      let f   = good ((+) 1)
      let bf  = f <?> _bm

      check_eq _e  (apply _e _e)    "apply - empty - empty"
      check_eq _e  (apply _e _3)    "apply - empty - good 3"
      check_eq _bm (apply _e _bm1)  "apply - empty - bad 1"
      check_eq _e  (apply f  _e)    "apply - +1    - empty"
      check_eq _4  (apply f  _3)    "apply - +1    - good 3"
      check_eq _bm (apply f  _bm1)  "apply - +1    - bad 1"
      check_eq _bm (apply bf  _e)   "apply - bad f  - empty"
      check_eq _bm (apply bf  _2)   "apply - bad f  - good 3"
      check_eq _bf (apply bf  _bv1) "apply - bad f  - bad value"

      check_eq _4 ((good (+)) <*> _1 <*> _3) "<*> - + - good 1 - good 2"

      check_eq _e   (forceApply _e _e)    "forceApply - empty   - empty"
      check_eq _e   (forceApply _e _3)    "forceApply - empty   - good 3"
      check_eq _bm  (forceApply _e _bm1)  "forceApply - empty   - bad 1"
      check_eq _e   (forceApply f  _e)    "forceApply - good +1 - empty"
      check_eq _4   (forceApply f  _3)    "forceApply - good +1 - good 2"
      check_eq _bm2 (forceApply f  _bm1)  "forceApply - good +1 - bad 1"
      check_eq _bm  (forceApply bf  _e)   "forceApply - bad f   - empty"
      check_eq _bm3 (forceApply bf  _2)   "forceApply - bad f   - good 3"
      check_eq _bf2 (forceApply bf  _bv1) "forceApply - bad f   - bad value"

      check_eq _4 ((good (+)) <*>! _1 <*>! _3) "<*> - + - good 1 - good 2"

    do
      let m = (+) 1

      check_eq _e   (map m _e)    "map - +1 - empty"
      check_eq _4   (map m _3)    "map - +1 - good 3"
      check_eq _bm  (map m _bm1)  "map - +1 - bad 1"
      check_eq _4 (_3 |>> m) "|>> - +1 - Just 3"

      check_eq _e   (forceMap m _e)    "forceMap - +1 - empty"
      check_eq _4   (forceMap m _3)    "forceMap - +1 - good 3"
      check_eq _bm2 (forceMap m _bm1)  "forceMap - +1 - bad 1"
      check_eq _4 (_3 |>>! m) "|>>! - +1 - good 3"

    do
      let bm    = badMessage "Bad"
      let bm1   = (good _1) <?> bm
      let bm11  = (good _1 <?> _bm) <?> _bv
      check_eq _e   (flatten _e)          "flatten - empty"
      check_eq _e   (flatten (good _e))   "flatten - good empty"
      check_eq _1   (flatten (good _1))   "flatten - good good 1"
      check_eq _bm1 (flatten (good _bm1)) "flatten - good bad 1"
      check_eq _bm  (flatten bm)          "flatten - bad"
      check_eq _bm1 (flatten bm1)         "flatten - bad good 1"
      check_eq _bf1 (flatten bm11)        "flatten - bad bad 1"

  let functionalTests () =
    highlight "TestOutcome.functionalTests"
    indent basicTests
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
[<EntryPoint>]
let main argv =
  try
    TestMaybe.functionalTests ()
    TestOutcome.functionalTests ()
  with
  | e -> errorf "Exception: %s" e.Message

  if errors = 0 then
    successf "Ran %d tests with no errors" tests
    0
  else
    errorf "Ran %d tests with %d errors" tests errors
    999
// ----------------------------------------------------------------------------------------------
