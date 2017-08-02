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
    let _n    = Nothing
    let _0    = Just 0
    let _1    = Just 1
    let _3    = Just 3
    let _4    = Just 4
    let _dn   = fun _ -> Nothing
    let _dp2  = fun v -> Just (v + 2)

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
[<EntryPoint>]
let main argv = 
  try
    TestMaybe.functionalTests ()
  with
  | e -> errorf "Exception: %s" e.Message

  if errors = 0 then
    successf "Ran %d tests with no errors" tests
    0
  else
    errorf "Ran %d tests with %d errors" tests errors
    999
// ----------------------------------------------------------------------------------------------
