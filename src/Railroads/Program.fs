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

  let functionalTests () =
    highlight "TestMaybe.functionalTests"
    indent basicTests
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
[<EntryPoint>]
let main argv = 
  TestMaybe.functionalTests ()

  if errors = 0 then
    successf "Ran %d tests with no errors" tests
  else
    errorf "Ran %d tests with %d errors" tests errors

  0
// ----------------------------------------------------------------------------------------------
