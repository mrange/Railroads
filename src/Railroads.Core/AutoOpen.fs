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
namespace Railroads.Core
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
[<AutoOpen>]
module AutoOpen =
  let maybe = Maybe.MaybeBuilder ()

  let outcome = Outcome.OutcomeBuilder ()

  let (|Good|Empty|Warning|Bad|) t =
    match t with
    | Outcome (Just tv, BadTree.Empty)  -> Good     tv
    | Outcome (Nothing, BadTree.Empty)  -> Empty
    | Outcome (tmv, bt) when bt.IsGood  -> Warning  (tmv, bt)
    | Outcome (tmv, bt)                 -> Bad      (tmv, bt)
// ----------------------------------------------------------------------------------------------
