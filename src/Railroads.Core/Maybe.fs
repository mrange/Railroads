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
/// <summary>The type of optional values. Similar to Option but is Maybe is a struct type.</summary>
///
/// <remarks>Use the constructors <c>Just</c> and <c>Nothing</c> to create values of this type.
/// Use the values in the <c>Maybe</c> module to manipulate values of this type,
/// or pattern match against the values directly.
[<Struct>]
type Maybe<'T> =
  /// <summary>The representation of "No value"</summary>
  | Nothing
  /// <summary>The representation of "Value of type 'T"</summary>
  /// <param name="Value">The input value.</param>
  /// <returns>A Maybe value.</returns>
  | Just    of 'T
and 'T maybe = Maybe<'T>
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
/// <summary>Basic operations on Maybe values.</summary>
module Maybe =
  [<GeneralizableValue>]
  let nothing<'T>         = Maybe<'T>.Nothing

  let inline just v       = Just v

  let inline bind t uf    =
    match t with
    | Just tv -> uf tv
    | Nothing -> Nothing

  let arrow f             = fun v -> just (f v)
  let kleisli tf uf       = fun v -> bind (tf v) uf

  let apply f t           =
    match f, t with
    | Just fv, Just tv  -> Just (fv tv)
    | _                 -> Nothing

  let inline filter f t      =
    match t with
    | Just tv   when f tv -> t
    | _                   -> Nothing

  let inline map m t      =
    match t with
    | Just tv   -> Just (m tv)
    | Nothing   -> Nothing

  let inline flatten t    =
    match t with
    | Just ttv  -> ttv
    | Nothing   -> Nothing

  let andAlso t u         =
    match t, u with
    | Just tv, Just uv  -> Just struct (tv, uv)
    | _                 -> Nothing

  let inline orElse t u   =
    match t, u with
    | Just _ , _        -> t
    | _                 -> u

  let trace n t           =
    printfn "Maybe(%s): %A" n (t : Maybe<_>)
    t

  let inline fromObj o    =
    if not (obj.ReferenceEquals (o, null)) then just o
    else nothing

  let inline fromOption o =
    match o with
    | Some v  -> just v
    | None    -> nothing

  let inline fromResult r =
    match r with
    | Ok v      -> just v
    | Error e   -> nothing

  let inline valueOr dv t =
    match t with
    | Just v    -> v
    | Nothing   -> dv

  type MaybeBuilder ()    =
    member inline x.Bind        (t, uf) = bind t uf
    member inline x.Return      v       = just v
    member inline x.ReturnFrom  t       = t : Maybe<_>
    member inline x.Zero        ()      = just LanguagePrimitives.GenericZero
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
type Maybe<'T> with
  static member inline ( >>=  )  (t, uf) = Maybe.bind     t uf
  static member inline ( |>>  ) (t, m)  = Maybe.map      m t
  static member inline ( <*>  ) (f, t)  = Maybe.apply    f t
  static member inline ( <&>  ) (t, u)  = Maybe.andAlso  t u
  static member inline ( .>>. ) (t, u)  = Maybe.andAlso  t u
  static member inline ( <|>  ) (t, u)  = Maybe.orElse   t u
// ----------------------------------------------------------------------------------------------
(*
// ----------------------------------------------------------------------------------------------
module KleisliMaybe =
  type KleisliMaybe = KleisliMaybe with
    static member        (?<-) (KleisliMaybe, a , b) = Maybe.bind a b
    static member inline (?<-) (KleisliMaybe, a , b) = a >=> b

  let inline (>=>) a b = (?<-) KleisliMaybe a b
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
open KleisliMaybe
// ----------------------------------------------------------------------------------------------
*)

