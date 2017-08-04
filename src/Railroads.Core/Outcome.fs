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
type [<AbstractClass>] BadOutcome() =
  class
    abstract Category         : string
    abstract Description      : string
    abstract BadType          : System.Type
    abstract BadValue         : obj
    override x.ToString ()    = sprintf "Bad outcome: %s - %s" x.Category x.Description
    override x.GetHashCode () =
      let bv = x.BadValue
      if bv <> null then bv.GetHashCode () else 0x55555555
    override x.Equals o       =
      match o with
      | :? BadOutcome as bo ->
        let bv  = x.BadValue
        let obv = bo.BadValue
        if bv <> null && obv <> null then bv.Equals obv
        else bv = null && obv = null
      | _                   ->
        false
  end
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
type [<AbstractClass>] BoxedBadOutcome () =
  class
    inherit BadOutcome ()

  end
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
type [<Sealed>] BoxedBadOutcome<'T> (v: 'T) =
  class
    inherit BoxedBadOutcome ()

    let d = sprintf "%A" v

    member   x.Boxed        = v
    override x.Category     = "Boxed"
    override x.Description  = d
    override x.BadType      = typeof<'T>
    override x.BadValue     = box v
  end

type [<Sealed>] ExceptionBadOutcome (e: exn) =
  class
    inherit BadOutcome ()

    let d = e.Message

    member   x.Exception    = e
    override x.Category     = "Exception"
    override x.Description  = d
    override x.BadType      = typeof<exn>
    override x.BadValue     = box e
  end
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
type [<Sealed>] MessageBadOutcome (m: string) =
  class
    inherit BadOutcome ()

    member   x.Message      = m
    override x.Category     = "Message"
    override x.Description  = m
    override x.BadType      = typeof<string>
    override x.BadValue     = box m
  end
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
[<RequireQualifiedAccess>]
type BadTree =
  | Empty
  | Suppress  of BadTree
  | Leaf      of BadOutcome
  | Fork      of BadTree*BadTree

  member x.IsGood =
    match x with
    | Empty
    | Suppress _  -> true
    | Leaf _
    | Fork _      -> false

  member x.IsBad  = not x.IsGood

  member x.Join y =
    match x, y with
    | Empty     ,  _          -> y
    | _         ,  Empty      -> x
    | Suppress x, Suppress y  -> Suppress (Fork (x, y))
    | _         , _           -> Fork (x, y)

  member x.SuppressIfNeeded () =
    match x with
    | Empty
    | Suppress _  -> x
    | _           -> Suppress x

  member x.Flatten () =
    let ra = ResizeArray 16

    let rec loop s t =
      match t with
      | Empty         -> ()
      | Leaf  bo      -> ra.Add struct (s, bo)
      | Suppress bt   -> loop false bt
      | Fork  (l, r)  -> loop s l; loop s r
    loop true x

    ra.ToArray ()

  member x.Describe (separator : string) =
    let sb            = System.Text.StringBuilder 16
    let inline app s  = sb.Append (s : string) |> ignore

    let rec loop s f t =
      match t with
      | Empty         -> ()
      | Leaf  o       ->
        if not f then app separator
        app (if s then "" else "(Suppressed) ")
        app o.Category
        app ": "
        app o.Description
      | Suppress bt   -> loop false f bt
      | Fork  (l, r)  -> loop s f l; loop s false r
    loop true true x

    sb.ToString ()
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
[<Struct>]
type Outcome<'T>  = Outcome of Maybe<'T>*BadTree
and 'T outcome    = Outcome<'T>
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
module Outcome =
  [<GeneralizableValue>]
  let empty<'T>         = Outcome (Nothing, BadTree.Empty)
//  let inline outcome v b= Outcome (v, b)
  let inline good    g  = Outcome (Just g , BadTree.Empty)
  let inline bad     bt = Outcome (Nothing, bt)
  let badLeaf        b  = bad (BadTree.Leaf b)
  let badMessage     m  = badLeaf (MessageBadOutcome m)
  let badMessagef    f  = FSharp.Core.Printf.kprintf badMessage f
  let badException   e  = badLeaf (ExceptionBadOutcome e)
  let badValue       v  = badLeaf (BoxedBadOutcome<_> v)

  let bind t uf         =
    match t with
    | Outcome (Just tv, tbt) when tbt.IsGood ->
      match uf tv with
      | Outcome (umv, ubt)    -> Outcome (umv, tbt.Join ubt)
    | Outcome (_      , tbt)  -> Outcome (Nothing, tbt)

  let forceBind t uf    =
    match t with
    | Outcome (Just tv, tbt)  ->
      match uf tv with
      | Outcome (umv, ubt)    -> Outcome (umv, tbt.Join ubt)
    | Outcome (Nothing, tbt)  -> Outcome (Nothing, tbt)

  let bindLeft t uf     =
    match t with
    | Outcome (Just tv, tbt) when tbt.IsGood ->
      match uf tv with
      | Outcome (_, ubt)-> Outcome (Just tv, tbt.Join ubt)
    | Outcome (tmv, tbt)  -> Outcome (tmv, tbt)

  let forceBindLeft t uf=
    match t with
    | Outcome (Just tv, tbt)  ->
      match uf tv with
      | Outcome (_, ubt)-> Outcome (Just tv, tbt.Join ubt)
    | Outcome (tmv, tbt)  -> Outcome (tmv, tbt)

  let bindRight t uf    =
    match t with
    | Outcome (_, tbt) when tbt.IsGood ->
      match uf () with
      | Outcome (umv, ubt)    -> Outcome (umv, tbt.Join ubt)
    | Outcome (_, tbt)  -> Outcome (Nothing, tbt)

  let forceBindRight t uf=
    match t with
    | Outcome (_, tbt)  ->
      match uf () with
      | Outcome (umv, ubt)    -> Outcome (umv, tbt.Join ubt)

  let arrow f             = fun v -> good (f v)
  let kleisli tf uf       = fun v -> bind (tf v) uf
  let forceKleisli tf uf  = fun v -> forceBind (tf v) uf

  let left t u        =
    match t, u with
    | Outcome (tmv, tbt), Outcome (_, ubt)  -> Outcome (tmv, tbt.Join ubt)

  let right t u     =
    match t, u with
    | Outcome (_, tbt), Outcome (umv, ubt)  -> Outcome (umv, tbt.Join ubt)

  let apply f t         =
    match f, t with
    | Outcome (Just fv, fbt) , Outcome (Just tv, tbt) when fbt.IsGood && tbt.IsGood ->
      Outcome (Just (fv tv), fbt.Join tbt)
    | Outcome (_      , fbt) , Outcome (_      , tbt) ->
      Outcome (Nothing, fbt.Join tbt)

  let forceApply f t    =
    match f, t with
    | Outcome (Just fv, fbt) , Outcome (Just tv, tbt) ->
      Outcome (Just (fv tv), fbt.Join tbt)
    | Outcome (_      , fbt) , Outcome (_      , tbt) ->
      Outcome (Nothing, fbt.Join tbt)

  let map m t           =
    match t with
    | Outcome (Just tv, tbt) when tbt.IsGood  -> Outcome (Just (m tv), tbt)
    | Outcome (_      , tbt)                  -> Outcome (Nothing, tbt)

  let forceMap m t      =
    match t with
    | Outcome (Just tv, tbt) -> Outcome (Just (m tv), tbt)
    | Outcome (Nothing, tbt) -> Outcome (Nothing, tbt)

  let flatten t         =
    match t with
    | Outcome (Just tv, tbt)  ->
      match tv with
      | Outcome (ttmv, ttbt)  -> Outcome (ttmv, ttbt.Join tbt)
    | Outcome (Nothing, tbt)  -> Outcome (Nothing, tbt)

  let suppress      t   =
    match t with
    | Outcome   (tmv, tbt) -> Outcome (tmv, tbt.SuppressIfNeeded ())

  let trace n t         =
    printfn "Outcome(%s): %A " n (t : Outcome<_>)
    t

  let ``and`` t u       =
    match t, u with
    | Outcome (Just tv, tbt) , Outcome (Just uv, ubt) ->
      Outcome (Just struct (tv, uv), tbt.Join ubt)
    | Outcome (_, tbt) , Outcome (_, ubt)             ->
      Outcome (Nothing, tbt.Join ubt)

  let andAlso t uf =
    match t with
    | Outcome (Just tv, tbt) when tbt.IsGood  ->
      match uf () with
      | Outcome (Just uv, ubt)  ->
        Outcome (Just struct (tv, uv), tbt.Join ubt)
      | Outcome (_, ubt)        ->
        Outcome (Nothing, tbt.Join ubt)
    | Outcome (_, tbt)                        ->
      Outcome (Nothing, tbt)

  let ``or`` t u        =
    match t, u with
    | Outcome (Just tv, tbt) , Outcome (_      , ubt) when tbt.IsGood             ->
      Outcome (Just tv, (tbt.Join (ubt.SuppressIfNeeded ())))
    | Outcome (_      , tbt) , Outcome (Just uv, ubt) when ubt.IsGood             ->
      Outcome (Just uv, (tbt.SuppressIfNeeded ()).Join ubt)
    | Outcome (_      , tbt) , Outcome (umv    , ubt) when tbt.IsBad && ubt.IsBad ->
      Outcome (umv, tbt.Join ubt)
    | Outcome (_      , tbt) , Outcome (_      , ubt)                             ->
      Outcome (Nothing, (tbt.Join ubt).SuppressIfNeeded ())

  let orElse t uf  =
    match t with
    | Outcome (Just tv, tbt) when tbt.IsGood  ->
      Outcome (Just tv, tbt)
    | Outcome (_, tbt)                        ->
      match uf () with
      | Outcome (Just uv, ubt) when ubt.IsGood          ->
        Outcome (Just uv, (tbt.SuppressIfNeeded ()).Join ubt)
      | Outcome (umv, ubt) when tbt.IsBad && ubt.IsBad  ->
        Outcome (umv, tbt.Join ubt)
      | Outcome (_, ubt)                                ->
        Outcome (Nothing, (tbt.Join ubt).SuppressIfNeeded ())

  let validate m v t    =
    match t with
    | Outcome (Just tv, tbt) when tbt.IsGood  ->
      if v tv then
        t
      else
        left t (badMessage m)
    | _                                     -> t

  let forceValidate m v t =
    match t with
    | Outcome (Just tv, _)  ->
      if v tv then
        t
      else
        left t (badMessage m)
    | _                     -> t

  let inline catch f    =
    try
      good (f ())
    with
    | e -> badException e

  let badIfNone m o     =
    match o with
    | Some v  -> good v
    | None    -> badMessage m

  let badIfNull m o     =
    if not (obj.ReferenceEquals (o, null)) then good o
    else badMessage m

  let emptyIfNone o     =
    match o with
    | Some v  -> good v
    | None    -> empty

  let emptyIfNull o     =
    if not (obj.ReferenceEquals (o, null)) then good o
    else empty

  let fromResult r      =
    match r with
    | Ok v      -> good v
    | Error e   -> badValue e

  let join (e : System.Collections.Generic.IEnumerable<_>) =
    let ra  = ResizeArray 16
    use e   = e.GetEnumerator ()

    let rec loop (bt : BadTree) =
      if e.MoveNext () then
        let tbt =
          match e.Current with
          | Outcome (Just tv, tbt)  -> ra.Add tv; tbt
          | Outcome (_      , tbt)  -> tbt

        loop (bt.Join tbt)
      else
        bt
    let bt = loop BadTree.Empty

    Outcome (Just (ra.ToArray ()), bt)

  let fold folder initial (e : System.Collections.Generic.IEnumerable<_>) =
    use e   = e.GetEnumerator ()

    let rec loop s =
      if e.MoveNext () then
        let f = folder s e.Current
        match f with
        | Outcome (Just fv, fbt) when fbt.IsGood  -> loop fv
        | Outcome (_      , fbt)                  -> Outcome (Nothing, fbt)
      else
        good s
    loop initial

  type OutcomeBuilder () =
    member inline x.Bind        (t, uf) = bind t uf
    member inline x.Delay       f       =
      try
        f ()
      with
      | e -> badException e
    member inline x.Return      v       = good v
    member inline x.ReturnFrom  t       = t : Outcome<_>
    member inline x.Zero        ()      = good LanguagePrimitives.GenericZero

  type ForcedOutcomeBuilder () =
    member inline x.Bind        (t, uf) = forceBind t uf
    member inline x.Delay       f       =
      try
        f ()
      with
      | e -> badException e
    member inline x.Return      v       = good v
    member inline x.ReturnFrom  t       = t : Outcome<_>
    member inline x.Zero        ()      = good LanguagePrimitives.GenericZero

type Outcome<'T> with
  static member inline ( >>=  ) (t, uf)  = Outcome.bind           t uf
  static member inline ( >>=! ) (t, uf)  = Outcome.forceBind      t uf
  static member inline ( .>>  ) (t, uf)  = Outcome.bindLeft       t uf
  static member inline ( .>>! ) (t, uf)  = Outcome.forceBindLeft  t uf
  static member inline ( >>.  ) (t, uf)  = Outcome.bindRight      t uf
  static member inline ( >>.! ) (t, uf)  = Outcome.forceBindRight t uf
  static member inline ( |>>  ) (t, m)   = Outcome.map            m t
  static member inline ( |>>! ) (t, m)   = Outcome.forceMap       m t
  static member inline ( <*>  ) (f, t)   = Outcome.apply          f t
  static member inline ( <*>! ) (f, t)   = Outcome.forceApply     f t
  static member inline ( <&>  ) (t, u)   = Outcome.``and``        t u
  static member inline ( <&&> ) (t, uf)  = Outcome.andAlso        t uf
  static member inline ( <|>  ) (t, u)   = Outcome.``or``         t u
  static member inline ( <||> ) (t, uf)  = Outcome.orElse         t uf
  static member inline ( <?>  ) (t, u)   = Outcome.left           t u
// ----------------------------------------------------------------------------------------------

(*
// ----------------------------------------------------------------------------------------------
module KleisliOutcome =
  type KleisliOutcome = KleisliOutcome with
    static member        (?<-) (KleisliOutcome, a , b) = Outcome.bind a b
    static member inline (?<-) (KleisliOutcome, a , b) = a >=> b

  let inline (>=>) a b = (?<-) KleisliOutcome a b
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
open KleisliOutcome
// ----------------------------------------------------------------------------------------------
*)
