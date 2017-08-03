# Railroads

`Railroads` are libraries designed to support Railway Oriented Programming (also known as ROP) in F#

## TL; DR

Railway Oriented Programming is a functional approach to error-handling intending
not to let the error-handling code overshadow the happy-path code.

```fsharp
// Example of Railway Oriented code
//  on error the flow will be interrupted with neither `if..else` nor `try..with`
extractCustomerIdFromInput input
  >>= readCustomerFromDatabase productionDatabase
  >>= prepareWelcomeMail
  >>= sendWelcomeMail productionSmtpServer
  >>= markCustomerAsWelcomeMailSent productionDatabase
```

## Motivation

[Scott Wlaschin](https://twitter.com/ScottWlaschin) has written excellent articles
on functional error-handling called
["Railway Oriented Programming"](https://fsharpforfunandprofit.com/rop/).

Scott has this blurb on ROP

> Many examples in functional programming assume that you are always on the “happy path”. But to create a robust real world application you must deal with validation, logging, network and service errors, and other annoyances.
>
> So, how do you handle all this in a clean functional way?
>
> This talk will provide a brief introduction to this topic, using a fun and easy-to-understand railway analogy.

The blog is a great resource of information and has inspired many F# developers and libraries such as [Chessie](https://fsprojects.github.io/Chessie/).

Is there a need for an additional library when we have `Option` and `Result` in F# and `Chessie` and OSS libraries such as `Chessie` exists?

`Railroads` currently provide two ROP enabling types `Maybe` and `Outcome`.

`Maybe` is essentially like `Option` but it is implemented as a `struct` type in order to reduce GC pressure:

```fsharp
[<Struct>]
type Maybe<'T> =
  | Nothing
  | Just    of 'T
and 'T maybe = Maybe<'T>
```

`Outcome` is designed to overcome the problem that `Option`/`Maybe` doesn't capture any information on what went wrong. This is also a motivation behind F# and `Chessie` `Result` type.

Consider this function like this:

```fsharp
val tryParseInt : string -> int option
```

If this function returns `None` we have no knowledge on what went wrong and can't trace or tell our users what the problem was.

Instead we could use F# `Result`:

```fsharp
type ParseIntFailureReasons = EmptyString | TrailingGarbage | NoDigits | Whatever
val tryParseInt : string -> Result<int, ParseIntFailureReasons>
```

`tryParseInt` now tells us what went wrong. The problem is that binding `Result` with different `Error` types is difficult.

Consider:

```fsharp
type ParseIntFailureReasons = EmptyString | TrailingGarbage | NoDigits | Whatever
val tryParseInt : string -> Result<int, ParseIntFailureReasons>

type DatabaseFailureReasons = ConnectionFailed | CouldNotAuthenticae | SomethingElse
val readCustomerAge : uint64 -> Result<string, DatabaseFailureReasons>

// Doesn't work because Result.bind requires the same Error type
let customerAge : Result<int, ???> = readCustomerAge 123UL |> Result.bind tryParseInt
```

A way around this is mapping the `Error` type into `obj` before binding:

```fsharp
type ParseIntFailureReasons = EmptyString | TrailingGarbage | NoDigits | Whatever
val tryParseInt : string -> Result<int, ParseIntFailureReasons>

type DatabaseFailureReasons = ConnectionFailed | CouldNotAuthenticae | SomethingElse
val readCustomerAge : uint64 -> Result<string, DatabaseFailureReasons>

let customerAge : Result<int, obj> = (readCustomerAge 123UL |> Result.mapError box) |> Result.bind (tryParseInt >> Result.mapError box)
```

We lose type-safety and it feels clunky.

In my opinion this reduces the perceived value of `Result` over `Option`.

This is the motivation for `Railroads` provide ROP support with capturing and aggregation of error information.

## Outcome

`Outcome` is the type that capture a result or an error, it essentially looks like this:

```fsharp
type [<AbstractClass>] BadOutcome() =
  class
  // Details removed
  end

[<RequireQualifiedAccess>]
type BadTree =
  | Empty
  | Suppress  of BadTree
  | Leaf      of BadOutcome
  | Fork      of BadTree*BadTree

[<Struct>]
type Outcome<'T>  = Outcome of Maybe<'T>*BadTree
and 'T outcome    = Outcome<'T>
```

In order to keep binding fluent `Outcome` doesn't support a generic error-type. Instead all
error-information will be represented as a tree of `BadOutcome`.

A bad outcome could be a message, exception, a boxed value or something else.

Bad outcomes are aggregated together in a tree structure in order preserve all bad outcomes that lead
to the failure to produce a good outcome:

```fsharp
// f is applied iff res0, res1 and res2 are good outcomes
//  otherwise all bad outcomes are aggregated into a single bad outcome
let x = good f <*> res0 <*> res1 <*> res2

// y is the first good outcome of res0, res1, res2
//  however potential bad outcomes are aggregated and suppressed
// if there's no good outcome all bad outcomes will be aggregated
let y = res0 <|> res1 <|> res2

// z is a good tuple iff res0, res1 and res2 are good outcomes
//  otherwise all bad outcomes are aggregated into a single bad outcome
let z = res0 <&> res1 <&> res2

// The previous example using Outcome instead
val tryParseInt : string -> Outcome<int>

val readCustomerAge : uint64 -> Outcome<string>

// Binding readCustomerAge and tryParseInt is unproblematic
let customerAge : Outcome<int> = readCustomerAge 123UL >>= tryParseInt
```

An `Outcome` has 4 possible cases extracted using pattern matching at top level:

```fsharp
match doSomething () with
| Good v            -> printfn "We have a good outcome: %A" v
| Empty             -> printfn "We have an empty outcome"
| Warning (mv, bt)  -> printfn "We have an outcome with suppressed errors: %A, %A" mv bt
| Error   (mv, bt)  -> printfn "We have a bad outcome: %A, %A" mv bt
```

`Good` and `Empty` corresponds to `Some` and `None` for `Option`. `Warning` and `Error` results in a tree
of `BadOutcome` which can be traversed or traced. `Warning` and `Error` also has an optional result which
may or may not be useful to you.

## Using Outcome with Scott's example code

Scott gave the following example code:

```fsharp
let executeUseCase =
  receiveRequest
  >> validateRequest
  >> canonicalizeEmail
  >> updateDbFromRequest
  >> sendEmail
  >> returnMessage
```

How will this look with `Outcome`? First we create signatures for the functions used:

```fsharp
val validateRequest     : Request -> Outcome<unit>
val canonicalizeEmail   : Request -> Outcome<CanonicalEmailAddress>
val updateDbFromRequest : Request -> Outcome<unit>
val sendEmail           : Request -> CanonicalEmailAddress -> Outcome<unit>
  >> canonicalizeEmail
  >> updateDbFromRequest
  >> sendEmail
  >> returnMessage
```