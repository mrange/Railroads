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

This implies the following function signatures:

```fsharp
val receiveRequest      : Option<unit>    -> Option<Request>
val validateRequest     : Option<Request> -> Option<Request>
val canonicalizeEmail   : Option<Request> -> Option<Request>
val updateDbFromRequest : Option<Request> -> Option<Request>
val sendEmail           : Option<Request> -> Option<Request>
val returnMessage       : Option<Request> -> Option<Response>
```

For `Outcome` I expanded on the function signatures in order to make them a bit
more like we are used to:

```fsharp
val receiveRequest      : unit    -> Outcome<Request>
val validateRequest     : Request -> Outcome<unit>
val canonicalizeEmail   : Request -> Outcome<CanonicalEmailAddress>
val updateDbFromRequest : Request -> Outcome<unit>
val sendEmail           : Request -> CanonicalEmailAddress -> Outcome<unit>
val returnMessage       : Request -> Outcome<Response>
```

Using these function signatures we can recreate Scott's example using `Outcome`.

```fsharp
let executeUseCase =
  // Receives the request
  receiveRequest ()
  // If the request was received successfully then validate it
  .>> validateRequest
  // If the request is valid then extract and canonicalize the email
  //  As we need the req in the future as well pair req and email with <&>
  >>= fun req -> good req <&> canonicalizeEmail req
  // If the request was successfully canonicalized then update the db using the request
  .>> fun struct (req, _)   -> updateDbFromRequest req
  // If the db was successfully updated then send the email using the request and email address
  .>> fun struct (req, cea) -> sendEmail req cea
  // If the email was successfully sent then return a Response
  >>= fun struct (req, _)   -> returnMessage req
```

Using Scott's signature it was up to each function to test the input parameter and take appropriate
action. In addition the signatures were quite restricted.

Using `Outcome` the tests are performed by the combinator operators such as `>>=` and `.>>` and they
allow combination of functions with heterogeneous signatures.

`>>=` is the well-known monadic `bind` that allows flexible combinations of `Outcome`s. `.>>` is
a specialized version of `>>=` in that it discards good results on the right side, this is useful when combining
functions that return `Outcome<unit>` like `validateRequest`.

`Outcome`s can also be combined using F# computation expressions:

```fsharp
let executeUseCase =
  outcome {
    let! req = receiveRequest       ()
    do!        validateRequest      req
    let! cea = canonicalizeEmail    req
    do!        updateDbFromRequest  req
    do!        sendEmail            req cea
    return!    returnMessage req
  }
```
