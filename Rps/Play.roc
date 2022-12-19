interface Rps.Play
    exposes [
        Play,
        parser,
    ]
    imports [
        Parser.Core.{ Parser, buildPrimitiveParser, parse },
    ]

Play : [Rock, Paper, Scisors]

parser : Parser Str Play
parser =
    buildPrimitiveParser (\input ->
        if Str.startsWith input "A" then
            Ok { val: Rock, input : Str.replaceFirst input "A" "" |> Result.withDefault input }
        else if Str.startsWith input "B" then
            Ok { val: Paper, input : Str.replaceFirst input "B" "" |> Result.withDefault input }
        else if Str.startsWith input "C" then
            Ok { val: Scisors, input : Str.replaceFirst input "C" "" |> Result.withDefault input }
        else if Str.startsWith input "X" then
            Ok { val: Rock, input : Str.replaceFirst input "X" "" |> Result.withDefault input }
        else if Str.startsWith input "Y" then
            Ok { val: Paper, input : Str.replaceFirst input "Y" "" |> Result.withDefault input }
        else if Str.startsWith input "Z" then
            Ok { val: Scisors, input : Str.replaceFirst input "Z" "" |> Result.withDefault input }
        else
            Err (ParsingFailure "unexpected play, can only be one of: A, B, C, X, Y, or Z")
    )

expect
    parsedPlay = parse parser "A" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedPlay == Ok Rock

expect
    parsedPlay = parse parser "B" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedPlay == Ok Paper

expect
    parsedPlay = parse parser "C" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedPlay == Ok Scisors

expect
    parsedPlay = parse parser "X" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedPlay == Ok Rock

expect
    parsedPlay = parse parser "Y" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedPlay == Ok Paper

expect
    parsedPlay = parse parser "Z" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedPlay == Ok Scisors

expect
    parsedPlay = parse parser "K" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedPlay == Err (ParsingFailure "unexpected play, can only be one of: A, B, C, X, Y, or Z")

expect
    parsedPlay = parse parser "AB" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedPlay == Err (ParsingIncomplete "B")


