interface Parser.Helper
    exposes [
        lineFeedParser,
        spaceParser,
    ]
    imports [
        Parser.Core.{ Parser, buildPrimitiveParser, parse },
    ]

lineFeedParser : Parser Str {}
lineFeedParser =
    buildPrimitiveParser (\input ->
        if Str.startsWith input "\n" then
            Ok { val: {}, input : Str.replaceFirst input "\n" "" |> Result.withDefault input }
        else
            Err (ParsingFailure "linefeed expected")
    )

expect
    parsedLineFeed = parse lineFeedParser "\n" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedLineFeed == Ok {}

expect
    parsedLineFeed = parse lineFeedParser "x" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedLineFeed == Err (ParsingFailure "linefeed expected")

expect
    parsedLineFeed = parse lineFeedParser "\nx" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedLineFeed == Err (ParsingIncomplete "x")


spaceParser : Parser Str {}
spaceParser =
    buildPrimitiveParser (\input ->
        if Str.startsWith input " " then
            Ok { val: {}, input : Str.replaceFirst input " " "" |> Result.withDefault input }
        else
            Err (ParsingFailure "whitespace expected")
    )

expect
    parsedSpace = parse spaceParser " " (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedSpace == Ok {}

expect
    parsedSpace = parse spaceParser "x" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedSpace == Err (ParsingFailure "whitespace expected")

expect
    parsedSpace = parse spaceParser " x" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedSpace == Err (ParsingIncomplete "x")
