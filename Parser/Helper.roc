interface Parser.Helper
    exposes [
        lineFeedParser,
        spaceParser,
    ]
    imports [
        Parser.Core.{ Parser, buildPrimitiveParser, parse },
    ]

lineFeedParser : Parser (List U8) {}
lineFeedParser =
    input <- buildPrimitiveParser
    when input is
        ['\n', ..] -> Ok { val: {}, input: List.dropFirst input }
        _ -> Err (ParsingFailure "linefeed expected")

spaceParser : Parser (List U8) {}
spaceParser =
    input <- buildPrimitiveParser
    when input is
        [' ', ..] -> Ok { val: {}, input: List.dropFirst input }
        _ -> Err (ParsingFailure "whitespace expected")

# --- TESTS - lineFeedParser
expect
    parsedLineFeed = parse lineFeedParser (Str.toUtf8 "\n") List.isEmpty

    parsedLineFeed == Ok {}

expect
    parsedLineFeed = parse lineFeedParser (Str.toUtf8 "x") List.isEmpty

    parsedLineFeed == Err (ParsingFailure "linefeed expected")

expect
    parsedLineFeed = parse lineFeedParser (Str.toUtf8 "\nx") List.isEmpty

    parsedLineFeed == Err (ParsingIncomplete ['x'])

# --- TESTS - lineFeedParser
expect
    parsedSpace = parse spaceParser (Str.toUtf8 " ") List.isEmpty

    parsedSpace == Ok {}

expect
    parsedSpace = parse spaceParser (Str.toUtf8 "x") List.isEmpty

    parsedSpace == Err (ParsingFailure "whitespace expected")

expect
    parsedSpace = parse spaceParser (Str.toUtf8 " x") List.isEmpty

    parsedSpace == Err (ParsingIncomplete ['x'])
