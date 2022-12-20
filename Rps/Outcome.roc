interface Rps.Outcome
    exposes [
        Outcome,
        parser,
    ]
    imports [
        Parser.Core.{ Parser, buildPrimitiveParser, parse },
    ]

Outcome : [Lose, Draw, Win]

parser : Parser (List U8) Outcome
parser =
    input <- buildPrimitiveParser
    when input is
        ['X',..] -> Ok { val: Lose, input: List.dropFirst input }
        ['Y',..] -> Ok { val: Draw, input: List.dropFirst input }
        ['Z',..] -> Ok { val: Win, input: List.dropFirst input }
        _ -> Err (ParsingFailure "unexpected outcome, can only be one of: X, Y, or Z")


# --- TESTS

expect
    parsedPlay = parse parser ['X'] List.isEmpty
    parsedPlay == Ok Lose

expect
    parsedPlay = parse parser ['Y'] List.isEmpty
    parsedPlay == Ok Draw

expect
    parsedPlay = parse parser ['Z'] List.isEmpty
    parsedPlay == Ok Win

expect
    parsedPlay = parse parser ['K'] List.isEmpty
    parsedPlay == Err (ParsingFailure "unexpected outcome, can only be one of: X, Y, or Z")

expect
    parsedPlay = parse parser ['X', 'B'] List.isEmpty
    parsedPlay == Err (ParsingIncomplete ['B'])

