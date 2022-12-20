interface Rps.Play
    exposes [
        Play,
        parser,
    ]
    imports [
        Parser.Core.{ Parser, buildPrimitiveParser, parse },
    ]

Play : [Rock, Paper, Scisors]

parser : Parser (List U8) Play
parser =
    input <- buildPrimitiveParser
    when input is
        ['A',..] | ['X',..] -> Ok { val: Rock, input: List.dropFirst input }
        ['B',..] | ['Y',..] -> Ok { val: Paper, input: List.dropFirst input }
        ['C',..] | ['Z',..] -> Ok { val: Scisors, input: List.dropFirst input }
        _ -> Err (ParsingFailure "unexpected play, can only be one of: A, B, C, X, Y, or Z")


# --- TESTS

expect
    parsedPlay = parse parser ['A'] List.isEmpty
    parsedPlay == Ok Rock

expect
    parsedPlay = parse parser ['B'] List.isEmpty
    parsedPlay == Ok Paper

expect
    parsedPlay = parse parser ['C'] List.isEmpty
    parsedPlay == Ok Scisors

expect
    parsedPlay = parse parser ['X'] List.isEmpty
    parsedPlay == Ok Rock

expect
    parsedPlay = parse parser ['Y'] List.isEmpty
    parsedPlay == Ok Paper

expect
    parsedPlay = parse parser ['Z'] List.isEmpty
    parsedPlay == Ok Scisors

expect
    parsedPlay = parse parser ['K'] List.isEmpty
    parsedPlay == Err (ParsingFailure "unexpected play, can only be one of: A, B, C, X, Y, or Z")

expect
    parsedPlay = parse parser ['A', 'B'] List.isEmpty
    parsedPlay == Err (ParsingIncomplete ['B'])
