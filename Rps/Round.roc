interface Rps.Round
    exposes [
        Round,
        parser,
        score,
    ]
    imports [
        Parser.Core.{ Parser, apply, const, parse },
        Parser.Helper.{ spaceParser },
        Rps.Play.{ Play },
    ]


Round : { opponent : Play, you : Play }


parser : Parser Str Round
parser =
    const (\o -> \_ -> \y -> { opponent: o, you: y })
        |> apply Rps.Play.parser
        |> apply spaceParser
        |> apply Rps.Play.parser


score : Round -> Nat
score = \round ->
    choiceScore = \r ->
        when r is
            Rock -> 1
            Paper -> 2
            Scisors -> 3

    resultScore =
        if choiceScore round.opponent % 3 == choiceScore round.you - 1 then
            6
        else if round.opponent == round.you then
            3
        else
            0

    choiceScore round.you + resultScore


### TESTS - parser

expect
    parsedRound = parse parser "B Z" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedRound == Ok { opponent: Paper, you: Scisors }

expect
    parsedRound = parse parser "BZ" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedRound == Err (ParsingFailure "whitespace expected")

expect
    parsedRound = parse parser "K Z" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedRound == Err (ParsingFailure "unexpected play, can only be one of: A, B, C, X, Y, or Z")

expect
    parsedRound = parse parser "A Zx" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedRound == Err (ParsingIncomplete "x")


### TESTS - score

expect
    result = score {opponent: Rock, you: Paper }
    result == 2 + 6

expect
    result = score {opponent: Paper, you: Rock }
    result == 1 + 0

expect
    result = score {opponent: Scisors, you: Scisors }
    result == 3 + 3
