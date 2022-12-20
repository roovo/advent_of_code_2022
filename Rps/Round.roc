interface Rps.Round
    exposes [
        Round,
        parser,
        parserPart2,
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
    parserHelper (\o -> \_ -> \y -> { opponent: o, you: y })


parserPart2 : Parser Str Round
parserPart2 =
    parserHelper (\o -> \_ -> \y ->
        when P o y is
        P Rock Rock -> { opponent: o, you: Scisors }
        P Paper Rock -> { opponent: o, you: Rock }
        P Scisors Rock -> { opponent: o, you: Paper }

        P Rock Paper -> { opponent: o, you: Rock }
        P Paper Paper -> { opponent: o, you: Paper }
        P Scisors Paper -> { opponent: o, you: Scisors }

        P Rock Scisors -> { opponent: o, you: Paper }
        P Paper Scisors -> { opponent: o, you: Scisors }
        P Scisors Scisors -> { opponent: o, you: Rock }
    )



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


### PRIVATE

parserHelper : (Play -> ({} -> (Play -> Round))) -> Parser Str Round
parserHelper = \builder ->
    const builder
        |> apply Rps.Play.parser
        |> apply spaceParser
        |> apply Rps.Play.parser


### TESTS - parser

expect
    parsedRound = parse parser "A Y" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedRound == Ok { opponent: Rock, you: Paper }

expect
    parsedRound = parse parser "B X" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedRound == Ok { opponent: Paper, you: Rock }

expect
    parsedRound = parse parser "C Z" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedRound == Ok { opponent: Scisors, you: Scisors }

expect
    parsedRound = parse parser "BZ" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedRound == Err (ParsingFailure "whitespace expected")

expect
    parsedRound = parse parser "K Z" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedRound == Err (ParsingFailure "unexpected play, can only be one of: A, B, C, X, Y, or Z")

expect
    parsedRound = parse parser "A Zx" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedRound == Err (ParsingIncomplete "x")


### TESTS - parserPart2

expect
    parsedRound = parse parserPart2 "A Y" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedRound == Ok { opponent: Rock, you: Rock }

expect
    parsedRound = parse parserPart2 "B X" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedRound == Ok { opponent: Paper, you: Rock }

expect
    parsedRound = parse parserPart2 "C Z" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedRound == Ok { opponent: Scisors, you: Rock }

expect
    parsedRound = parse parserPart2 "BZ" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedRound == Err (ParsingFailure "whitespace expected")

expect
    parsedRound = parse parserPart2 "K Z" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedRound == Err (ParsingFailure "unexpected play, can only be one of: A, B, C, X, Y, or Z")

expect
    parsedRound = parse parserPart2 "A Zx" (\leftover -> Str.countUtf8Bytes leftover == 0)
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
