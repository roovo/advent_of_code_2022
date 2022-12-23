interface Day02.Round
    exposes [
        Round,
        fromPlayParser,
        fromOutcomeParser,
        score,
    ]
    imports [
        Parser.Core.{ Parser, apply, const, parse },
        Parser.Helper.{ spaceParser },
        Day02.Outcome.{ Outcome },
        Day02.Play.{ Play },
    ]

Round : { opponent : Play, you : Play }

fromPlayParser : Parser (List U8) Round
fromPlayParser =
    const (\o -> \_ -> \y -> { opponent: o, you: y })
    |> apply Day02.Play.parser
    |> apply spaceParser
    |> apply Day02.Play.parser

fromOutcomeParser : Parser (List U8) Round
fromOutcomeParser =
    fromOutcome = \o -> \_ -> \y ->
                when P o y is
                    P Rock Lose -> { opponent: o, you: Scisors }
                    P Paper Lose -> { opponent: o, you: Rock }
                    P Scisors Lose -> { opponent: o, you: Paper }
                    P Rock Draw -> { opponent: o, you: Rock }
                    P Paper Draw -> { opponent: o, you: Paper }
                    P Scisors Draw -> { opponent: o, you: Scisors }
                    P Rock Win -> { opponent: o, you: Paper }
                    P Paper Win -> { opponent: o, you: Scisors }
                    P Scisors Win -> { opponent: o, you: Rock }

    const fromOutcome
    |> apply Day02.Play.parser
    |> apply spaceParser
    |> apply Day02.Outcome.parser

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

# --- TESTS - fromPlayParser
expect
    parsedRound = parse fromPlayParser (Str.toUtf8 "A Y") List.isEmpty

    parsedRound == Ok { opponent: Rock, you: Paper }

expect
    parsedRound = parse fromPlayParser (Str.toUtf8 "B X") List.isEmpty

    parsedRound == Ok { opponent: Paper, you: Rock }

expect
    parsedRound = parse fromPlayParser (Str.toUtf8 "C Z") List.isEmpty

    parsedRound == Ok { opponent: Scisors, you: Scisors }

expect
    parsedRound = parse fromPlayParser (Str.toUtf8 "BZ") List.isEmpty

    parsedRound == Err (ParsingFailure "whitespace expected")

expect
    parsedRound = parse fromPlayParser (Str.toUtf8 "K Z") List.isEmpty

    parsedRound == Err (ParsingFailure "unexpected play, can only be one of: A, B, C, X, Y, or Z")

expect
    parsedRound = parse fromPlayParser (Str.toUtf8 "A Zx") List.isEmpty

    parsedRound == Err (ParsingIncomplete ['x'])

# --- TESTS - fromOutcomeParser
expect
    parsedRound = parse fromOutcomeParser (Str.toUtf8 "A Y") List.isEmpty

    parsedRound == Ok { opponent: Rock, you: Rock }

expect
    parsedRound = parse fromOutcomeParser (Str.toUtf8 "B X") List.isEmpty

    parsedRound == Ok { opponent: Paper, you: Rock }

expect
    parsedRound = parse fromOutcomeParser (Str.toUtf8 "C Z") List.isEmpty

    parsedRound == Ok { opponent: Scisors, you: Rock }

expect
    parsedRound = parse fromOutcomeParser (Str.toUtf8 "BZ") List.isEmpty

    parsedRound == Err (ParsingFailure "whitespace expected")

expect
    parsedRound = parse fromOutcomeParser (Str.toUtf8 "K Z") List.isEmpty

    parsedRound == Err (ParsingFailure "unexpected play, can only be one of: A, B, C, X, Y, or Z")

expect
    parsedRound = parse fromOutcomeParser (Str.toUtf8 "A Zx") List.isEmpty

    parsedRound == Err (ParsingIncomplete ['x'])

# --- TESTS - score
expect
    result = score { opponent: Rock, you: Paper }

    result == 2 + 6

expect
    result = score { opponent: Paper, you: Rock }

    result == 1 + 0

expect
    result = score { opponent: Scisors, you: Scisors }

    result == 3 + 3
