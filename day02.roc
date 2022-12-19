app "aoc_day2"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.3/5SXwdW7rH8QAOnD71IkHcFxCmBEPtFSLAIkclPEgjHQ.tar.br" }
    imports [
        pf.File,
        pf.Path,
        pf.Stdout,
        pf.Task.{ Task },
        Parser.Core.{ Parser, apply, buildPrimitiveParser, const, parse },
    ]
    provides [main] to pf


main : Task {} []
main =
   filePath = Path.fromStr "day02_input.txt"

   task =
        contents <- File.readUtf8 filePath |> Task.await

        output =
            parse inputParser contents (\s -> Str.countUtf8Bytes s == 0)
            |> Result.map (\r -> List.map r score)
            |> Result.map List.sum
            |> Result.map (\s ->
                                printable = Num.toStr s
                                "The total score is: \(printable)")
            |> Result.withDefault "Ooops, the sum could not be calculated"


        Stdout.line "part1: \(output)"

   Task.onFail task \_ -> Stdout.line "Oops something went wrong."


inputParser : Parser Str (List Round)
inputParser =
    roundsParer =
        Parser.Core.sepBy roundParser lineFeedParser

    const (\rs -> \_ -> rs)
    |> apply roundsParer
    |> apply (Parser.Core.many lineFeedParser)



expect
    parsedInput = parse inputParser "A Y\nB X\nC Z" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedInput == Ok [
        {opponent: Rock, you: Paper },
        {opponent: Paper, you: Rock },
        {opponent: Scisors, you: Scisors },
    ]

# these aree not great parse errors!
expect
    parsedInput = parse inputParser "AY\nB X\nC Z" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedInput == Err (ParsingIncomplete "AY\nB X\nC Z")

expect
    parsedInput = parse inputParser "A Y\nB XC Z" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedInput == Err (ParsingIncomplete "C Z")

expect
    parsedInput = parse inputParser "A Y\nB X\nC Z\n" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedInput == Ok [
        {opponent: Rock, you: Paper },
        {opponent: Paper, you: Rock },
        {opponent: Scisors, you: Scisors },
    ]

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

expect
    result = score {opponent: Rock, you: Paper }
    result == 2 + 6

expect
    result = score {opponent: Paper, you: Rock }
    result == 1 + 0

expect
    result = score {opponent: Scisors, you: Scisors }
    result == 3 + 3

### PLAY

Play : [Rock, Paper, Scisors]

playParser : Parser Str Play
playParser =
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
    parsedPlay = parse playParser "A" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedPlay == Ok Rock

expect
    parsedPlay = parse playParser "B" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedPlay == Ok Paper

expect
    parsedPlay = parse playParser "C" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedPlay == Ok Scisors

expect
    parsedPlay = parse playParser "X" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedPlay == Ok Rock

expect
    parsedPlay = parse playParser "Y" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedPlay == Ok Paper

expect
    parsedPlay = parse playParser "Z" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedPlay == Ok Scisors

expect
    parsedPlay = parse playParser "K" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedPlay == Err (ParsingFailure "unexpected play, can only be one of: A, B, C, X, Y, or Z")

expect
    parsedPlay = parse playParser "AB" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedPlay == Err (ParsingIncomplete "B")


### WHITESPACE

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


### ROUND

Round : { opponent : Play, you : Play }

roundParser : Parser Str Round
roundParser =
    const (\o -> \_ -> \y -> { opponent: o, you: y })
        |> apply playParser
        |> apply spaceParser
        |> apply playParser

expect
    parsedRound = parse roundParser "B Z" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedRound == Ok { opponent: Paper, you: Scisors }

expect
    parsedRound = parse roundParser "BZ" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedRound == Err (ParsingFailure "whitespace expected")

expect
    parsedRound = parse roundParser "K Z" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedRound == Err (ParsingFailure "unexpected play, can only be one of: A, B, C, X, Y, or Z")

expect
    parsedRound = parse roundParser "A Zx" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedRound == Err (ParsingIncomplete "x")
