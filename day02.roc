app "aoc_day2"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.3/5SXwdW7rH8QAOnD71IkHcFxCmBEPtFSLAIkclPEgjHQ.tar.br" }
    imports [
        pf.File,
        pf.Path,
        pf.Stdout,
        pf.Task.{ Task },
        Parser.Core.{ Parser, apply, const, parse },
        Parser.Helper.{ lineFeedParser },
        Rps.Round.{ Round },
    ]
    provides [main] to pf


main : Task {} []
main =
   filePath = Path.fromStr "day02_input.txt"

   task =
        contents <- File.readUtf8 filePath |> Task.await

        part1 : Str
        part1 =
            parseAndScore Rps.Round.parser contents

        part2 : Str
        part2 =
            parseAndScore Rps.Round.parserPart2 contents


        _ <- Stdout.line "part1: \(part1)" |> Task.await
        Stdout.line "part1: \(part2)"

   Task.onFail task \_ -> Stdout.line "Oops something went wrong."


### PRIVATE

inputParser : Parser Str Round -> Parser Str (List Round)
inputParser = \roundParser ->
    roundsParer =
        Parser.Core.sepBy roundParser lineFeedParser

    const (\rs -> \_ -> rs)
    |> apply roundsParer
    |> apply (Parser.Core.many lineFeedParser)


parseAndScore : Parser Str Round, Str -> Str
parseAndScore = \parser, contents ->
    scoreRounds : List Round -> List Nat
    scoreRounds = \r -> List.map r Rps.Round.score

    parse (inputParser parser) contents (\s -> Str.countUtf8Bytes s == 0)
    |> Result.map scoreRounds
    |> Result.map List.sum
    |> Result.map (\s ->
                        printable = Num.toStr s
                        "The total score is: \(printable)")
    |> Result.withDefault "Ooops, the sum could not be calculated"



### TESTS - inputParser

expect
    parsedInput = parse (inputParser Rps.Round.parser) "A Y\nB X\nC Z" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedInput == Ok [
        {opponent: Rock, you: Paper },
        {opponent: Paper, you: Rock },
        {opponent: Scisors, you: Scisors },
    ]

expect
    parsedInput = parse (inputParser Rps.Round.parserPart2) "A Y\nB X\nC Z" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedInput == Ok [
        {opponent: Rock, you: Rock },
        {opponent: Paper, you: Rock },
        {opponent: Scisors, you: Rock },
    ]

# these are not great parse errors!
expect
    parsedInput = parse (inputParser Rps.Round.parser) "AY\nB X\nC Z" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedInput == Err (ParsingIncomplete "AY\nB X\nC Z")

expect
    parsedInput = parse (inputParser Rps.Round.parser) "A Y\nB XC Z" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedInput == Err (ParsingIncomplete "C Z")

expect
    parsedInput = parse (inputParser Rps.Round.parser) "A Y\nB X\nC Z\n" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedInput == Ok [
        {opponent: Rock, you: Paper },
        {opponent: Paper, you: Rock },
        {opponent: Scisors, you: Scisors },
    ]
