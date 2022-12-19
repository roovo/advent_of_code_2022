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

        output =
            parse inputParser contents (\s -> Str.countUtf8Bytes s == 0)
            |> Result.map (\r -> List.map r Rps.Round.score)
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
        Parser.Core.sepBy Rps.Round.parser lineFeedParser

    const (\rs -> \_ -> rs)
    |> apply roundsParer
    |> apply (Parser.Core.many lineFeedParser)



### TESTS - inputParser

expect
    parsedInput = parse inputParser "A Y\nB X\nC Z" (\leftover -> Str.countUtf8Bytes leftover == 0)
    parsedInput == Ok [
        {opponent: Rock, you: Paper },
        {opponent: Paper, you: Rock },
        {opponent: Scisors, you: Scisors },
    ]

# these are not great parse errors!
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

