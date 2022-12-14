app "aoc_day1"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.3/5SXwdW7rH8QAOnD71IkHcFxCmBEPtFSLAIkclPEgjHQ.tar.br" }
    imports [
        pf.File,
        pf.Path,
        pf.Stdout,
        pf.Task.{ Task },
    ]
    provides [main] to pf


main : Task {} []
main =
   filePath = Path.fromStr "day01_input.txt"

   task =
        contents <- File.readUtf8 filePath |> Task.await

        caloriesPerElf : List U32
        caloriesPerElf = sumIntegerBlocks contents

        part1 : Str
        part1 = caloriesPerElf
                |> List.max
                |> Result.withDefault 0
                |> Num.toStr


        part2 : Str
        part2 = caloriesPerElf
                |> List.sortDesc
                |> List.takeFirst 3
                |> List.sum
                |> Num.toStr

        _ <- Stdout.line "part1: \(part1)" |> Task.await
        Stdout.line "part2: \(part2)"

   Task.onFail task \_ -> Stdout.line "Oops something went wrong."


sumIntegerBlocks : Str -> List U32
sumIntegerBlocks = \integerBlocks ->
    integerBlocks
    |> Str.split "\n\n"
    |> List.map sumIntegerBlock


sumIntegerBlock : Str -> U32
sumIntegerBlock = \integerBlock ->
    integerBlock
    |> Str.split "\n"
    |> List.map Str.toU32
    |> List.map (\r -> Result.withDefault r 0)
    |> List.sum


expect
    summedBlocks = sumIntegerBlocks   """1000
                                      2000
                                      3000

                                      4000

                                      5000
                                      6000

                                      bar

                                      7000
                                      8000
                                      9000

                                      10000
                                      """
    summedBlocks == [6000, 4000, 11000, 0, 24000, 10000]

expect
    summedInts = sumIntegerBlock "1000\n2000\nfoo\n3000"
    summedInts == 6000
