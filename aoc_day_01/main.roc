app "aoc_day1.1"
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
   filePath = Path.fromStr "input.txt"

   task =
        contents <- File.readUtf8 filePath |> Task.await

        maxCalories =   contents
                        |> Str.split "\n\n"
                        |> List.map caloriesPerElf
                        |> List.max
                        |> Result.withDefault 0
                        |> Num.toStr

        Stdout.line "Max calories: \(maxCalories)"

   Task.attempt task \result ->
        when result is
            Ok {} -> Stdout.line "success!"
            Err _ -> Stdout.line "failure"


caloriesPerElf : Str -> U32
caloriesPerElf = \caloriesText ->
    caloriesText
    |> Str.split "\n"
    |> List.map Str.toU32
    |> List.map (\r -> Result.withDefault r 0)
    |> List.sum


expect caloriesPerElf "1000\n2000\n3000" == 6000
expect caloriesPerElf "1000\n2000\nfoo\n3000" == 6000
