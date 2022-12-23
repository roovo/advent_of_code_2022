app "aoc_day3"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.3/5SXwdW7rH8QAOnD71IkHcFxCmBEPtFSLAIkclPEgjHQ.tar.br" }
    imports [
        pf.File,
        pf.Path,
        pf.Stdout,
        pf.Task.{ Task },
        Day03.Rucksack.{ Rucksack },
    ]
    provides [main] to pf

main : Task {} []
main =
    filePath = Path.fromStr "day03_input.txt"

    task =
        contents <- File.readUtf8 filePath |> Task.await

        rucksacks : List (Rucksack U8)
        rucksacks =
            contents
            |> Str.split "\n"
            |> List.map Day03.Rucksack.pack

        part1 : Str
        part1 =
            rucksacks
            |> List.map Day03.Rucksack.findDuplicates
            |> List.map scoreItems
            |> List.sum
            |> Num.toStr

        Stdout.line "part1: \(part1)"

    Task.onFail task \_ -> Stdout.line "Oops something went wrong."

scoreItems : List (Int *) -> Int Signed32
scoreItems = \items ->
    items
    |> List.map Num.toI32
    |> List.map (\i -> if i >= 97 then i - 96 else i - 38)
    |> List.sum

# --- TESTS - scoreItems
expect
    score = scoreItems (Str.toUtf8 "a")

    score == 1

expect
    score = scoreItems (Str.toUtf8 "z")

    score == 26

expect
    score = scoreItems (Str.toUtf8 "A")

    score == 27

expect
    score = scoreItems (Str.toUtf8 "Z")

    score == 52

expect
    score = scoreItems (Str.toUtf8 "abc")

    score == 6
