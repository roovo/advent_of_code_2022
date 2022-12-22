app "aoc_day3"
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
    filePath = Path.fromStr "day03_input.txt"

    task =
        contents <- File.readUtf8 filePath |> Task.await

        rucksacks =
            contents
            |> Str.split "\n"
            |> List.map packRucksack

        part1 =
            rucksacks
            |> List.map findDuplicates
            |> List.map scoreItems
            |> List.sum
            |> Num.toStr

        Stdout.line "part1: \(part1)"

    Task.onFail task \_ -> Stdout.line "Oops something went wrong."

# ## PRIVATE
Rucksack elem : { compartment1 : List elem, compartment2 : List elem }

packRucksack : Str -> Rucksack U8
packRucksack = \input ->
    input
    |> Str.toUtf8
    |> List.split (Num.divCeil (Str.countUtf8Bytes input) 2)
    |> \{ before, others } -> { compartment1: before, compartment2: others }

findDuplicates : Rucksack U8 -> List U8
findDuplicates = \rucksack ->
    rucksack
    |> \{ compartment1, compartment2 } -> Set.intersection (Set.fromList compartment1) (Set.fromList compartment2)
    |> Set.toList

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

# --- TESTS - findDuplicates
expect
    duplicates = findDuplicates { compartment1: [], compartment2: [] }

    duplicates == []

expect
    duplicates = findDuplicates { compartment1: Str.toUtf8 "a", compartment2: Str.toUtf8 "b" }

    duplicates == []

expect
    duplicates = findDuplicates { compartment1: Str.toUtf8 "a", compartment2: Str.toUtf8 "a" }

    duplicates == Str.toUtf8 "a"

expect
    duplicates = findDuplicates { compartment1: Str.toUtf8 "abcde", compartment2: Str.toUtf8 "xyaz" }

    duplicates == Str.toUtf8 "a"

expect
    duplicates = findDuplicates { compartment1: Str.toUtf8 "abcde", compartment2: Str.toUtf8 "xyazb" }

    duplicates == Str.toUtf8 "ab"

# --- TESTS - packRucksack
expect
    packedRucksack = packRucksack ""

    packedRucksack == { compartment1: [], compartment2: [] }

expect
    packedRucksack = packRucksack "a"

    packedRucksack == { compartment1: Str.toUtf8 "a", compartment2: [] }

expect
    packedRucksack = packRucksack "ab"

    packedRucksack == { compartment1: Str.toUtf8 "a", compartment2: Str.toUtf8 "b" }

expect
    packedRucksack = packRucksack "cdab"

    packedRucksack == { compartment1: Str.toUtf8 "cd", compartment2: Str.toUtf8 "ab" }

expect
    packedRucksack = packRucksack "cdzab"

    packedRucksack == { compartment1: Str.toUtf8 "cdz", compartment2: Str.toUtf8 "ab" }
