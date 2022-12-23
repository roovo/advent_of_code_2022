interface Day03.Rucksack
    exposes [
        Rucksack,
        findDuplicates,
        pack,
    ]
    imports []

Rucksack elem : { compartment1 : List elem, compartment2 : List elem }

pack : Str -> Rucksack U8
pack = \input ->
    input
    |> Str.toUtf8
    |> List.split (Num.divCeil (Str.countUtf8Bytes input) 2)
    |> \{ before, others } -> { compartment1: before, compartment2: others }

findDuplicates : Rucksack U8 -> List U8
findDuplicates = \rucksack ->
    rucksack
    |> \{ compartment1, compartment2 } -> Set.intersection (Set.fromList compartment1) (Set.fromList compartment2)
    |> Set.toList

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

# --- TESTS - pack
expect
    packedRucksack = pack ""

    packedRucksack == { compartment1: [], compartment2: [] }

expect
    packedRucksack = pack "a"

    packedRucksack == { compartment1: Str.toUtf8 "a", compartment2: [] }

expect
    packedRucksack = pack "ab"

    packedRucksack == { compartment1: Str.toUtf8 "a", compartment2: Str.toUtf8 "b" }

expect
    packedRucksack = pack "cdab"

    packedRucksack == { compartment1: Str.toUtf8 "cd", compartment2: Str.toUtf8 "ab" }

expect
    packedRucksack = pack "cdzab"

    packedRucksack == { compartment1: Str.toUtf8 "cdz", compartment2: Str.toUtf8 "ab" }
