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

        Stdout.line "part1: \(contents)"

   Task.onFail task \_ -> Stdout.line "Oops something went wrong."


### PRIVATE


### TESTS
