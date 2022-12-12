app "hello"
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
        Stdout.line contents

    Task.attempt task \result ->
        when result is
            Ok {} -> Stdout.line "success!"
            Err _ -> Stdout.line "failure"
