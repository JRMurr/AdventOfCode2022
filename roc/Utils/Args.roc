interface Utils.Args
    exposes [getArgs]
    imports [
        pf.Task.{ Task },
        pf.Stdout,
        pf.Arg
    ]

parser = 
    Arg.str {name: "day"}
    |>  Arg.program { name: "aoc", help: "run aoc" }


getArgs : Task {} I32
getArgs = 
    args <- Arg.list |> Task.await
    # when Arg.parseFormatted parser args is
    #     Ok res ->
    #         Stdout.line res

    #     Err helpMenu ->
    #         {} <- Stdout.line helpMenu |> Task.await
            
    #         Task.err 1 # 1 is an exit code to indicate failure
