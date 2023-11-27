app "aoc"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.6.0/QOQW08n38nHHrVVkJNiPIjzjvbR3iMjXeFY5w1aT46w.tar.br" }
    imports [pf.Task.{ Task, await }, pf.Arg, pf.Stdout, Utils.Args]
    provides [main] to pf

boolToStr = \b -> if b then "true" else "false"

main : Task {} I32
main =
    { day, part, useExample } <- await Utils.Args.getArgs

    Stdout.line "day: \(Num.toStr day)\tpart: \(Num.toStr part)\tuseExample: \(boolToStr useExample)"
