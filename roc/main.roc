app "aoc"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.6.0/QOQW08n38nHHrVVkJNiPIjzjvbR3iMjXeFY5w1aT46w.tar.br" }
    imports [pf.Task.{ Task }, pf.Stdout, pf.Arg, Utils.Args]
    provides [main] to pf

main : Task {} I32
main =
    # _ <- Task.await Stdout.line "test"
    Utils.Args.getArgs
