interface Utils.Args
    exposes [getArgs]
    imports [
        pf.Task.{ Task },
        pf.Arg,
    ]

AocArgs : {
    day : U8,
    part : U8,
    useExample : Bool,
}

listToStr : List Str -> Str
listToStr = \lst ->
    joined = Str.joinWith lst ", "
    "[\(joined)]"

toU8Unsafe : Str -> U8
toU8Unsafe = \s ->
    when Str.toU8 s is
        Ok n -> n
        Err _ -> crash "invalid u8: \(s)"

boolToStr : Str -> Bool
boolToStr = \s ->
    when s is
        "1" -> Bool.true
        "true" -> Bool.true
        _ -> Bool.false

getArgs : Task AocArgs *
getArgs =
    args <- Task.await Arg.list
    # drop first arg since its the binary/program name
    droppedArgs = List.dropFirst args 1
    when droppedArgs is
        [day, part, useExample] -> Task.ok { day: toU8Unsafe day, part: toU8Unsafe part, useExample: boolToStr useExample }
        _ ->
            crash "bad arguments: \(listToStr droppedArgs)"
