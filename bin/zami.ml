open Lib

let () =
  File.read_whole_file test_file
  |> Parse.lex
  |> ignore
