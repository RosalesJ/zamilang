open Core
open Lib

let () =
  File.read_whole_file File.test_file
  |> Parse.lex
  |> ignore
