open Core
open! Lib

let () =
  File.read_whole_file File.queens
  |> Parse.parse
  |> ignore
