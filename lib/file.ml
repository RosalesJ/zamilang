open Core

let test_file = "/Users/coby/projects/zamilang/queens.zam"

let read_whole_file = In_channel.read_all

let get_queens = read_whole_file test_file
