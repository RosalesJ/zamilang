let test_file = "/Users/coby/projects/zamilang/queens.zam"

let read_whole_file filename =
    let ch = open_in filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s

let get_queens = read_whole_file test_file
