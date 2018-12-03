:-module day1a.
:-interface.
:-import_module io.

:-pred main(io::di, io::uo) is det.

:-implementation.
:-import_module bool, int, list, set, string.

:-func single_pass(list.list(int)) = int.
single_pass(Offsets) = list.foldl(int.plus, Offsets, 0).

main -->
  io.read_file_as_string(Res),
  ( { Res = ok(String) },
    { Lines = string.split_at_string("\n", string.strip(String)) },
    { Stripped = list.map(string.strip, Lines) },
    { Offsets = list.map(string.det_to_int, Stripped) },
    %{ Result = list.foldl(int.plus, Offsets, 0) },
    { Result = single_pass(Offsets) },
    { string.int_to_string(Result, ResultString) },
    print(ResultString),
    nl
    ;
    { Res = error(_ , ErrorCode) },
    { error_message(ErrorCode, ErrorMessage) },
    stderr_stream(StdErr),
    print(StdErr, "day1a: error reading input: "),
    print(StdErr, ErrorMessage),
    nl(StdErr)
  ).
