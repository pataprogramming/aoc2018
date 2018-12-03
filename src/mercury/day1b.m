:-module day1b.
:-interface.
:-import_module io.

:-pred main(io::di, io::uo) is det.

:-implementation.
:-import_module bool, int, list, set, string.

:-func rotate_list(list.list(T)) = list.list(T).
rotate_list(Input) = Output :-
  ( if Input = [ Head | Tail ]
    then Output = list.append( Tail, [ Head ] )
    else Output = []
  ).

:-func calibrate(list.list(int), set.set(int), int) = int.
calibrate(Offsets, Seen, Current) = Goal :-
  %trace [ io(!IO) ] ( io.write_int(Current, !IO), io.nl(!IO) ),
  % trace [ io(!IO) ]
  % ( TraceList = list.map(string.int_to_string, set.to_sorted_list(Seen)),
  %   io.write_string(string.join_list(",", TraceList), !IO),
  %   io.nl(!IO) ),
  is_member(Current, Seen, Reencountered),
  ( if Next = list.head(Offsets)
    then ( if Reencountered = bool.yes
           then Goal = Current
           else Goal = calibrate(rotate_list(Offsets),
                                 set.insert(Seen, Current),
                                 Current + Next))
    else Goal = Current
  ).

% Morbidly slow; three minutes running time
:-func first_repeated(list.list(int)) = int.
  first_repeated(Offsets) = Result :-
  Result = calibrate(Offsets, set.init, 0).

main -->
  io.read_file_as_string(Res),
  ( { Res = ok(String) },
    { Lines = string.split_at_string("\n", string.strip(String)) },
    { Stripped = list.map(string.strip, Lines) },
    { Offsets = list.map(string.det_to_int, Stripped) },
    %{ Result = list.foldl(int.plus, Offsets, 0) },
    { Result = first_repeated(Offsets) },
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
