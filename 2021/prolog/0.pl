read_file(Filename) :-
    open(Filename, read, Stream),
    read_lines(Stream, DefaultState),
    close(Stream).

read_lines(Stream, CurrentState) :-
    read_line_to_string(Stream, Line),
    (   Line == end_of_file
    ->  res(CurrentState, Res), writeln(Res)
    ;   process_line(Line, CurrentState, NewState),
        read_lines(Stream, NewState)
    ).

:- initialization(main).

main :-
    read_file('input/test.txt').

/**************/

DefaultState = .

res(CurrentState, Res) :- . 

process_line(Line, CurrentState, NewState) :-
    writeln(Line).