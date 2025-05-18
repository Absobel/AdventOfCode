read_file(Filename) :-
    open(Filename, read, Stream),
    read_lines(Stream, [0, 999]),
    close(Stream).

read_lines(Stream, CurrentState) :-
    read_line_to_string(Stream, Line),
    (   Line == end_of_file
    ->  nth0(0, CurrentState, Res), writeln(Res)
    ;   process_line(Line, CurrentState, NewState),
        read_lines(Stream, NewState)
    ).

:- initialization(main).

main :-
    read_file('input/input.txt').

/**************/

is_increase(D1, D2) :- D1 < D2.

process_line(Line, CurrentState, NewState) :-
    number_string(NewD, Line),
    nth0(1, CurrentState, CurrD),
    nth0(0, CurrentState, C),
    (   is_increase(CurrD, NewD)
    ->  NewC is C + 1
    ;   NewC is C
    ),
    NewState = [NewC, NewD].