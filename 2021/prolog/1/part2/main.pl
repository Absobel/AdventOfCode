read_file(Filename) :-
    open(Filename, read, Stream),
    read_lines(Stream, [0, []]),
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

sum([], 0).
sum([H|T], Sum) :-
    sum(T, Sum1),
    Sum is H + Sum1.

slide([_, B, C], D, NewWindow) :- NewWindow = [B, C, D].

process_line(Line, CurrentState, NewState) :-
    number_string(NewD, Line),
    [C, CurrWindow] = CurrentState,

    ( length(CurrWindow, 3) -> 
        sum(CurrWindow, CurrSum),
        slide(CurrWindow, NewD, NewWindow),
        sum(NewWindow, NewSum),
        (   is_increase(CurrSum, NewSum)
        ->  NewC is C + 1
        ;   NewC is C
        )
    ;   append(CurrWindow, [NewD], NewWindow),
    NewC is C
    ),
    
    NewState = [NewC, NewWindow].