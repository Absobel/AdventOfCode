read_file(Filename) :-
    default_state(DefaultState),
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
    read_file('input/input.txt').

/**************/

default_state([0, 0]).

res(CurrentState, Res) :- 
    [X, Y] = CurrentState,
    Res is X * Y.

process_line(Line, OldState, NewState) :-
    split_string(Line, " ", "", [Instruction, AmountStr]),
    number_string(Amount, AmountStr),
    [X, Y] = OldState,
    (   Instruction == "forward"
    ->  NewX is X + Amount,
        NewY is Y
    ;   Instruction == "down"
    ->  NewX is X,
        NewY is Y + Amount
    ;   Instruction == "up"
    ->  NewX is X,
        NewY is Y - Amount
    ),
    NewState = [NewX, NewY].