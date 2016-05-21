% Krzysztof Sakowski
:- ensure_loaded([library(lists), library(file_systems)]). % TODO file_systems lib?

verify(ProcAmt, FileName) :-
    validateProcAmt(ProcAmt),
    readProgram(FileName, Data),
    write(Data), nl.

validateProcAmt(ProcAmt) :-
    ( ProcAmt =< 0 ->
        write('Error: parametr 0 powinien byc liczba > 0'), nl, !, fail
    ;
        true
    ).

readProgram(FileName, Data) :-
    ( file_exists(FileName) ->
        open(FileName, read, Str),
        parseProgram(Str, Data),
        close(Str)
    ;
        write('Error: brak pliku o nazwie - '), write(FileName), nl, !, fail
    ).

parseProgram(Str, Data) :-
    read(Str, Vars),
    read(Str, Arrs),
    read(Str, Ins),
    Data = [Vars, Arrs, Ins].
