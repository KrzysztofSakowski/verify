% Krzysztof Sakowski
:- ensure_loaded([library(lists), library(file_systems)]). % TODO file_systems lib?

verify(N, FileName) :-
    validateProcAmt(N),
    readProgram(FileName, Data),
    write(Data), nl,
    initState(Data, N, Out).

validateProcAmt(N) :-
    ( N =< 0 ->
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


% [(zmienna, wartość)]
% [(tablica, [lista_wartosci])]
% [kolejcna instrukcja dla proc]

% getVarList(Program, Vars) :-


getInstrList(Program, Ins) :-
    nth1(3, Program, program(Ins)).

% initState(+Program, +N, -StanPoczątkowy)
initState(Program, N, InitState) :-
    initVariablesWithZeros(Program, InitVars),
    initArrsWithZeros(Program, N, InitArrs),
    generateListWithZeros(N, 1, InitInstr),
    write([InitVars, InitArrs, InitInstr]), nl,
    InitState = [InitVars, InitArrs, InitInstr].

initVariablesWithZeros(Program, InitVars) :-
    nth1(1, Program, vars(Vars)),
    initVariablesWithZeros2(Vars, InitVars).

initVariablesWithZeros2([], []).
initVariablesWithZeros2([X|Xs], [Y|Ys]) :-
    Y = (X, 0),
    initVariablesWithZeros2(Xs, Ys).

initArrsWithZeros(Program, N, InitArrs) :-
    nth1(2, Program, arrays(Arrs)),
    initArrsWithZeros2(Arrs, N, InitArrs).

initArrsWithZeros2([], _, []).
initArrsWithZeros2([X|Xs], N, [Y|Ys]) :-
    generateListWith(N, 0, List),
    Y = (X, List),
    initArrsWithZeros2(Xs, N, Ys).

% generateListWith(+Size, +Value, -List)
generateListWith(0, _, []).
generateListWith(N, Val, [X|Xs]) :-
    X is Val,
    M is N-1,
    generateListWith(M, Val, Xs).






% step(+Program, +StanWe, ?PrId, -StanWy)
