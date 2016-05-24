% Krzysztof Sakowski
:- ensure_loaded([library(lists), library(file_systems)]). % TODO file_systems lib?

verify(N, FileName) :-
    validateProcAmt(N),
    readProgram(FileName, Data),
    initState(Data, N, InitState),
    getInstrList(Data, Program),

    xStep(Program, InitState, 10).

    % step(Program, InitState, 0, OutState),
    % write(OutState), nl,
    % step(Program, OutState, 0, OutState2),
    % write(OutState2).

xStep(_, InState, 0) :- % TODO remove
    write(InState), nl.

xStep(Program, InState, Amt) :-
    write(InState), nl,
    step(Program, InState, 0, OutState),
    Amt2 is Amt-1,
    xStep(Program, OutState, Amt2).

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

getInstrList(Program, Ins) :-
    nth1(3, Program, program(Ins)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TODO opisac
% [(zmienna, wartość)]
% [(tablica, [lista_wartosci])]
% [kolejcna instrukcja dla proc]

% initState(+Program, +N, -StanPoczątkowy)
initState(Program, N, InitState) :-
    initVariablesWithZeros(Program, InitVars),
    initArrsWithZeros(Program, N, InitArrs),
    generateListWith(N, 1, InitInstr),
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% step(+Program, +StanWe, ?PrId, -StanWy)
step(Program, InState, PrId, OutState) :-
    currentInstr(Program, InState, PrId, Instr),
    write(Instr), nl,
    executeInstr(InState, PrId, Instr, OutState).

currentInstr(Program, InState, PrId, Instr) :-
    nth1(3, InState, OrderList),
    nth0(PrId, OrderList, InstrNum),
    nth1(InstrNum, Program, Instr).

% TODO opisac
% [(zmienna, wartość)]
% [(tablica, [lista_wartosci])]
% [kolejcna instrukcja dla proc]

% executeInstr(Program, InState, PrId, condGoto(BoolExp, Val), OutState) :-
% incementOrder(Orders, PrId, Orders2),

executeInstr([Vars, Arrs, Orders], PrId, goto(InstrNum), OutState) :-
    replace0(Orders, PrId, InstrNum, Orders2),
    OutState = [Vars, Arrs, Orders2].

executeInstr([Vars, Arrs, Orders], PrId, sekcja, OutState) :-
    incementOrder(Orders, PrId, Orders2),
    OutState = [Vars, Arrs, Orders2].

executeInstr([Vars, Arrs, Orders], PrId, assign(Ident, Exp), OutState) :-
    evaluateArthmeticExp([Vars, Arrs, Orders], PrId, Exp, Val),
    setVar(Vars, Ident, Val, Vars2),
    incementOrder(Orders, PrId, Orders2),
    OutState = [Vars2, Arrs, Orders2].

executeInstr([Vars, Arrs, Orders], PrId, assign(arr(Ident, IndExp), ValExp), OutState) :-
    evaluateArthmeticExp([Vars, Arrs, Orders], PrId, ValExp, Val),
    evaluateArthmeticExp([Vars, Arrs, Orders], PrId, IndExp, Ind),
    setArrAtInd(Arrs, Ident, Ind, Val, Arrs2),
    incementOrder(Orders, PrId, Orders2),
    OutState = [Vars, Arrs2, Orders2].

incementOrder(Orders, PrId, Orders2) :-
    nth0(PrId, Orders, InstrNum),
    InstrNum2 is InstrNum+1,
    replace0(Orders, PrId, InstrNum2, Orders2).

% TODO red cut? add cuts?
setArrAtInd([(Ident, Vals)|T], Ident, Ind, Val, [(Ident, Vals2)|T]) :-
    replace0(Vals, Ind, Val, Vals2).

setArrAtInd([_|T], Ident, Ind, Val, [_|T2]) :-
    setArrAtInd(T, Ident, Ind, Val, T2).

setVar([(Ident, _)|T], Ident, Val, [(Ident, Val)|T]).

setVar([_|T], Ident, Val, [_|T2]) :-
    setVar(T, Ident, Val, T2).


evaluateArthmeticExp(State, PrId, (LOp + ROp), Val) :-
    evalutateSimpleExp(State, PrId, LOp, LVal),
    evalutateSimpleExp(State, PrId, ROp, RVal),
    Val is LVal + RVal.

evaluateArthmeticExp(State, PrId, (LOp - ROp), Val) :-
    evalutateSimpleExp(State, PrId, LOp, LVal),
    evalutateSimpleExp(State, PrId, ROp, RVal),
    Val is LVal - RVal.

evaluateArthmeticExp(State, PrId, (LOp * ROp), Val) :-
    evalutateSimpleExp(State, PrId, LOp, LVal),
    evalutateSimpleExp(State, PrId, ROp, RVal),
    Val is LVal * RVal.

evaluateArthmeticExp(State, PrId, (LOp / ROp), Val) :-
    evalutateSimpleExp(State, PrId, LOp, LVal),
    evalutateSimpleExp(State, PrId, ROp, RVal),
    Val is LVal / RVal.

evaluateArthmeticExp(State, PrId, Exp, Val) :-
    evalutateSimpleExp(State, PrId, Exp, Val).

evalutateSimpleExp(_, _, Val, Val) :-
    number(Val).

evalutateSimpleExp([Vars, Arrs, Orders], PrId, arr(Ident, Exp), Val) :-
    evaluateArthmeticExp([Vars, Arrs, Orders], PrId, Exp, Ind),
    getArrAtInd(Arrs, Ident, Ind, Val).

evalutateSimpleExp([Vars, _, _], PrId, Ident, Val) :-
    ( Ident = pid ->
        Val is PrId
    ;
        getVar(Vars, Ident, Val)
    ).

getVar([(Ident2, Val2)|T], Ident, Val) :-
    ( Ident2 = Ident ->
        Val is Val2
    ;
        getVar(T, Ident, Val)
    ).

getArrAtInd([(Ident2, Vals)|T], Ident, Ind, Val) :-
    ( Ident2 = Ident ->
        nth0(Ind, Vals, Val)
    ;
        getArrAtInd(T, Ident, Ind, Val)
    ).

% replace0(+List, +Ind, +Elt, -List2)
replace0([_|T], 0, X, [X|T]).
replace0([H|T], I, X, [H|T2]):-
    I > 0,
    I2 is I-1,
    replace(T, I2, X, T2).
