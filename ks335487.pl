% Krzysztof Sakowski
:- ensure_loaded([library(lists)]).

verify(ProcAmt, FileName) :-
    validateProcAmt(ProcAmt),
    readProgram(FileName, Vars, Arrs, Program),
    initState(Vars, Arrs, ProcAmt, InitState),

    FirstStateId is 1,

    Graph = graph([(FirstStateId, InitState)],
            [InitState], [], ProcAmt, FirstStateId),

    bfs(Program, Graph).

    % TODO init ancestor
    % TODO yes if error?
    % TODO change state represenation to term

validateProcAmt(N) :-
    ( N =< 0 ->
        format('Error: parametr 0 powinien byc liczba > 0~n'),
        !, fail
    ;
        true
    ).

readProgram(FileName, Vars, Arrs, Instrs) :-
    set_prolog_flag(fileerrors, off),
    see(FileName),
    !,
    read(vars(Vars)),
    read(arrays(Arrs)),
    read(program(Instrs)),
    seen.

 readProgram(FileName, _) :-
    format('Error: brak pliku o nazwie - ~p~n', [FileName]),
    !, fail. % TODO?

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Sekcja przechodząca po grafie stanów przy pomocy algorytmu BFS

% bfs(+Program, +graph(NodesToVisit, Visited, Ancestors, ProcAmt, CurStateId)) % TODO odpisac strukture
bfs(Program, Graph) :-
    arg(1, Graph, Nodes),
    arg(4, Graph, ProcAmt),
    arg(3, Graph, Ancestors),
    head(Nodes, (StateId, Node)),

    arg(2, Graph, Visited),
    length(Visited, Size), % TODO remove
    write('bfs: '), write(Size), write(' '), nl,

    ( isStateUnsafe(Program, Node, ProcAmt, 0) ->
        format("Program jest niepoprawny: stan nr ~p nie jest bezpieczny.~n",
                [StateId]),

                % Niepoprawny przeplot:
                % Proces 1: 1
                % Proces 0: 1
                % Procesy w sekcji: 1, 0.

        getAncestors(Node, Ancestors, [Node], Path),
        % writeWithNl(Path),
        !
    ;
        iterateProc(0, Program, Graph, Graph2),
        bfs(Program, Graph2)
    ).

bfs(_, graph([], _, _, _, _)) :-
    write('Program jest poprawny (bezpieczny).'), nl. % TODO cut?

iterateProc(ProcId, Program,
        graph(Nodes, Visited, Ancestors, ProcAmt, CurStateId),
        Graph3) :-

    head(Nodes, (_, Node)),
    step(Program, Node, ProcId, Node2),
    ( member(Node2, Visited)  ->
        Nodes2 = Nodes,
        Visited2 = Visited,
        Ancestors2 = Ancestors,
        NextStateId is CurStateId
    ;
        NextStateId is CurStateId+1,
        append(Nodes, [(NextStateId, Node2)], Nodes2),
        Visited2 = [Node2|Visited],
        Ancestors2 = [(Node2, (Node, ProcId))|Ancestors]
    ),

    ( ProcAmt is ProcId+1 ->
        tail(Nodes2, Nodes3),
        Graph3 = graph(Nodes3, Visited2, Ancestors2, ProcAmt, NextStateId)
    ;
        ProcId2 is ProcId+1,
        Graph2 = graph(Nodes2, Visited2, Ancestors2, ProcAmt, NextStateId),
        iterateProc(ProcId2, Program, Graph2, Graph3)
    ).

isStateUnsafe(Program, State, ProcAmt, PrId) :-
    PrId2 is PrId+1,
    PrId2 < ProcAmt,
    ( currentInstr(Program, State, PrId, sekcja) ->
        !,
        isStateUnsafe2(Program, State, ProcAmt, PrId2)
    ;
        isStateUnsafe(Program, State, ProcAmt, PrId2)
    ).

isStateUnsafe2(Program, State, ProcAmt, PrId) :-
    ( currentInstr(Program, State, PrId, sekcja) ->
        true
    ;
        PrId2 is PrId+1,
        PrId2 < ProcAmt,
        isStateUnsafe2(Program, State, ProcAmt, PrId2)
    ).

getAncestors(Node, Ancestors, Acc, Path) :-
    ( getAncestor(Ancestors, Node, (Ancestor, PrId)) ->
        Acc2 = [(Ancestor, PrId)|Acc],
        getAncestors(Ancestor, Ancestors, Acc2, Path)
    ;
        Path = Acc
    ).

getAncestor([(Ident2, Val2)|T], Ident, Val) :-
    ( Ident2 = Ident ->
        Val = Val2
    ;
        getAncestor(T, Ident, Val)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Sekcja generująca stan początkowy
%
% Stan reprenetowany jest przez trzy-elementową listę [Vars, Arrs, Orders]]
% Vars to lista par (NAZWA_ZMIENNEJ, WARTOŚĆ)
% Arrs to lista par postaci (NAZWA_TABLICY, LISTA_N_WARTOŚCI)
% Orders to lista długości liczby procesów zawierają nr kolejnej instrukcji
% dla każdego procesu

initState(Vars, Arrs, ProcAmt, InitState) :-
    initVariablesWithZeros(Vars, InitVars),
    initArrsWithZeros(Arrs, ProcAmt, InitArrs),
    generateListWith(ProcAmt, 1, InitInstr),
    InitState = [InitVars, InitArrs, InitInstr].

initVariablesWithZeros([], []).
initVariablesWithZeros([X|Xs], [Y|Ys]) :-
    Y = (X, 0),
    initVariablesWithZeros(Xs, Ys).

initArrsWithZeros([], _, []).
initArrsWithZeros([X|Xs], ProcAmt, [Y|Ys]) :-
    generateListWith(ProcAmt, 0, List),
    Y = (X, List),
    initArrsWithZeros(Xs, ProcAmt, Ys).

% generateListWith(+Size, +Value, -List)
generateListWith(0, _, []).
generateListWith(N, Val, [X|Xs]) :-
    X is Val,
    M is N-1,
    generateListWith(M, Val, Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Sekcja wykonująca instrukcje

% step(+Program, +StanWe, ?PrId, -StanWy)
step(Program, InState, PrId, OutState) :-
    currentInstr(Program, InState, PrId, Instr),
    executeInstr(InState, PrId, Instr, OutState).

currentInstr(Program, InState, PrId, Instr) :-
    nth1(3, InState, OrderList),
    nth0(PrId, OrderList, InstrNum),
    nth1(InstrNum, Program, Instr).

executeInstr([Vars, Arrs, Orders], PrId, condGoto(BoolExp, ValExp), OutState) :-
    ( evaluateBoolExp([Vars, Arrs, Orders], PrId, BoolExp) ->
        evaluateArthmeticExp([Vars, Arrs, Orders], PrId, ValExp, Val),
        replace0(Orders, PrId, Val, Orders2),
        OutState = [Vars, Arrs, Orders2]
    ;
        incementOrder(Orders, PrId, Orders2),
        OutState = [Vars, Arrs, Orders2]
    ).

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

setArrAtInd([(Ident, Vals)|T], Ident, Ind, Val, [(Ident, Vals2)|T]) :-
    replace0(Vals, Ind, Val, Vals2).

setArrAtInd([_|T], Ident, Ind, Val, [_|T2]) :-
    setArrAtInd(T, Ident, Ind, Val, T2).

setVar([(Ident, _)|T], Ident, Val, [(Ident, Val)|T]).

setVar([_|T], Ident, Val, [_|T2]) :-
    setVar(T, Ident, Val, T2).

% replace0(+List, +Ind, +Elt, -List2)
replace0([_|T], 0, X, [X|T]).
replace0([H|T], I, X, [H|T2]):-
    I > 0,
    I2 is I-1,
    replace0(T, I2, X, T2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Sekcja ewaluująca wyrażenia logiczne

evaluateBoolExp(State, PrId, (LOp < ROp)) :-
    evalutateSimpleExp(State, PrId, LOp, LVal),
    evalutateSimpleExp(State, PrId, ROp, RVal),
    LVal < RVal.

evaluateBoolExp(State, PrId, (LOp = ROp)) :-
    evalutateSimpleExp(State, PrId, LOp, LVal),
    evalutateSimpleExp(State, PrId, ROp, RVal),
    LVal is RVal.

% TODO
% evaluateBoolExp(State, PrId, (LOp (<>) ROp)) :-
%     evalutateSimpleExp(State, PrId, LOp, LVal),
%     evalutateSimpleExp(State, PrId, ROp, RVal),
%     LVal =\= RVal.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Sekcja ewaluująca wyrażenia

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
