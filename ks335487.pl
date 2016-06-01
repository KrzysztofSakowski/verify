% Krzysztof Sakowski
:- ensure_loaded([library(lists)]).
% TODO yes if error?
verify(N, FileName) :-
    validateProcAmt(N),
    readProgram(FileName, Data),
    initState(Data, N, InitState),
    getInstrList(Data, Program),
    write([InitState]),
    bfs(Program, graph([InitState], [InitState], [], N)).  % TODO init ancestor

    % TODO change state represenation to term

validateProcAmt(N) :-
    ( N =< 0 ->
        format('Error: parametr 0 powinien byc liczba > 0~n'),
        !,
        fail
    ;
        true
    ).

readProgram(FileName, Data) :-
    set_prolog_flag(fileerrors, off),
    see(FileName),
    !,
    % read(vars(Vars)),
    % read(arrays(Arrs)),
    % read(program(Instrs)),
    read(Vars),
    read(Arrs),
    read(Instrs),
    seen,
    write(Vars),nl,
    Data = [Vars, Arrs, Instrs].

 readProgram(FileName, _) :-
    format('Error: brak pliku o nazwie - ~p~n', [FileName]),
    !,
    fail.

getInstrList(Program, Ins) :-
    nth1(3, Program, program(Ins)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  0 : ProcAmt-1  TODO remove
% bfs(+Program, +graph(Nodes, Visited, Ancestors, ProcAmt))
bfs(Program, Graph) :-
    arg(1, Graph, Nodes),
    arg(4, Graph, ProcAmt),
    arg(2, Graph, Visited),
    arg(3, Graph, Ancestors),
    head(Nodes, Node),

    length(Visited, Size), % TODO remove
    write('bfs: '), write(Size), write(' '), nl,

    ( isStateUnsafe(Program, Node, ProcAmt, 0) ->
        write('Program jest niepoprawny: stan nr '),
        write('42'), % TODO
        write(' nie jest bezpieczny.'), nl,
        % TODO print report, Ancestors
        getAncestors(Node, Ancestors, [Node], Path),
        writeWithNl(Path),
        !
    ;
        iterateProc(0, Program, Graph, Graph2),
        bfs(Program, Graph2)
    ).

bfs(_, graph([], _, _, _)) :-
    write('Program jest poprawny (bezpieczny).'), nl. % TODO cut

iterateProc(ProcId, Program, graph(Nodes, Visited, Ancestors, ProcAmt), Graph3) :-
    head(Nodes, Node),
    step(Program, Node, ProcId, Node2),
    ( member(Node2, Visited)  ->
        Nodes2 = Nodes,
        Visited2 = Visited,
        Ancestors2 = Ancestors
    ;
        append(Nodes, [Node2], Nodes2),
        Visited2 = [Node2|Visited],
        Ancestors2 = [(Node2, (Node, ProcId))|Ancestors]
    ),

    ( ProcAmt is ProcId+1 ->
        tail(Nodes2, Nodes3),
        Graph3 = graph(Nodes3, Visited2, Ancestors2, ProcAmt)
    ;
        ProcId2 is ProcId+1,
        Graph2 = graph(Nodes2, Visited2, Ancestors2, ProcAmt),
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

writeWithNl([H|T]) :- % TODO remove
    write(H), nl,
    writeWithNl(T).

writeWithNl([]).

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
    executeInstr(InState, PrId, Instr, OutState).

currentInstr(Program, InState, PrId, Instr) :-
    nth1(3, InState, OrderList),
    nth0(PrId, OrderList, InstrNum),
    nth1(InstrNum, Program, Instr).

executeInstr([Vars, Arrs, Orders], PrId, condGoto(BoolExp, ValExp), OutState) :-
    ( evaluateBoolExp([Vars, Arrs, Orders], PrId, BoolExp) ->
        evaluateArthmeticExp([Vars, Arrs, Orders], PrId, ValExp, Val),
        replace0(Orders, PrId, Val, Orders2),
        OutState = [Vars, Arrs, Orders2]    ;
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
    replace0(T, I2, X, T2).
