/*set_prolog_flag(double_quotes, string).*/

:- initialization main.

main :-
    read(user_input, T),
    typeProg(T, R),
    print(R).

addEnv(G, [], G).
addEnv(G, L, X) :- append(G, L, X), !.

argsType([], []).
argsType([(_, AT)|T], X) :- argsType(T, X1), append(X1, [AT], X), !.

typeEnv([(ID, X)|_], ID, X).
typeEnv([_|T], ID, X) :- typeEnv(T, ID, X).

typeExpr(_, true, bool).
typeExpr(_, false, bool).

typeExpr(_, N, int) :- integer(N).

typeExpr(G, and(O1, O2), bool) :- typeExpr(G, O1, bool), typeExpr(G, O2, bool). 
typeExpr(G, or(O1, O2), bool) :- typeExpr(G, O1, bool), typeExpr(G, O2, bool).
typeExpr(G, not(O), bool) :- typeExpr(G, O, bool).
typeExpr(G, eq(O1, O2), bool) :- typeExpr(G, O1, int), typeExpr(G, O2, int).
typeExpr(G, lt(O1, O2), bool) :- typeExpr(G, O1, int), typeExpr(G, O2, int).
typeExpr(G, add(O1, O2), int) :- typeExpr(G, O1, int), typeExpr(G, O2, int).
typeExpr(G, sub(O1, O2), int) :- typeExpr(G, O1, int), typeExpr(G, O2, int).
typeExpr(G, mul(O1, O2), int) :- typeExpr(G, O1, int), typeExpr(G, O2, int).
typeExpr(G, div(O1, O2), int) :- typeExpr(G, O1, int), typeExpr(G, O2, int).

typeExpr(G, ID, X) :- string(ID), typeEnv(G, ID, X).



typeExpr(G, if(Cond, Consequence, Alternative), X) :- typeExpr(G, Cond, bool),
						      typeExpr(G, Consequence, X),
						      typeExpr(G, Alternative, X).
typeExpr(_, [], []).
typeExpr(G, [H|T], [X1|X2]) :- typeExpr(G, H, X1), typeExpr(G, T, X2), !.
    
typeExpr(G, app(ID, ARGS), X) :- typeExpr(G, ID, arrow(AT, X)), typeExpr(G, ARGS, AT), !.
typeExpr(G, abs(ARGS, E), arrow(AT, X)) :- append(G, ARGS, G1), typeExpr(G1, E, X), argsType(ARGS, AT).   



typeDec(G, const(ID, T, E), X) :- typeExpr(G, E, T), append(G, [(ID, T)], X), !.
typeDec(G, fun(ID, arrow(FT, T), PARAMS, Body), X) :- addEnv(G, PARAMS, G1),
						     typeExpr(G1, Body, T),
						     addEnv(G, [(ID, arrow(FT, T))], X), !.
typeDec(G, funRec(ID, arrow(FT, T), PARAMS, Body), X) :- addEnv(G, PARAMS, G1),
						     append(G1, [(ID, arrow(FT, T))], X),
						     typeExpr(X, Body, T), !.
typeStat(G, echo(E), void).

typeCmds(G, [], void).
typeCmds(G, [H|T], void) :- typeDec(G, H, G1), typeCmds(G1, T, void).
typeCmds(G, [H|T], void) :- typeStat(G, H, void), typeCmds(G, T, void).

typeProg(T, ok) :- typeCmds([], T, void).
typeProg(T, ko).
