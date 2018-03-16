/*set_prolog_flag(double_quotes, string).*/

addEnv(G, [], G).
addEnv(G, L, X) :- append(X, G, L), !.

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
typeExpr(G, abs(ARGS, E), X) :- addEnv(G, ARGS, G1), typeExpr(G1, E, X).   



typeDec(G, const(ID, T, E), X) :- typeExpr(G, E, T), addEnv(G, [(ID, T)], X), !.
typeDec(G, fun(ID, PARAMS, arrow(FT, T), Body), X) :- addEnv(G, PARAMS, G1), typeExpr(G1, Body, T), addEnv(G, [(ID, arrow(FT, T))], X), !.
typeDec(G, funRec(ID, PARAMS, T, Body), X) :- addEnv(G, [(ID, T)], X), typeExpr(G, Body, T), !.
typeExpr(G, echo(E), void).