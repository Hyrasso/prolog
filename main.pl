
% https://www.ic.unicamp.br/~meidanis/courses/mc336/2009s2/prolog/problemas/

% p01
mylast(K, [K]).
mylast(K, [_|T]) :- mylast(K, T).

% p02
myplast(K, [K,_]).
myplast(K, [_|T]) :- myplast(K, T).

% p03
element_at(X, T, I) :- elem_at(X, T, I, 1).

elem_at(X, [X|_], I, C) :- I == C.
elem_at(X, [_|T], I, C) :- C > 0, N is C + 1, elem_at(X, T, I, N).

% p04
len(X, T) :- list_len(X, T, 0).
list_len(X, [], C) :- X = C.
list_len(X, [_|T], C) :- N is C + 1, list_len(X, T, N).

% p05
append([], X, [X]).
append([H|T], X, [H|R]) :- append(T, X, R).

prepend([], X, [X]).
prepend(R, X, [X|R]).

% there is a simpler way that do not use prepend
rev(X, Y) :- list_rev(X, [], Y).
list_rev([], M, M).
list_rev([H|T], M, Y) :- prepend(M, H, NM), list_rev(T, NM, Y).

% p06
palin(X) :- rev(X, X).

% p07
