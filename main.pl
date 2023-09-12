
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
extend(X, [], X).
extend(E, [H|T], X) :- append(E, H, NE), extend(NE, T, X).

flatten([], []).
flatten([H|T], X) :- is_list(H), flatten(H, FH), flatten(T, FT), extend(FH, FT, X).
flatten([H|T], [H|R]) :- flatten(T, R).

% p08
compress([], []).
compress([A], [A]).
compress([A,A|T], [A|R]) :- compress(T, R).
compress([A,B|T], [A|R]) :- A \= B, compress([B|T], R).

% p09
pack([], []).
pack([E], [[E]]).
pack([A,A|T], [[A|R]|DR]) :- pack([A|T], [R|DR]).
pack([A,B|T], [[A],[B|R]|DR]) :- A \= B, pack([B|T], [[B|R]|DR]).

% p10
% ok to check but causes error when trying to generate the encoded list
rle([], []).
rle([A], [[1, A]]).
rle([A,A|T], [[N, A]|R]) :- rle([A|T], [[NN, A]|R]), N is NN + 1.
rle([A,B|T], [[1, A],[M, B]|R]) :- A \= B, rle([B|T], [[M, B]|R]).



% p11 / p13 actually
mrle([], []).
mrle([A], [A]).
mrle([A, A], [[2, A]]).
mrle([A,A,B|T], [[2, A]|R]) :- A \= B, mrle([B|T], R).
mrle([A,A|T], [[N, A]|R]) :- mrle([A|T], [[NN, A]|R]), N is NN + 1.
mrle([A,B|T], [A|R]) :- A \= B, mrle([B|T], R).

% p12
decode_rle([], []).
decode_rle([X], [X]) :- \+ is_list(X).
decode_rle([X|R], [X,Y|T]) :- decode_rle(R, [Y|T]), X \= Y, \+ is_list(X).
decode_rle([[1, X]|R], [X|T]) :- decode_rle(R, T).
decode_rle([[N, X]|R], [X|T]) :- N > 1, NN is N - 1, decode_rle([[NN, X]|R], T).

% p14
dup_list([], []).
dup_list([A|T], [A, A|R]) :- dup_list(T, R).

% p15
repeat(A, N, B) :- inner_repeat(A, N, N, B).

inner_repeat([], N, N, []).
inner_repeat([X|T], 1, N, [X|R]) :- inner_repeat(T, N, N, R).
inner_repeat([X|T], A, N, [X|R]) :- A > 1, A1 is A - 1, inner_repeat([X|T], A1, N, R).

% p16
drop_every_n(X, K, Y) :- inner_drop_very_n(X, 0, K, Y).

inner_drop_very_n([], _, _, []).
inner_drop_very_n([_|T], K, K, R) :- inner_drop_very_n(T, 0, K, R).
inner_drop_very_n([X|T], N, K, [X|R]) :- N < K, NN is N + 1, inner_drop_very_n(T, NN, K, R).

% p17
split(X, 0, [], X).
split([X|T], K, [X|R], L2) :- K > 0, KN is K - 1, split(T, KN, R, L2).