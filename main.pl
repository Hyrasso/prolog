
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
len([], 0).
len([_|Xs], L) :- len(Xs, NL), L is NL + 1.

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

% p18
slice(X, F, T, L) :- split(X, T, M, _), split(M, F, _, L).

% p19
rotatel([], N, []) :- N > 0.
rotatel(X, N, Y) :- split(X, N, B, E), extend(E, B, Y).

% p20
remove_at([_|R], 1, R).
remove_at([H|T], K, [H|R]) :- K > 1, NK is K - 1, remove_at(T, NK, R).

% p21
insert_at(E, R, 1, [E|R]).
insert_at(E, [H|T], K, [H|R]) :- K > 1, NK is K - 1, insert_at(E, T, NK, R).

% p22
range(T, T, [T]).
range(F, T, [F|R]) :- T > F, NF is F + 1, range(NF, T, R).

% p23-25 random, later, maybe write the rng in prolog?

% p26
% for combination order do not matter
combinations(0, _, []).
combinations(K, [X|Xs], [X|Ys]) :- NK is K - 1, combinations(NK, Xs, Ys).
combinations(K, [_|Xs], Ys) :- K > 0, combinations(K, Xs, Ys).

% p28
lsort([], []).
% selection sort style for checking order
lsort(Xs, [M|T]) :- pop_shortest(Xs, [], NX, M), lsort(NX, T).

% move from 1 to 2, expect for shortest, which is put in M
pop_shortest([X|Xs], A, Y, M) :- append(A, X, NA), pop_shortest(Xs, NA, Y, M), len(X, XL), len(M, ML), XL > ML.
pop_shortest([M|Xs], A, Y, M) :- is_shortest(Xs, M), extend(A, Xs, Y).

is_shortest([], _).
is_shortest([X|H], M) :- len(X, XL), len(M, ML), XL >= ML, is_shortest(H, M).

% p31
is_prime(2).
is_prime(N) :- N > 2, PN is N - 1, inner_prime(N, PN).

inner_prime(_, 1).
inner_prime(N, N).
inner_prime(N, K) :- N > K, mod(N, K) > 0, PK is K - 1, inner_prime(N, PK).

% p32
gcd(A, B, D) :- T is min(A, B) - 1, inner_gcd(A, B, T, D).

% list all common divisors
inner_gcd(A, B, D, D) :- D > 0, mod(A, D) =:= 0, mod(B, D) =:= 0.
inner_gcd(A, B, K, D) :- K > 0, NK is K - 1, inner_gcd(A, B, NK, D).

% p33
coprime(A, B) :- gcd(A, B) =:= 1.

% p34
phi(M, Y) :- PM is M - 1, inner_phi(M, PM, Y), !.

inner_phi(_, D, 1) :- D =< 1.
inner_phi(M, D, Y) :- D > 1, coprime(M, D), PD is D - 1, inner_phi(M, PD, PY), Y is PY + 1.
inner_phi(M, D, Y) :- D > 1, gcd(M, D) =\= 1, PD is D - 1, inner_phi(M, PD, Y).

% p35
prime_factors(X, L) :- PX is X - 1, inner_prime_factors(X, PX, L).

inner_prime_factors(1, _, []).
inner_prime_factors(X, _, [X]) :- is_prime(X), !.
inner_prime_factors(X, C, [C|T]) :- X > 2, is_prime(C), divmod(X, C, D, 0), inner_prime_factors(D, C, T), !.
inner_prime_factors(X, C, T) :- X > 2, C > 2, PC is C - 1, inner_prime_factors(X, PC, T), !.

% p36
rle_prime_factors(X, L) :- prime_factors(X, T), rle(T, L).

% p37
phi2(N, Y) :- rle_prime_factors(N, L), inner_phi2(L, Y).

inner_phi2([], 1).
inner_phi2([[C, P]|T], Y) :- inner_phi2(T, PY), P1 is P - 1, C1 is C - 1, pow(P, C1, Temp), Y is PY * P1 * Temp.

% p49
iter_bin(1, [0]).
iter_bin(1, [1]).
iter_bin(N, C) :- N > 1, PN is N - 1, iter_bin(PN, PC), extend([0], PC, C).
iter_bin(N, C) :- N > 1, PN is N - 1, iter_bin(PN, PC), extend([1], PC, C).

gray(1, [0]).
gray(1, [1]).
gray(N, L) :- N > 1, PN is N - 1, gray(PN, PL), extend([0], PL, L).
gray(N, L) :- N > 1, PN is N - 1, graym(PN, PL), extend([1], PL, L).

graym(1, [1]).
graym(1, [0]).
graym(N, L) :- N > 1, PN is N - 1, gray(PN, PL), extend([1], PL, L).
graym(N, L) :- N > 1, PN is N - 1, graym(PN, PL), extend([0], PL, L).

% p50
% huffman_bits(CF, CM) :- huffman_tree(CF, HT), read_huffmane_tree(HT, CM).

% huffman_tree(CF, HT) :- sorted(CF, ICF), inner_huffman_tree(ICF, HT).
