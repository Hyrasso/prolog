
% https://www.ic.unicamp.br/~meidanis/courses/mc336/2009s2/prolog/problemas/

% p01
mylast(K, [K]).
mylast(K, [_|T]) :- mylast(K, T).

% p02
myplast(K, [K,_]).
myplast(K, [_|T]) :- myplast(K, T).

% p03
element_at([E|_], 0, E).
element_at([_|T], I, E) :- NI is I - 1, element_at(T, NI, E).

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

% p54
is_tree(nil).
is_tree(t(_, X, Y)) :- is_tree(X), is_tree(Y).

% p55
cbal_tree(0, nil).
cbal_tree(1, t(_, nil, nil)).
cbal_tree(C, T) :- C > 1, PC is C - 1, bal_distrib(PC, LC, RC), cbal_tree(LC, TL), cbal_tree(RC, TR), T = t(_, TL, TR).

bal_distrib(T, R, R) :- divmod(T, 2, R, 0).
bal_distrib(T, L, R) :- divmod(T, 2, R, 1), L is R + 1.
bal_distrib(T, L, R) :- divmod(T, 2, L, 1), R is L + 1.

% p56
symmetric_tree(t(_, L, R)) :- symmetric_tree(L, R).
symmetric_tree(nil, nil).
% symmetric_tree(t(_, nil, nil), t(_, nil, nil)).
% symmetric_tree(t(_, nil, LR), t(_, RL, nil)) :- symmetric_tree(LR, RL).
% symmetric_tree(t(_, LL, nil), t(_, nil, RR)) :- symmetric_tree(LL, RR).
symmetric_tree(t(_, LL, LR), t(_, RL, RR)) :- symmetric_tree(LL, RR), symmetric_tree(LR, RL).

% p57
construct_binary_tree(L, T) :- sort(L, SL), inner_construct_binary_tree(SL, T).

inner_construct_binary_tree([], nil).
inner_construct_binary_tree(L, t(E, TL, TR)) :-
    median(L, E, LL, RR),
    inner_construct_binary_tree(LL, TL),
    inner_construct_binary_tree(RR, TR).

median(Xs, E, LL, RR) :- len(Xs, L), divmod(L, 2, K, 1), split_at(Xs, K, E, LL, RR).
median(Xs, E, LL, RR) :- len(Xs, L), divmod(L, 2, K, 0), PK is K - 1, between(PK, K, MK), split_at(Xs, MK, E, LL, RR).

split_at([M|T], 0, M, [], T).
split_at([X|T], MI, M, L, R) :- NMI is MI - 1, split_at(T, NMI, M, PL, R), [X|PL] = L.

% p61
count_leaves(nil, 0).
count_leaves(t(_, nil, nil), 1) :- !.
count_leaves(t(_, LT, RT), T) :- count_leaves(LT, CL), count_leaves(RT, CR), T is CL + CR.

collect_leaves(nil, []).
collect_leaves(t(X, nil, nil), [X]) :- !.
collect_leaves(t(_, L, R), CL) :- collect_leaves(L, LL), collect_leaves(R, RL), extend(LL, RL, CL).
