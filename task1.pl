/*
edge(a, c, 8).
edge(a, b, 3).
edge(c, d, 12).
edge(b, d, 0).
edge(e, d, 9).

edge(a, b, 1).
edge(a, c, 9).
edge(a, d, 2).
edge(b, e, 0).
edge(d, c, 1).
edge(e, d, 1).
edge(c, e, 7).

w(cost, way).
*/

%test2
edge(a, b, 4).
edge(a, c, 1).
edge(a, d, 4).
edge(b, c, 3).
edge(b, e, 2).
edge(b, f, 7).
edge(c, d, 3).
edge(c, e, 1).
edge(d, f, 15).
edge(d, e, 2).

% (i, i, i), (i, i, o), (i, o, i); not_determined : (o, o, i)
append([], L, L).
append([X | R1], L2, [X |R3]) :- append(R1, L2, R3).

% (i, i), (i, o), (o, i)
reverse1([], []).
reverse1([X | R1], L) :- reverse1(R1, L1), append(L1, [X], L).

% (i, i, i), (i, i, o), (o, i, i);
delete_first(_, [], []).
delete_first(E, [E | R1], R1) :- !.
delete_first(E, [H | R1], [H | L1])  :- delete_first(E, R1, L1).

% (i, i, i), (i, i, o)
delete_all(_, [], []).
delete_all(E, [E | R1], L) :- !, delete_all(E, R1, L).
delete_all(E, [H | R1], [H | L1]) :- delete_all(E, R1, L1).

% (i, i, i), (o, i, i); not_determined : (i, i, o), (i, o, i)
delete_one(E, [E | R], R).
delete_one(E, [X | R1], [X | R2]) :- delete_one(E, R1, R2).

% (i, i), (i, o)
no_doubles([], []).
no_doubles([H | R], [H | L2]) :- delete_all(H, R, L1), no_doubles(L1, L2).

% (i, i), not_determined: (o, i), (i, o)
sublist(L1, L2) :- append(_, L12, L2), append(L1, _, L12).

% (i, i, i), (i, o, i), (o, i, i), not_determined: (o, o, i)
number(E, 1, [E | _]).
number(E, N, [_ | R]) :- number(E, N1, R), N is N1+1.

%(insertion sort) (i, i), (i, o)
sort1([], []).
sort1([X | R], L) :- sort1(R, R_Sort), insert(X, R_Sort, L), !.

insert(X, [], [X]).
insert(X, [H | R], [H | R1]) :- X>H, !,  insert(X, R, R1).
insert(X, R, [X | R]).

% (i, i), not_determined: (o, i), (i, o)
subset1([], _).
subset1([X | R], M) :- member(X, M), delete_all(X, M, M1), subset1(R, M1).

% (i, i, i), (i, i, o)
per([], []).
per([X | L], P) :- per(L, L1), delete_one(X, P, L1).

union(M1, M2, M) :- union1(M1, M2, MN), per(MN, M), !.

union1([], M, M).
union1([X | R], M1, M) :- member(X, M1), !, union1(R, M1, M).
union1([X | R], M1, [X | M]) :- union1(R, M1, M).

% (i, i), (i, o)
tree_depth(nil, 0).
tree_depth(tree(L, R, _), N) :- tree_depth(L, N1), tree_depth(R, N2), max(N1, N2, NM), N is NM+1, !.
max(X, Y, X) :- X>=Y.
max(X, Y, Y) :- X<Y.

% (i, i), not_determined: (o, i), (i, o)
sub_tree(T, T).
sub_tree(T, tree(L, R, _)) :- sub_tree(T, L); sub_tree(T, R).

% (i, i), (i, o)
flatten_tree(T, L) :- flat_tree(T, L, []).

flat_tree(nil, L, L).
flat_tree(tree(L, R, X), Res, Acc) :- flat_tree(R, R1, Acc), flat_tree(L, Res, [X | R1]).

% (i, i, i, i), (i, i, i, o), (o, i, i, i), (i, o, i, i), (i, i, o, i), (i, o, o, i)
substitute(nil, _, _, nil).
substitute(tree(L, R, V), V, T, tree(L1, R1, V)) :- substitute(L, V, T, L1), substitute(R, V, T, R1).
substitute(tree(L, R, X), V, T, tree(L1, R1, X)) :- substitute(L, V, T, L1), substitute(R, V, T, R1).

% (o, o, i), (o, i, i), (i, o, i), not_determined: (i, i, o), (i, o, o), (o, i, o), (o, o, o)
path(X, Y, L) :- path1([Y], X, L).

connect(X, Y, C) :- edge(X, Y, C); edge(Y, X, C).

path1([X | R], X, [X | R]).
path1(L1, Y, L) :- next_point(L1, L2), path1(L2, Y, L).

next_point([X | R], [Z, X | R]) :- connect(X, Z, _), not(member(Z, [X | R])).

% (o, o, i), (o, i, i), (i, o, i), not_determined: (i, i, o), (i, o, o), (o, i, o), (o, o, o)
min_path(A, Z, L) :- path_new(A, Z, L, MinC), not((path_new(A, Z, _, C), C<MinC)).

path_new(A, Z, L, C) :- path_new1(A, [Z], 0, L, C).

path_new1(A, [A | L1], C, [A | L1], C).
path_new1(A, [Y | L1], C1, L, C) :- connect(X, Y, CXY), not(member(X, L1)), C2 is C1+CXY, path_new1(A, [X, Y | L1], C2, L, C).

% (i, i, i), (o, i, i), (i, o, o); not_determined: (i, i, o), (o, i, o)
short_path(X, Y, L) :- path2([[Y]], X, L), not((path2([[Y]], X, L1), length(L1, N1), length(L, N), N1<N)).

path2([[X | R] | _], X, [X | R]).
path2([L1 | LR], Y, L) :- findall(X, next_point(L1, X), LS), append(LR, LS, NL), path2(NL, Y, L).

node(X) :- edge(X, _, _); edge(_, X, _).

cyclic :- setof(X, node(X), NList), findall(_, edge(_, _, _), EList), length(NList, N), length(EList, E), E1 is E+1, not(E1=N).

not_connected :- connect(X, _, _), connect(Y, _, _), not(X = Y), not(path(X, Y, _)).

is_connected :- not(not_connected).


































