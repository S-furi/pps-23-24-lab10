search(X, cons(X, _)).
search(X, cons(_, Xs)) :- search(X, Xs).

% search(a, cons(a, cons(b, cons(c, nil))))
% search(a, cons(c, cons(d, cons(e, nil))))
% search(X, cons(a, cons(b, cons(c, nil))))
% search(a, X) --> inf. loop
% search(a, cons(X, ))
% search(a, cons(X, cons(b, cons(Y, cons(Z, nil)))))

search2(X, cons(X, cons(X, _))).
search2(X, cons(_, Xs)) :- search2(X, Xs).


search_two(X, cons(X, cons(Y, cons(X, _)))) :- X \= Y.
search_two(X, cons(_, T)) :- search_two(X, T).

% search_two(a, cons(c, cons(a, cons(a, cons(b, nil)))))
% search_two(a, cons(c, cons(a, cons(d, cons(a, cons(b, nil))))))

search_anytwo(H, cons(H, T)) :- search(H, T).
search_anytwo(H, cons(_, T)) :- search_anytwo(H, T).

% search_anytwo(a, cons(c, cons(a, cons(a, cons(b, nil)))))
% search_anytwo(a, cons(c, cons(a, cons(d, cons(a, cons(b, nil))))))

size(nil, zero).
size(cons(H, T), s(N)) :- size(T, N).

% size(cons(a, cons(b, cons(c, nil))), X)

% sum_list(List, Sum)

sum(X, zero, X).
sum(X, s(Y), s(Z)) :- sum(X, Y, Z).

sum_list(nil, zero).
sum_list(cons(H,T), O) :- sum_list(T, ST), sum(H, ST, O).

% sum_list(cons(zero, cons(s(s(zero)), cons(s(zero), nil))), X)

% count(List, Element, NOccurrensies)
count(List, E, N):- count(List, E, zero, N).
count(nil, E, N, N).
count(cons(E, L), E, N, M) :- count(L, E, s(N), M).
count(cons(E, L), E2, N, M) :- E \= E2, count(L, E2, N, M).

% count(cons(a,cons(b,cons(c,cons(a,cons(b,nil))))), a, N)
% count(cons(a,cons(b,cons(c,cons(a, cons(b, nil))))), a, zero, N)
% count(cons(b,cons(c,cons(a,cons(b,nil)))),a, s(zero), N)
% count(cons(b,cons(c, cons(a, cons(b, nil)))), a, s(zero), N)

% max(List, Max)
greater(s(N), zero, s(N)).
greater(zero, s(N), s(N)).
greater(zero, zero, zero).
greater(s(N1), s(N2), s(R)) :- greater(N1, N2, R).

max(cons(H, T), M) :- max(cons(H, T), zero, M).
max(cons(H, nil), TM, M) :- greater(H, TM, M).
max(cons(H, T), TM, M) :- greater(H, TM, M1), max(T, M1, M).

% max(cons(zero, cons(s(s(zero)), cons(s(zero), nil))), Max)

smaller(s(_), zero, zero).
smaller(zero, s(_), zero).
smaller(zero, zero, zero).
smaller(s(N1), s(N2), s(R)) :- smaller(N1, N2, R).

min(cons(H, T), M) :- min(cons(H,T), H, M).
min(cons(H, nil), TM, M) :- smaller(H, TM, M).
min(cons(H, T), TM, M) :- smaller(H, TM, M1), min(T, M1, M).

% min-max(List, Min, Max)
min_max(cons(H,T), Min, Max) :- max(cons(H, T), Max), min(cons(H, T), Min).

% same(List1, List2)
same(nil, nil).
same(cons(H, T1), cons(H, T2)) :- same(T1, T2).

% all_bigger(List1, List2)

bigger(s(_), zero).
bigger(s(X), s(Y)) :- bigger(X, Y).

all_bigger(cons(H1, nil), cons(H2, nil)) :- bigger(H1, H2).
all_bigger(cons(H1, T1), cons(H2, T2)) :- bigger(H1, H2), all_bigger(T1, T2).

% sublist(List1, List2)
sublist(cons(H1, nil), cons(H2, T2)) :- search(H1, cons(H2, T2)).
sublist(cons(H1, T1), cons(H2, T2)) :- search(H1, cons(H2, T2)), sublist(T1, cons(H2, T2)).

% seq(N, E, List)
seq(zero, _, nil).
seq(s(N), E, cons(E, T)) :- seq(N, E, T).

% seqR(N, List)
seqR(zero, nil).
seqR(s(N), cons(N, T)) :- seqR(N, T).

% seqR2(N, List)
last(cons(X, nil), E, cons(X, cons(E, nil))).
last(cons(H, T), E, cons(H, R)) :- last(T, E, R).

seqR2(zero, cons(zero, nil)).
seqR2(s(N), Result) :- seqR2(N, Rest), last(Rest, s(N), Result).

% List Functions Prolog Porting

%% Last
llast(cons(H, nil), X).
llast(cons(_, T), L) :- llast(T, L).

%% l map (_ + 1)
map(cons(H, nil), cons(s(H), nil)).
map(cons(H, T), cons(s(H), T)) :- map(cons(H, T)).