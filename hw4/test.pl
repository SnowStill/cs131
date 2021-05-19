transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).


check_count([], Z, Count, _) :-
    Z = Count.
check_count([H|T], Z, Count, P) :-
    (P < H -> M is Z + 1,
     check_count(T, M, Count, H);
     check_count(T, Z, Count, P)
     ).

check_sp_count([], []).
check_sp_count([R|F], [C|S]) :-
    check_count(R, 0, C, 0),
    check_sp_count(F, S).

check_all_counts(F, S, Top, Bottom, Left, Right) :-
    check_sp_count(S, Top),
    maplist(reverse, S, R),
        check_sp_count(R, Bottom),
    check_sp_count(F, Left),
    maplist(reverse, F, V),
    check_sp_count(V, Right).

setup(_, []).
setup(N, [H|T]) :-
    length(H, N),
    fd_domain(H, 1, N),
    fd_all_different(H),
    setup(N, T).

tower(N, T, C) :-
    C = counts(Top, Bottom, Left, Right),
    length(Top, N),
    length(Bottom, N),
    length(Left, N),
    length(Right, N),
    length(T, N),
    setup(N,T),
    transpose(T, V),
    setup(N, V),
    maplist(fd_labeling, T),
    check_all_counts(T, V, Top, Bottom, Left, Right).


% taken from TA slides

elements_between(List, Min, Max) :-
    maplist(between(Min,Max), List).

all_unique([]).
all_unique([H|T]) :- member(H,T), !, fail.
all_unique([_|T]) :- all_unique(T).

unique_list(List, N) :-
    length(List, N),
    elements_between(List, 1, N),
    all_unique(List).

plain_tower(N, T, C) :-
    C = counts(Top, Bottom, Left, Right),
    length(Top, N),
    length(Bottom, N),
    length(Left, N),
    length(Right, N),
    length(T, N),
    maplist(unique_list(N), T),
    transpose(T, V),
    maplist(unique_list(N), V),
    check_all_counts(T, V, Top, Bottom, Left, Right).


% speedup

test_tower(time) :-
    statistics(cpu, [s|_]),
    tower(5,_,counts([2,3,4,1,2],[3,2,1,3,2],[2,4,1,2,3],[2,1,3,3,2])),
    statistics(cpu, [e|_]),
    time is e-s.

test_plain_tower(time) :-
    statistics(cpu, [s|_]),
        plain_tower(5,_,counts([2,3,4,1,2],[3,2,1,3,2],[2,4,1,2,3],[2,1,3,3,2])),
        statistics(cpu, [e|_]),
        time is e-s.

speedup(ratio) :-
    test_tower(t),
    test_plain_tower(pt),
    ratio is pt/t.


% ambiguous

ambiguous(N, C, T1, T2) :-
    tower(N, T1, C),
    tower(N, T2, C),
    T1 \= T2.
