% TreeMap = node in root
% node = (Key, Value, Priority, Size, Left, Right)

get_size(null, 0) :- !.
get_size(node(Key, Value, Priority, Size, Left, Right), Size).
node(Key, Value, Priority, Left, Right, node(Key, Value, Priority, Size, Left, Right)) :- get_size(Left, SL),
                                                                                          get_size(Right, SR),
                                                                                          Size is SL + SR + 1.


split(null, Key, null, null, null) :- !.
split(node(K, V, P, S, L, R), Key, TL, TM, TR) :- K < Key, !,
                                                  split(R, Key, TL_NEW, TM, TR),
                                                  node(K, V, P, L, TL_NEW, TL).
split(node(K, V, P, S, L, R), K, L, node(K, V, P, 1, null, null), R) :- !.
split(node(K, V, P, S, L, R), Key, TL, TM, TR) :- split(L, Key, TL, TM, TR_NEW),
                                                  node(K, V, P, TR_NEW, R, TR).
                                           

merge(null, T2, T2) :- !.
merge(T1, null, T1) :- !.
merge(node(K1, V1, P1, S1, L1, R1), node(K2, V2, P2, S2, L2, R2), Result) :- P1 > P2, !,
                                                                             node(K2, V2, P2, L2, R2, T2),
                                                                             merge(R1, T2, R1_NEW),
                                                                             node(K1, V1, P1, L1, R1_NEW, Result).
merge(node(K1, V1, P1, S1, L1, R1), node(K2, V2, P2, S2, L2, R2), Result) :- node(K1, V1, P1, L1, R1, T1),
                                                                             merge(T1, L2, L2_NEW),
                                                                             node(K2, V2, P2, L2_NEW, R2, Result).

map_get(node(K, V, P, S, L, R), K, V) :- !.
map_get(node(K, V, P, S, L, R), Key, Value) :- Key < K, !,
                                               map_get(L, Key, Value).
map_get(node(K, V, P, S, L, R), Key, Value) :- map_get(R, Key, Value).


map_put(TreeMap, Key, Value, Result) :- rand_int(2147483647, P),
                                        split(TreeMap, Key, TL, TM, TR),
                                        merge(TL, node(Key, Value, P, 1, null, null), TL_NEW),
                                        merge(TL_NEW, TR, Result).

map_remove(TreeMap, Key, Result) :- split(TreeMap, Key, TL, TM, TR),
                                    merge(TL, TR, Result).

map_build([], null) :- !.
map_build([(K, V) | L], TreeMap) :- map_build(L, T),
                                    map_put(T, K, V, TreeMap).

map_submapSize(Map, FromKey, ToKey, 0) :- FromKey >= ToKey, !.
map_submapSize(Map, FromKey, ToKey, Size) :- split(Map, FromKey, TL1, TM1, TR1),
                                             split(TR1, ToKey, TL2, TM2, TR2),
                                             get_size(TM1, S1),
                                             get_size(TL2, S2),
                                             Size is S1 + S2.

