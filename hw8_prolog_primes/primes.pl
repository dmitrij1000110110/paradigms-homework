not_primes(1).
sieve_loop(DIV, I, N, STEP) :- I < N,
                               assert(not_primes(I)),
                               NEW_I is I + STEP,
                               sieve_loop(DIV, NEW_I, N, STEP).
build_sieve(I, N) :- not not_primes(I),
                     NEXT_I is I * I,
                     sieve_loop(I, NEXT_I, N, I).
build_sieve(I, N) :- I * I < N,
                     NEXT_I is I + 1,
                     build_sieve(NEXT_I, N).

init(MAX_N) :- build_sieve(2, MAX_N).
composite(X) :- X > 1, not_primes(X).
prime(X) :- not not_primes(X).

min_divisor_loop(N, I, R) :- I * I < N + 1,
                             prime(I),
                             0 is N mod I, R is I, !.
min_divisor_loop(N, I, R) :- I * I < N + 1,
                             NEW_I is I + 1, !,
                             min_divisor_loop(N, NEW_I, R).
min_divisor_loop(N, I, R) :- R is N, !.

sorted_and_prime([]) :- !.
sorted_and_prime([H]) :- prime(H), !.
sorted_and_prime([H, H1 | T]) :- H =< H1,
                                 prime(H),
                                 sorted_and_prime([H1 | T]).

mult_list(1, []) :- !.
mult_list(N, [H]) :- N is H, !.
mult_list(N, [H | T]) :- mult_list(N1, T),
                         N is N1 * H.		

prime_divisors(1, []).
prime_divisors(N, [N]) :- prime(N), !.
prime_divisors(N, [H | T]) :- number(N), !,
                              N > 1, min_divisor_loop(N, 2, H),
                              N1 is N / H, prime_divisors(N1, T).
prime_divisors(N, [H | T]) :- sorted_and_prime([H | T]),
                              mult_list(N, [H | T]).


merge([], L, L) :- !.
merge(L, [], L) :- !.
merge([H | T1], [H | T2], [H | T]) :- !, merge(T1, T2, T).
merge([H1 | T1], [H2 | T2], [H1 | T]) :- H1 < H2, !,
                                       merge(T1, [H2 | T2], T).
merge([H1 | T1], [H2 | T2], [H2 | T]) :- H2 < H1, !,
                                       merge([H1 | T1], T2, T).

lcm(A, B, LCM) :- prime_divisors(A, LA),
                  prime_divisors(B, LB),
                  merge(LA, LB, R),
                  mult_list(LCM, R).
