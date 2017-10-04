:- use_module(library(writef)).

write_id(I) :- thread_self(X), writef('my id is %t\n', [X]), open('output', append, O), atom_concat(I, ' thread', R),writeln(O, R).
make_threads(N) :- N < 0.
make_threads(N) :- N >= 0, thread_create(go(N), _, []), I is N - 1, make_threads(I).

%misc
clear_log :- write('Clear log!\n'),
	(exists_file('output') -> delete_file('output'); true).
go(X) :- write_id(X).

% GOAL
%?- clear_log.
?- go(0).
?- make_threads(6).
?- halt.
