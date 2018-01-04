:- use_module(library(writef)).

write_id(I) :- thread_self(X), writef('my id is %t\n', [X]), open('output', append, O), atom_concat(I, ' thread', R),writeln(O, R).
make_threads(N, []) :- N =< 0.
make_threads(N, [Id|Ids]) :- N > 0, thread_create(go(N), Id, []), I is N - 1, make_threads(I, Ids).


%misc
clear_log :- write('Clear log!\n'),
	(exists_file('output') -> delete_file('output'); true).
go(X) :- with_mutex(mut,write_id(X)).


run:-
	mutex_create(mut),
	make_threads(6, Ids),
	% join.
	join(Ids),
	mutex_destroy(mut).

join([]).
join([I|T]):-thread_join(I,_), join(T).

%initialization:-
%	run,
%	halt.

:- run.
:- halt.


% ?- run.

% GOAL
%?- clear_log.
%?- go(0).
%?- make_threads(6).
%?- halt.
