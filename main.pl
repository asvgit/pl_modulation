:- use_module(library(writef)).

% DEVEL

write_id(Id) :-
	atom_concat('My id is ', Id, Res),
	writeln(Res),
	write2log(Res).

make_threads(N, []) :- N =< 0.
make_threads(N, [Id|Ids]) :- N > 0,
	thread_create(go(N), Id, []),
	I is N - 1,
	make_threads(I, Ids).

% MISC

go(X) :- with_mutex(mut,write_id(X)).

write2log(Mes) :-
	thread_self(TID),
	open('project.log', append, LOGFILE),
	get_time(Now), format_time(atom(Date), '%d %b %Y %T ', Now, posix),
	%atom_concat(Date, TID, Info),
	%atom_concat(' ', Mes, Line),
	%atom_concat(Info, Line, Res),
	atom_concat(Date, Mes, Res),
	writeln(LOGFILE, Res).

join([]).
join([I|T]):-thread_join(I,_), join(T).

run:-
	mutex_create(mut),
	make_threads(6, Ids),
	% join.
	join(Ids),
	mutex_destroy(mut).

% GOAL

%:- write2log('Start main thread').
:- run.
%:- write2log('Stop main thread').
:- halt.
