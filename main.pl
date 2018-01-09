:- use_module(library(writef)).
:- nb_setval(logfile, 'project.log').
:- nb_setval(n_steps, 9).
:- nb_setval(n_elevators, 2).
:- nb_setval(n_people, 3).

% DEVEL

init :-
	write2log('Init proc'),
	nb_getval(n_people, NPeople),
	nb_getval(n_steps, Steps),
	randset(NPeople, Steps, RandPeopleAppearList),
	dec_list(RandPeopleAppearList, PeopleAppearList),
	nb_setval(people_appear, PeopleAppearList),
	swritef(PeopleLog, 'Init people \'%t\'', [PeopleAppearList]),
	write2log(PeopleLog).

write_id(Id) :-
	atom_concat('My id is ', Id, Res),
	write2log(Res).

check_people_appear(_, _, []).
check_people_appear(Step, Id, [H | T]) :-
	(Step = H ->
		swritef(AppearLog, 'A man appears with id \'%t\'', [Id]),
		write2log(AppearLog)
	; true),
	NextId is Id + 1,
	check_people_appear(Step, NextId, T).

do_loop_step(Step) :-
	write_id(Step),
	nb_getval(people_appear, PeopleAppearList),
	check_people_appear(Step, 0, PeopleAppearList).

% MISC

dec_list([], []).
dec_list([H | T], [ResH | ResT]) :- ResH is H - 1, dec_list(T, ResT).

write2log(Mes) :-
	nb_getval(logfile, Logfile),
	open(Logfile, append, LOG),
	get_time(Now), format_time(atom(Date), '%d %b %Y %T ', Now, posix),
	atom_concat(Date, Mes, Res),
	writeln(LOG, Res),
	close(LOG).

% PROCESSING

do_loop :-
	(nb_current(step, Step) ->
		false
	;
		write2log('Var \'Step\' has null value'),
		write2log('Set first step \'0\''),
		nb_setval(step, 0)
	),
	do_loop.
do_loop :-
	nb_getval(step, Step),
	nb_getval(n_steps, Steps),
	Step >= Steps.
do_loop :-
	nb_getval(step, Step),
	nb_getval(n_steps, Steps),
	Step < Steps, Next is Step + 1,
	nb_setval(step, Next),
	do_loop_step(Step),
	do_loop.

run:-
	write2log('Start modulation'),
	init,
	do_loop,
	write2log('Stop modulation').

% GOAL
:- run.
:- halt.
