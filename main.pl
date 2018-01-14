:- use_module(library(writef)).
:- nb_setval(logfile, 'project.log').
:- nb_setval(n_steps, 9).
:- nb_setval(n_elevators, 2).
:- nb_setval(n_people, 3).
:- nb_setval(n_floors, 5).

% DEVEL

get_elem([], _, _, _) :- write2log('Error! Can\'t find by index'), halt.
get_elem([H | T], Id, Ind, Res) :-
	(Ind = Id ->
		Res is H
	;
		NextInd is Ind + 1,
		get_elem(T, Id, NextInd, Res)
	).

get_people_elem(ListName, Id, Res) :-
	nb_getval(n_people, NPeople),
	(Id >= NPeople ->
		write2log('Error! Index out of n_people'), halt
	;
		nb_getval(ListName, List),
		get_elem(List, Id, 0, Res)
	).

check_people_appear(_, _, []).
check_people_appear(Step, Id, [H | T]) :-
	(Step = H ->
		get_people_elem(people_floors, Id, Floor),
		swritef(AppearLog, 'A man appears with id \'%t\' on floor with id %t', [Id, Floor]),
		write2log(AppearLog)
	; true),
	NextId is Id + 1,
	check_people_appear(Step, NextId, T).

do_loop_step(Step) :-
	nb_getval(people_appear, PeopleAppearList),
	check_people_appear(Step, 0, PeopleAppearList).

% MISC

zero_list(N, []) :- N =< 0.
zero_list(N, [H | T]) :- Next is N - 1, H is 0, zero_list(Next, T).

rand_seq(0, _, []).
rand_seq(Size, Top, [H | T]) :- random(0, Top, H), Next is Size - 1, rand_seq(Next, Top, T).

dec_list([], []).
dec_list([H | T], [ResH | ResT]) :- ResH is H - 1, dec_list(T, ResT).

get_step_mod(Res) :-
	nb_getval(n_steps, Steps),
	(nb_current(step, Step), Step < Steps ->
		Res = Step
	;
		Res = null
	).

write2log(Mes) :-
	nb_getval(logfile, Logfile),
	open(Logfile, append, LOG),
	get_time(Now), format_time(atom(Date), '%d %b %Y %T ', Now, posix),
	get_step_mod(SID),
	process_id(PID),
	swritef(StepId, '[%t:%t] ', [PID, SID]),
	atom_concat(Date, StepId, Prefix),
	atom_concat(Prefix, Mes, Res),
	writeln(LOG, Res),
	close(LOG).
% Init

init :-
	write2log('Init proc'),
	init_people.

init_people :-
	write2log('Init people proc'),
	nb_getval(n_steps, Steps),
	nb_getval(n_people, NPeople),
	rand_seq(NPeople, Steps, PeopleAppearList),
	nb_setval(people_appear, PeopleAppearList),
	swritef(PeopleAppearLog, 'Init people apprears \'%t\'', [PeopleAppearList]),
	write2log(PeopleAppearLog),
	nb_getval(n_floors, NFloors),
	rand_seq(NPeople, NFloors, PeopleFloorList),
	nb_setval(people_floors, PeopleFloorList),
	swritef(PeopleFloorLog, 'Init people floors \'%t\'', [PeopleFloorList]),
	write2log(PeopleFloorLog),
	init_waiting_list.

init_waiting_list :-
	nb_getval(n_people, N),
	zero_list(N, List),
	nb_setval(people_waiting, List),
	swritef(PeopleLog, 'Init people waiting \'%t\'', [List]),
	write2log(PeopleLog).

% PROCESSING

do_loop :-
	(nb_current(step, _) ->
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
	Step < Steps,
	write2log('Start step'),
	do_loop_step(Step),
	write2log('Finish step'),
	Next is Step + 1,
	nb_setval(step, Next),
	do_loop.

run:-
	write2log('Start modulation'),
	init,
	do_loop,
	write2log('Finith modulation').

% GOAL
:- run.
:- halt.
