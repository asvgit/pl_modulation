:- use_module(library(writef)).
:- nb_setval(logfile, 'project.log').
:- nb_setval(n_steps, 9).
:- nb_setval(n_elevators, 2).
:- nb_setval(n_floors, 5).
:- nb_setval(n_people, 3).

show_var :-
	nb_getval(n_steps, Steps),
	nb_getval(n_elevators, Elev),
	nb_getval(n_floors, Floors),
	nb_getval(n_people, People),
	swritef(VarLog, 'Show var: Steps \'%t\' Elev \'%t\' Floors \'%t\' People \'%t\'',
		[Steps, Elev, Floors, People]),
	write2log(VarLog).

% DEVEL

do_loop_step(Step) :-
	nb_getval(people_appear, PeopleAppearList),
	check_people_appear(Step, 0, PeopleAppearList),
	check_people_waiting(Step).

% MISC PEOLPE

manage_people_waiting(_, _, []).
manage_people_waiting(Step, Id, [H_PSL | T_PSL]) :-
	(H_PSL = 1 -> % 1 is wating state
		get_people_elem(people_waiting, Id, Waiting),
		swritef(WaitingLog, 'A man with id \'%t\' has been waiting for \'%t\'', [Id, Waiting]),
		write2log(WaitingLog),
		NewWaitingVal is Waiting + 1,
		set_people_elem(people_waiting, Id, NewWaitingVal)
	; true),
	NextId is Id + 1,
	manage_people_waiting(Step, NextId, T_PSL).

check_people_waiting(Step) :-
	nb_getval(people_states, PeopleStatesList),
	manage_people_waiting(Step, 0, PeopleStatesList).

check_people_appear(_, _, []).
check_people_appear(Step, Id, [H | T]) :-
	(Step = H ->
		get_people_elem(people_floors, Id, Floor),
		swritef(AppearLog, 'A man appears with id \'%t\' on floor with id %t', [Id, Floor]),
		write2log(AppearLog),
		set_people_elem(people_states, Id, 1)
	; true),
	NextId is Id + 1,
	check_people_appear(Step, NextId, T).

get_people_elem(ListName, Id, Res) :-
	nb_getval(n_people, NPeople),
	(Id >= NPeople ->
		write2log('Error! Index out of n_people'), halt
	;
		nb_getval(ListName, List),
		get_elem(List, Id, 0, Res)
	).

set_people_elem(ListName, Id, Val) :-
	nb_getval(n_people, NPeople),
	(Id >= NPeople ->
		write2log('Error! Index out of n_people'), halt
	;
		nb_getval(ListName, List),
		set_elem(List, Id, 0, Val, Res),
		nb_setval(ListName, Res),
		swritef(ListLog, 'Change list \'%t\' with id \'%t\' res:\'%t\'', [ListName, Id, Res]),
		write2log(ListLog)
	).

% MISC

get_elem([], _, _, _) :- write2log('Error! Can\'t find by index'), halt.
get_elem([H | T], Id, Ind, Res) :-
	(Ind = Id ->
		Res is H
	;
		NextInd is Ind + 1,
		get_elem(T, Id, NextInd, Res)
	).

set_elem([], _, _, _, []).
set_elem([H | T], Id, Ind, Val, [HRes | TRes]) :-
	(Ind = Id -> HRes is Val ; HRes is H),
	NextInd is Ind + 1,
	set_elem(T, Id, NextInd, Val, TRes).

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
	init_people,
	init_elevators.

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
	init_waiting_list,
	init_people_states.

init_waiting_list :-
	nb_getval(n_people, N),
	zero_list(N, List),
	nb_setval(people_waiting, List),
	swritef(PeopleLog, 'Init people waiting \'%t\'', [List]),
	write2log(PeopleLog).

% People statuses
% 0 - inactive
% 1 - wating
% 2 - moving
init_people_states :-
	nb_getval(n_people, NPeople),
	zero_list(NPeople, List),
	nb_setval(people_states, List),
	swritef(PeopleLog, 'Init people states \'%t\'', [List]),
	write2log(PeopleLog).

init_elevators :-
	write2log('Init elevators proc'),
	nb_getval(n_elevators, N),
	zero_list(N, List),
	nb_setval(elevators_waiting, List),
	swritef(ElevLog, 'Init elevators floors \'%t\'', [List]),
	write2log(ElevLog).

% PROCESSING
show_stat :-
	write2log('Show statistics'),
	nb_getval(people_waiting, PeopleWaitingList),
	swritef(PeopleWaitingLog, 'People waiting results: \'%t\'', [PeopleWaitingList]),
	write2log(PeopleWaitingLog).

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
	show_var,
	write2log('Start modulation'),
	init,
	do_loop,
	write2log('Finith modulation'),
	show_stat.

% GOAL
:- run.
:- halt.
