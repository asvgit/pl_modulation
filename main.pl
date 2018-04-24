:- include('conf.pl').
:- include('log.pl').

show_var :-
	nb_getval(n_steps, Steps),
	nb_getval(n_elevators, Elev),
	nb_getval(n_floors, Floors),
	nb_getval(n_people, People),
	swritef(VarLog, 'Show var: Steps \'%t\' Elev \'%t\' Floors \'%t\' People \'%t\'',
		[Steps, Elev, Floors, People]),
	logdebug(VarLog).

% DEVEL

getting_people_in(Elev, Floor, Id, Res) :-
	( Id > 0 ->
		% swritef(Trace, 'Trace getting_people_in \'%t\'', [Id]),
		% logtrace(Trace),
		NextId is Id - 1,
		get_people_elem(people_floors, NextId, PFloor),
		get_people_elem(people_states, NextId, PState),
		(PFloor = Floor, PState = 1 ->
			Res = [H | T],
			H = NextId,
			set_people_elem(people_states, NextId, 2), % 2 - moving state
			get_people_elem(people_targets, NextId, PTarget),
			append2map(Elev, PTarget),
			getting_people_in(Elev, Floor, NextId, T)
		;
			getting_people_in(Elev, Floor, NextId, Res)
		)
	;
		Res = []
	).

get_people_in(NPeople, Elev, Floor) :-
	getting_people_in(Elev, Floor, NPeople, ComingPeople),
	swritef(ElevLog, 'Coming people \'%t\' to elev \'%t\'', [ComingPeople, Elev]),
	loginfo(ElevLog),
	ListPrefix = 'elev_people_',
	get_elev_list(ListPrefix, Elev, List),
	append(List, ComingPeople, NewList),
	set_elev_list(ListPrefix, Elev, NewList).

getting_people_out(Elev, Floor, Id, Res) :-
	( Id > 0 ->
		NextId is Id - 1,
		get_people_elem(people_targets, NextId, PTarget),
		get_people_elem(people_states, NextId, PState),
		get_elev_list('elev_people_', Elev, List),
		(PTarget = Floor, PState = 2, member(NextId, List) ->
			Res = [H | T],
			H = NextId,
			set_people_elem(people_states, NextId, 3), % 3 - Moved
			getting_people_out(Elev, Floor, NextId, T)
		;
			getting_people_out(Elev, Floor, NextId, Res)
		)
	;
		Res = []
	).

delete_sublist([], _, []).
delete_sublist(List, [], List).
delete_sublist(List, [HSL | TSL], ResList) :-
	delete(List, HSL, Res),
	delete_sublist(Res, TSL, ResList).

get_people_out(NPeople, Elev, Floor) :-
	getting_people_out(Elev, Floor, NPeople, PeopleList),
	swritef(ElevLog, 'Going out people \'%t\' from elev \'%t\'', [PeopleList, Elev]),
	loginfo(ElevLog),
	ListPrefix = 'elev_people_',
	get_elev_list(ListPrefix, Elev, List),
	delete_sublist(List, PeopleList, NewList),
	set_elev_list(ListPrefix, Elev, NewList).

manage_elev_people(Elev, Floor) :-
	nb_getval(n_people, NPeople),
	get_people_out(NPeople, Elev, Floor),
	get_people_in(NPeople, Elev, Floor).

move_elev(_, _, [], []).
move_elev(_, Pos, [Pos | TM], TM).
move_elev(Id, Pos, [-1 | _], TList) :-
	logtrace('Get people'),
	manage_elev_people(Id, Pos),
	get_elev_list('elev_rmap_', Id, [-1 | TList]).
move_elev(Id, Pos, [HT | TM], [HT | TM]) :-
	( Pos > HT ->
		NewPos = Pos - 1
	;
		NewPos = Pos + 1
	),
	nb_getval(elevators_floors, List),
	set_elem(List, Id, 0, NewPos, NewList),
	nb_setval(elevators_floors, NewList).

move_elevators(Id) :-
	(Id =< 0 ->
		true
	;
		NextId is Id - 1,
		swritef(Trace, 'Trace move_elevators \'%t\'', [NextId]),
		logtrace(Trace),
		ListPrefix = 'elev_rmap_',
		get_elev_list(ListPrefix, NextId, RMap),
		nb_getval(elevators_floors, ElevFloors),
		get_elem(ElevFloors, NextId, 0, ElevPos),
		move_elev(NextId, ElevPos, RMap, NewRMap),
		get_elev_list('elev_people_', NextId, PeopleList),
		swritef(ElevLog, 'Current elev \'%t\' road map \'%t\' people \'%t\'',
			[NextId, NewRMap, PeopleList]),
		logdebug(ElevLog),
		set_elev_list(ListPrefix, NextId, NewRMap),
		move_elevators(NextId)
	).

manage_elevators :-
	nb_getval(elevators_floors, ElevFloors),
	swritef(ElevLog, 'Elev floors \'%t\'', [ElevFloors]),
	logdebug(ElevLog),
	nb_getval(n_elevators, NElev),
	move_elevators(NElev).

do_loop_step(Step) :-
	check_people_appear(Step),
	check_people_waiting(Step),
	manage_elevators.

% MISC ELEVATORS

get_elev_list(ListPrefix, Id, List) :-
	nb_getval(n_elevators, NElev),
	(Id >= NElev ->
		logerror('Error! Index out of n_elevators'), halt
	;
		atom_concat(ListPrefix, Id, ListName),
		term_string(ListTerm, ListName),
		nb_getval(ListTerm, List)
	).

set_elev_list(ListPrefix, Id, List) :-
	nb_getval(n_elevators, NElev),
	(Id >= NElev ->
		logerror('Error! Index out of n_elevators'), halt
	;
		atom_concat(ListPrefix, Id, ListName),
		term_string(ListTerm, ListName),
		nb_setval(ListTerm, List)
	).

append2map(Elev, Floor) :-
	atom_concat('elev_rmap_', Elev, MapName),
	term_string(MapTerm, MapName),
	nb_getval(MapTerm, Map),
	append(Map, [Floor], MapWithFloor),
	append(MapWithFloor, [-1], NewMap),
	nb_setval(MapTerm, NewMap),
	swritef(ElevLog, 'Append to road map \'%t\' floor \'%t\'', [NewMap, Floor]),
	logdebug(ElevLog).

get_min_dist_id([], 100500, Id) :- nb_getval(n_elevators, Id).
get_min_dist_id([HDist | TDist], Res, Id) :-
	get_min_dist_id(TDist, Min, MinId),
	(Min >= HDist ->
		Res is HDist,
		Id is MinId - 1
	;
		Res is Min,
		Id is MinId
	).

get_dist(Pos, [], Floor, Res) :- Res is abs(Pos - Floor).
get_dist(Pos, [H | T], Floor, Res) :-
	(H = -1 ->
		get_dist(Pos, T, Floor, Dist),
		Res is Dist + 1
	;
		get_dist(H, T, Floor, Dist),
		Res is Dist + abs(Pos - H)
	).

fill_dist(0, _, Dist, Dist).
fill_dist(Ind, Floor, Dist, Res) :-
	NextInd is Ind - 1,
	nb_getval(elevators_floors, ElevFloors),
	get_elem(ElevFloors, NextInd, 0, ElevPos),
	atom_concat('elev_rmap_', NextInd, MapName),
	term_string(MapTerm, MapName),
	nb_getval(MapTerm, Map),
	get_dist(ElevPos, Map, Floor, D),
	set_elem(Dist, NextInd, 0, D, NewDist),
	fill_dist(NextInd, Floor, NewDist, Res).

find_available_elev(Floor) :-
	nb_getval(n_elevators, NElev),
	zero_list(NElev, Dist),
	fill_dist(NElev, Floor, Dist, Distances),
	swritef(ElevDistLog, 'Current distances are \'%t\'', [Distances]),
	logdebug(ElevDistLog),
	get_min_dist_id(Distances, MinDist, Elev),
	swritef(ElevMinDistLog, 'Current min dist \'%t\' with id \'%t\'', [MinDist, Elev]),
	logdebug(ElevMinDistLog),
	append2map(Elev, Floor).

find_in_list([], _, Res) :- Res = false.
find_in_list([H | T], Val, Res) :-
	(H = Val ->
		Res = true
	;
		find_in_list(T, Val, Res)
	).

find_in_elev_lists(_, Ind, Res) :- Ind =< 0, Res = false.
find_in_elev_lists(Floor, Ind, Res) :-
	Ind >= 0,
	NextInd is Ind - 1,
	atom_concat('elev_rmap_', NextInd, ListName),
	term_string(ListTerm, ListName),
	nb_getval(ListTerm, List),
	find_in_list(List, Floor, FRes),
	(FRes ->
		Res = true
	;
		find_in_elev_lists(Floor, NextInd, Res)
	).

find_floor(Floor, Res) :-
	nb_getval(n_elevators, NElev),
	find_in_elev_lists(Floor, NElev, Res).

elev_call(Floor) :-
	find_floor(Floor, IsFloorInMaps),
	(IsFloorInMaps ->
		logdebug('Floor is in maps')
	;
		logdebug('Putting floor to map'),
		find_available_elev(Floor)
	).

% MISC PEOLPE

manage_people_waiting(_, _, []).
manage_people_waiting(Step, Id, [H_PSL | T_PSL]) :-
	(H_PSL = 1 -> % 1 is wating state
		get_people_elem(people_waiting, Id, Waiting),
		swritef(WaitingLog, 'A man with id \'%t\' has been waiting for \'%t\'', [Id, Waiting]),
		logdebug(WaitingLog),
		NewWaitingVal is Waiting + 1,
		set_people_elem(people_waiting, Id, NewWaitingVal)
	; true),
	NextId is Id + 1,
	manage_people_waiting(Step, NextId, T_PSL).

check_people_waiting(Step) :-
	nb_getval(people_states, PeopleStatesList),
	manage_people_waiting(Step, 0, PeopleStatesList).

manage_people_appear(_, _, []).
manage_people_appear(Step, Id, [H | T]) :-
	(Step = H ->
		get_people_elem(people_floors, Id, Floor),
		swritef(AppearLog, 'A man appears with id \'%t\' on floor with id \'%t\'', [Id, Floor]),
		loginfo(AppearLog),
		set_people_elem(people_states, Id, 1),
		elev_call(Floor)
	; true),
	NextId is Id + 1,
	manage_people_appear(Step, NextId, T).

check_people_appear(Step) :-
	nb_getval(people_appear, PeopleAppearList),
	manage_people_appear(Step, 0, PeopleAppearList).

get_people_elem(ListName, Id, Res) :-
	nb_getval(n_people, NPeople),
	(Id >= NPeople ->
		logerror('Error! Index out of n_people'), halt
	;
		nb_getval(ListName, List),
		get_elem(List, Id, 0, Res)
	).

set_people_elem(ListName, Id, Val) :-
	nb_getval(n_people, NPeople),
	(Id >= NPeople ->
		logerror('Error! Index out of n_people'), halt
	;
		nb_getval(ListName, List),
		set_elem(List, Id, 0, Val, Res),
		nb_setval(ListName, Res),
		swritef(ListLog, 'Change list \'%t\' with id \'%t\' res:\'%t\'', [ListName, Id, Res]),
		logdebug(ListLog)
	).

% MISC

get_elem([], _, _, _) :- logerror('Error! Can\'t find by index'), halt.
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

enother_random(Val, Top, Res) :-
	random(0, Top, Rnd),
	(Val = Rnd ->
		enother_random(Val, Top, Res)
	;
		Res is Rnd
	).

dec_list([], []).
dec_list([H | T], [ResH | ResT]) :- ResH is H - 1, dec_list(T, ResT).

get_step_mod(Res) :-
	nb_getval(n_steps, Steps),
	(nb_current(step, Step), Step < Steps ->
		Res = Step
	;
		Res = null
	).
% Init

init :-
	logtrace('Init proc'),
	init_people,
	init_elevators.

init_people :-
	logtrace('Init people proc'),
	nb_getval(n_steps, Steps),
	nb_getval(n_people, NPeople),
	rand_seq(NPeople, Steps, PeopleAppearList),
	nb_setval(people_appear, PeopleAppearList),
	swritef(PeopleAppearLog, 'Init people apprears \'%t\'', [PeopleAppearList]),
	logdebug(PeopleAppearLog),
	nb_getval(n_floors, NFloors),
	rand_seq(NPeople, NFloors, PeopleFloorList),
	nb_setval(people_floors, PeopleFloorList),
	swritef(PeopleFloorLog, 'Init people floors \'%t\'', [PeopleFloorList]),
	logdebug(PeopleFloorLog),
	init_people_targets,
	init_waiting_list,
	init_people_states.

init_waiting_list :-
	nb_getval(n_people, N),
	zero_list(N, List),
	nb_setval(people_waiting, List),
	swritef(PeopleLog, 'Init people waiting \'%t\'', [List]),
	logdebug(PeopleLog).

% People statuses
% 0 - inactive
% 1 - wating
% 2 - moving
% 3 - moved
init_people_states :-
	nb_getval(n_people, NPeople),
	zero_list(NPeople, List),
	nb_setval(people_states, List),
	swritef(PeopleLog, 'Init people states \'%t\'', [List]),
	logdebug(PeopleLog).

fill_people_targets(_, [], []).
fill_people_targets(NFloors, [HPFL | TPFL], [HPTL | TPTL]) :-
	enother_random(HPFL, NFloors, HPTL),
	fill_people_targets(NFloors, TPFL, TPTL).

init_people_targets :-
	nb_getval(n_floors, NFloors),
	nb_getval(people_floors, PeopleFloorList),
	fill_people_targets(NFloors, PeopleFloorList, PeopleTargetsList),
	nb_setval(people_targets, PeopleTargetsList),
	swritef(PeopleLog, 'Init people targets \'%t\'', [PeopleTargetsList]),
	logdebug(PeopleLog).


init_elevators :-
	logtrace('Init elevators proc'),
	nb_getval(n_elevators, N),
	zero_list(N, List),
	nb_setval(elevators_floors, List),
	swritef(ElevLog, 'Init elevators floors \'%t\'', [List]),
	logdebug(ElevLog),
	init_elev_lists.

init_elev_lists :-
	logtrace('Init elevators lists'),
	nb_getval(n_elevators, NElev),
	fill_elev_list(NElev).

fill_elev_list(Id) :-
	(Id =< 0 ->
		true
	;
		NextId is Id - 1,
		empty_set('elev_rmap_', NextId),
		empty_set('elev_people_', NextId),
		fill_elev_list(NextId)
	).

empty_set(Prefix, Id) :-
		atom_concat(Prefix, Id, Res),
		term_string(Name, Res),
		nb_setval(Name, []),
		swritef(ElevLog, 'Init elevator \'%t\', val \'%t\'', [Name, []]),
		logdebug(ElevLog).

% PROCESSING
show_stat :-
	logtrace('Show statistics'),
	nb_getval(people_waiting, PeopleWaitingList),
	swritef(PeopleWaitingLog, 'People waiting results: \'%t\'', [PeopleWaitingList]),
	loginfo(PeopleWaitingLog).

do_loop :-
	(nb_current(step, _) ->
		false
	;
		logwarn('Var \'Step\' has null value'),
		loginfo('Set first step \'0\''),
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
	logtrace('Start step'),
	do_loop_step(Step),
	logtrace('Finish step'),
	Next is Step + 1,
	nb_setval(step, Next),
	do_loop.

run:-
	show_var,
	logtrace('Start modulation'),
	init,
	do_loop,
	logtrace('Finith modulation'),
	show_stat.

% GOAL
:- run.
:- halt.
