:- ensure_loaded('conf.pl').
:- ensure_loaded('log.pl').
:- ensure_loaded('util.pl').
:- ensure_loaded('peopletool.pl').
:- ensure_loaded('elevatortool.pl').

init :-
	logtrace('Init proc'),
	init_people,
	init_elevators.

init_people :-
	logtrace('Init people proc'),
	% nb_getval(n_steps, Steps),
	% nb_getval(n_people, NPeople),
	% rand_seq(NPeople, Steps, PeopleAppearList),
	PeopleAppearList = [2, 4],
	nb_setval(people_appear, PeopleAppearList),
	swritef(PeopleAppearLog, 'Init people apprears \'%t\'', [PeopleAppearList]),
	logdebug(PeopleAppearLog),
	% nb_getval(n_floors, NFloors),
	% rand_seq(NPeople, NFloors, PeopleFloorList),
	PeopleFloorList = [1, 0],
	nb_setval(people_floors, PeopleFloorList),
	swritef(PeopleFloorLog, 'Init people floors \'%t\'', [PeopleFloorList]),
	logdebug(PeopleFloorLog),
	init_people_targets,
	init_people_probability,
	init_people_probability_real,
	init_waiting_list,
	init_people_states.

init_people_probability :-
	nb_getval(n_people, N),
	rand_seq(N, 100, List),
	nb_setval(people_probability, List),
	swritef(PeopleLog, 'Init people probability \'%t\'', [List]),
	logdebug(PeopleLog).

init_people_probability_real :-
	nb_getval(n_people, N),
	rand_seq(N, 100, List),
	nb_setval(people_ver, List),
	swritef(PeopleLog, 'Init people probability (real) \'%t\'', [List]),
	logdebug(PeopleLog).

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
	% nb_getval(n_floors, NFloors),
	% nb_getval(people_floors, PeopleFloorList),
	% fill_people_targets(NFloors, PeopleFloorList, PeopleTargetsList),
	PeopleTargetsList = [4, 4],
	nb_setval(people_targets, PeopleTargetsList),
	swritef(PeopleLog, 'Init people targets \'%t\'', [PeopleTargetsList]),
	logdebug(PeopleLog).


init_elevators :-
	logtrace('Init elevators proc'),
	% nb_getval(n_elevators, N),
	% zero_list(N, List),
	List = [0, 1],
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
