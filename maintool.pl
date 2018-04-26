:- ensure_loaded('conf.pl').
:- ensure_loaded('log.pl').
:- ensure_loaded('util.pl').
:- ensure_loaded('peopletool.pl').
:- ensure_loaded('elevatortool.pl').
:- ensure_loaded('init.pl').

getting_people_in(Elev, Floor, Id, Res) :-
	( Id > 0 ->
		swritef(Trace, 'Trace getting_people_in \'%t\'', [Id]),
		logtrace(Trace),
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
move_elev(Id, Pos, [Pos | TM], TM) :-
	swritef(ElevLog, 'Elevator \'%t\' has reached \'%t\'', [Id, Pos]),
	logdebug(ElevLog),
	logtrace('Get people'),
	manage_elev_people(Id, Pos).
	% move_elev(Id, Pos, TM, TList).
move_elev(Id, Pos, [-1 | TM], TList) :-
	swritef(ElevLog, 'Elevator \'%t\' is closing the door', [Id]),
	logtrace(ElevLog),
	move_elev(Id, Pos, TM, TList).
move_elev(Id, Pos, [HT | TM], [HT | TM]) :-
	( Pos > HT ->
		NewPos is Pos - 1
	;
		NewPos is Pos + 1
	),
	nb_getval(elevators_floors, List),
	set_elem(List, Id, 0, NewPos, NewList),
	nb_setval(elevators_floors, NewList),
	swritef(ElevLog, 'Move elevator \'%t\' from \'%t\' to \'%t\'',
		[Id, Pos, (NewPos)]),
	logdebug(ElevLog).

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
