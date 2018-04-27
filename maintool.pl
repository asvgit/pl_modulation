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
	logtrace('Manage people'),
	nb_getval(n_people, NPeople),
	get_people_out(NPeople, Elev, Floor),
	get_people_in(NPeople, Elev, Floor).

change_elev_pos(_, _, []).
change_elev_pos(ElevId, ElevPos, [H | _]) :-
	swritef(Trace, 'Change ElevPos \'%t\' Id \'%t\'', [ElevPos, ElevId]),
	logtrace(Trace),
	( ElevPos > H ->
		NewPos is ElevPos - 1
	;
		NewPos is ElevPos + 1
	),
	nb_getval(elevators_floors, List),
	set_elem(List, ElevId, 0, NewPos, NewList),
	nb_setval(elevators_floors, NewList),
	swritef(ElevLog, 'Move elevator \'%t\' from \'%t\' to \'%t\'',
		[ElevId, ElevPos, NewPos]),
	logdebug(ElevLog).

move_elev(ElevId) :-
	nb_getval(elevators_floors, ElevFloors),
	get_elem(ElevFloors, ElevId, 0, ElevPos),
	MapPrefix = 'elev_rmap_',
	get_elev_list(MapPrefix, ElevId, RMap),
	( RMap = [] ->
		NewRMap = RMap
	;
		RMap = [HRMap | TRMap],
		swritef(ElevGoalLog, 'Elevator on \'%t\' has goal \'%t\'', [ElevPos, HRMap]),
		logdebug(ElevGoalLog),
		( HRMap = ElevPos ->
			swritef(ElevLog, 'Elevator \'%t\' has reached \'%t\'', [ElevId, ElevPos]),
			logdebug(ElevLog),
			set_elev_list(MapPrefix, ElevId, TRMap),
			manage_elev_people(ElevId, ElevPos),
			NewRMap = TRMap
		;
			( HRMap = -1 ->
				swritef(ElevClosingLog, 'Elevator \'%t\' is closing the door', [ElevId]),
				logtrace(ElevClosingLog),
				set_elev_list(MapPrefix, ElevId, TRMap),
				change_elev_pos(ElevId, ElevPos, TRMap),
				NewRMap = TRMap
			;
				change_elev_pos(ElevId, ElevPos, RMap),
				NewRMap = RMap
			)
		)
	),
	get_elev_list('elev_people_', ElevId, PeopleList),
	swritef(Log, 'Current elev \'%t\' road map \'%t\' people \'%t\'',
		[ElevId, NewRMap, PeopleList]),
	logdebug(Log).

moving_elevators(ElevId, NElev) :-
	(ElevId >= NElev ->
		true
	;
		(ElevId < 0 ->
			logerror('Error! Wrong value of index'),
			halt
		;
			swritef(Trace, 'moving_elevators NElev \'%t\' Id \'%t\'', [NElev, ElevId]),
			logtrace(Trace),
			move_elev(ElevId),
			NextId is ElevId + 1,
			moving_elevators(NextId, NElev)
		)
	).

move_elevators :-
	nb_getval(n_elevators, NElev),
	moving_elevators(0, NElev).
