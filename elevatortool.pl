:- ensure_loaded('conf.pl').
:- ensure_loaded('log.pl').
:- ensure_loaded('util.pl').

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
