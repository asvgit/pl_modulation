:- ensure_loaded('conf.pl').
:- ensure_loaded('log.pl').
:- ensure_loaded('util.pl').

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
	var_getvalue(people_states, PeopleStatesList),
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
		var_getvalue(ListName, List),
		get_elem(List, Id, 0, Res)
	).

set_people_elem(ListName, Id, Val) :-
	nb_getval(n_people, NPeople),
	(Id >= NPeople ->
		logerror('Error! Index out of n_people'), halt
	;
		var_getvalue(ListName, List),
		set_elem(List, Id, 0, Val, Res),
		var_setvalue(ListName, Res),
		swritef(ListLog, 'Change list \'%t\' with id \'%t\' res:\'%t\'', [ListName, Id, Res]),
		logdebug(ListLog)
	).
