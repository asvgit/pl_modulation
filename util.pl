:- ensure_loaded('conf.pl').
:- ensure_loaded('log.pl').

show_var :-
	nb_getval(n_steps, Steps),
	nb_getval(n_elevators, Elev),
	nb_getval(n_floors, Floors),
	nb_getval(n_people, People),
	swritef(VarLog, 'Show var: Steps \'%t\' Elev \'%t\' Floors \'%t\' People \'%t\'',
		[Steps, Elev, Floors, People]),
	logdebug(VarLog).

show_stat :-
	logtrace('Show statistics'),
	var_getvalue(people_states, PeopleStatusList),
	swritef(PeopleStaatusLog, 'People status results: \'%t\'', [PeopleStatusList]),
	loginfo(PeopleStaatusLog),
	var_getvalue(people_waiting, PeopleWaitingList),
	swritef(PeopleWaitingLog, 'People waiting results: \'%t\'', [PeopleWaitingList]),
	loginfo(PeopleWaitingLog).

sim_getval(SimPrefix, Var, Value) :-
	atom_concat(SimPrefix, Var, VarName),
	term_string(NewVar, VarName),
	( nb_current(NewVar, Value) -> true;
		swritef(ErrorLog, 'Such var does not exist \'%t\'', [NewVar]),
		logerror(ErrorLog),
		halt
	).
	
sim_setval(SimPrefix, Var, Value) :-
	atom_concat(SimPrefix, Var, VarName),
	term_string(NewVar, VarName),
	nb_setval(NewVar, Value),
	( nb_current(NewVar, _) -> 
		nb_setval(NewVar, Value)
	;
		swritef(ErrorLog, 'Fail set var! Such var does not exist \'%t\'', [NewVar]),
		logerror(ErrorLog),
		halt
	).

var_getvalue(Key, Value) :-
	( nb_current(current_sim, CurrentPrefix) ->
		sim_getval(CurrentPrefix, Key, Value)
	;
		( nb_current(Key, Value) -> true;
			swritef(ErrorLog, 'Such var does not exist \'%t\'', [Key]),
			logerror(ErrorLog),
			halt
		)
	).

var_setvalue(Key, Value) :-
	( nb_current(current_sim, CurrentPrefix) ->
		sim_setval(CurrentPrefix, Key, Value)
	;
		( nb_current(Key, _) ->
			nb_setval(Key, Value)
		;
			swritef(ErrorLog, 'Fail set var! Such var does not exist \'%t\'', [Key]),
			logerror(ErrorLog),
			halt
		)
	).

is_memder([], _) :- false.
is_memder([H | _], Val) :- H = Val.
is_memder([_ | T], Val) :- is_memder(T, Val).

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

manage_elevators :-
	var_getvalue(elevators_floors, ElevFloors),
	swritef(ElevLog, 'Elev floors \'%t\'', [ElevFloors]),
	logdebug(ElevLog),
	move_elevators.

manage_people :-
	var_getvalue(step,Step),
	check_people_appear(Step),
	check_people_waiting(Step).
