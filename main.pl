:- ensure_loaded('conf.pl').
:- ensure_loaded('log.pl').
:- ensure_loaded('util.pl').
:- ensure_loaded('peopletool.pl').
:- ensure_loaded('elevatortool.pl').
:- ensure_loaded('maintool.pl').
:- ensure_loaded('init.pl').

manage_elevators :-
	nb_getval(elevators_floors, ElevFloors),
	swritef(ElevLog, 'Elev floors \'%t\'', [ElevFloors]),
	logdebug(ElevLog),
	nb_getval(n_elevators, NElev),
	move_elevators(NElev).

manage_people :-
	check_people_appear(Step),
	check_people_waiting(Step).

do_loop_step :-
	manage_people,
	manage_elevators.

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
	do_loop_step,
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
