:- ensure_loaded('conf.pl').
:- ensure_loaded('log.pl').
:- ensure_loaded('util.pl').
:- ensure_loaded('peopletool.pl').
:- ensure_loaded('elevatortool.pl').
:- ensure_loaded('maintool.pl').
:- ensure_loaded('init.pl').
:- ensure_loaded('simulationtool.pl').

init_sim(SimPrefix) :-
	copy_var(SimPrefix, step),
	copy_var(SimPrefix, people_targets),
	copy_var(SimPrefix, people_floors),
	copy_var(SimPrefix, people_waiting),
	copy_var(SimPrefix, people_states),
	copy_var(SimPrefix, elevators_floors),
	nb_getval(n_elevators, NElev),
	copy_elev_list(SimPrefix, NElev).

simulate_loop_step :-
	logtrace('Do loop step'),
	manage_people,
	manage_elevators.

simulate_loop(SimPrefix) :-
	nb_setval(current_sim, SimPrefix),
	sim_getval(SimPrefix, step, Step),
	nb_getval(n_steps, Steps),
	Step >= Steps,
	swritef(SimLog, 'Simulation \'%t\' is finished', [SimPrefix]),
	logtrace(SimLog),
	show_stat.
simulate_loop(SimPrefix) :-
	nb_setval(current_sim, SimPrefix),
	var_getvalue(step, Step),
	nb_getval(n_steps, Steps),
	Step < Steps,
	logtrace('Start step'),
	simulate_loop_step,
	logtrace('Finish step'),
	Next is Step + 1,
	var_setvalue(step, Next),
	simulate_loop(SimPrefix).

simulate :-
	(nb_current(current_sim, CurrentPrefix) ->
		SimPrefix = CurrentPrefix
	;
		SimPrefix = 'r_'
	),
	init_sim(SimPrefix),
	simulate_loop(SimPrefix).

do_simulate(Floor, Elev, H) :-
	nb_getval(current_sim, SimPrefix),
	append2map(Elev, Floor),
	manage_elevators,
	var_getvalue(step, Step),
	Next is Step + 1,
	var_setvalue(step, Next),
	simulate_loop(SimPrefix),
	sim_getval(SimPrefix, people_waiting, PeopleWaitingList),
	sum_list(PeopleWaitingList, H),
	swritef(SimLog, 'Waiting sum is \'%t\'', [H]),
	logdebug(SimLog).

simulate(Floor, Elev, H) :-
	(nb_current(current_sim, CurrentPrefix) ->
		atom_concat(CurrentPrefix, Elev, SimPrefix),
		atom_concat(SimPrefix, '_', Prefix),
		init_sim(Prefix),
		nb_setval(current_sim, Prefix),
		do_simulate(Floor, Elev, H),
		nb_setval(current_sim, CurrentPrefix)
	;
		atom_concat('r_', Elev, SimPrefix),
		atom_concat(SimPrefix, '_', Prefix),
		init_sim(Prefix),
		nb_setval(current_sim, Prefix),
		do_simulate(Floor, Elev, H),
		nb_delete(current_sim)
	).

calculating(_, Elev, []) :-
	nb_getval(n_elevators, NElev),
   	Elev >= NElev.
calculating(Floor, Elev, [H | T]) :-
	nb_getval(n_elevators, NElev),
   	Elev < NElev,
	simulate(Floor, Elev, H),
	Next is Elev + 1,
	calculating(Floor, Next, T).

calculate(Floor, ResList) :-
	calculating(Floor, 0, ResList).

do_elev_call(Floor, Elev) :-
	calculate(Floor, ResList),
	min_list(ResList, Min),
	nth0(Elev, ResList, Min).
