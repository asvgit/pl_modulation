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
	copy_var(SimPrefix, people_waiting),
	copy_var(SimPrefix, people_states),
	copy_var(SimPrefix, elevators_floors),
	nb_getval(n_elevators, NElev),
	copy_elev_list(SimPrefix, NElev).

simulate_loop_step :-
	logtrace('Do loop step').

simulate_loop(SimPrefix) :-
	nb_setval(current_sim, SimPrefix),
	sim_getval(SimPrefix, step, Step),
	nb_getval(n_steps, Steps),
	Step >= Steps,
	swritef(SimLog, 'Simulation \'%t\' is finished', [SimPrefix]),
	logtrace(SimLog),
	nb_delete(current_sim).
simulate_loop(SimPrefix) :-
	nb_setval(current_sim, SimPrefix),
	sim_getval(SimPrefix, step, Step),
	nb_getval(n_steps, Steps),
	Step < Steps,
	logtrace('Start step'),
	simulate_loop_step,
	logtrace('Finish step'),
	Next is Step + 1,
	sim_setval(SimPrefix, step, Next),
	simulate_loop(SimPrefix).

simulate :-
	(nb_current(current_sim, CurrentPrefix) ->
		SimPrefix = CurrentPrefix
	;
		SimPrefix = 'r_'
	),
	init_sim(SimPrefix),
	simulate_loop(SimPrefix).
