:- use_module(library(writef)).
:- nb_setval(logfile, 'project.log').
:- nb_setval(steps, 9).

% DEVEL

write_id(Id) :-
	atom_concat('My id is ', Id, Res),
	write2log(Res).

do_loop :-
	(nb_current(step, Step) ->
		false
	;
		write2log('Set first step \'0\''),
		nb_setval(step, 0)
	),
	do_loop.
do_loop :-
	nb_getval(step, Step),
	nb_getval(steps, Steps),
	Step >= Steps.
do_loop :-
	nb_getval(step, Step),
	nb_getval(steps, Steps),
	Step < Steps, Next is Step + 1,
	nb_setval(step, Next),
	do_loop_step(Step),
	do_loop.

% MISC

write2log(Mes) :-
	nb_getval(logfile, Logfile),
	open(Logfile, append, LOG),
	get_time(Now), format_time(atom(Date), '%d %b %Y %T ', Now, posix),
	atom_concat(Date, Mes, Res),
	writeln(LOG, Res),
	close(LOG).

do_loop_step(Step) :-
	write_id(Step).

run:-
	write2log('Start modulation'),
	do_loop,
	write2log('Stop modulation').

% GOAL
:- run.
:- halt.
