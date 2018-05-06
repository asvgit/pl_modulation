:- ensure_loaded('conf.pl').
:- ensure_loaded('util.pl').

:- use_module(library(writef)).
:- nb_setval(logfile, 'project.log').

:- nb_setval(colorend, '\033[0m').
:- nb_setval(color_green, '\033[1;32m').
:- nb_setval(color_yellow, '\033[1;33m').
:- nb_setval(color_orange, '\033[0;33m').
:- nb_setval(color_black, '\033[0;30m').
:- nb_setval(color_gray, '\033[1;30m').
:- nb_setval(color_cyan, '\033[1;36m').
:- nb_setval(color_rad, '\033[1;31m').

write2log(Mes) :-
	nb_getval(logfile, Logfile),
	open(Logfile, append, LOG),
	get_time(Now), format_time(atom(Date), '%d %b %Y %T ', Now, posix),
	get_step_mod(SID),
	process_id(PID),
	swritef(StepId, '[%t:%t] ', [PID, SID]),
	atom_concat(Date, StepId, Prefix),
	atom_concat(Prefix, Mes, Res),
	writeln(LOG, Res),
	close(LOG).

make_logstr(Name, Color, Mes) :-
	atom_concat(' ', Mes, Msg),
	nb_getval(use_color_log, CUse),
	( CUse > 0 -> 
		nb_getval(colorend, CEnd),
		atom_concat(Color, Name, CName),
		atom_concat(CName, Msg, CTempl),
		atom_concat(CTempl, CEnd, Res)
	;
		atom_concat(Name, Msg, Res)
	),
	( nb_current(current_sim, CurrentPrefix) ->
		nb_getval(use_log, LogLevel),
		( LogLevel > 3 ->
			sim_getval(CurrentPrefix, step, SimStep),
			swritef(SimPrefix, '[%t:%t] ', [CurrentPrefix, SimStep]),
			atom_concat(SimPrefix, Res, SimRes),
			write2log(SimRes)
		; true)
	;
		write2log(Res)
	).

	% nb_getval(use_log, LogLevel), LogLevel > 3,
logerror(Mes) :-
	nb_getval(color_rad, Color),
	make_logstr('ERROR', Color, Mes).	

logwarn(Mes) :-
	nb_getval(color_orange, Color),
	make_logstr('WARNING', Color, Mes).	

loginfo(Mes) :-
	nb_getval(use_log, LogLevel), LogLevel > 0,
	nb_getval(color_green, Color),
	make_logstr('INFO', Color, Mes).	
loginfo(_).

logtrace(Mes) :-
	nb_getval(use_log, LogLevel), LogLevel > 1,
	nb_getval(color_cyan, Color),
	make_logstr('TRACE', Color, Mes).	
logtrace(_).

logdebug(Mes) :-
	nb_getval(use_log, LogLevel), LogLevel > 2,
	nb_getval(color_yellow, Color),
	make_logstr('DEBUG', Color, Mes).	
logdebug(_).
