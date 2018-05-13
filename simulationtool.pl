:- ensure_loaded('conf.pl').
:- ensure_loaded('log.pl').
:- ensure_loaded('util.pl').
:- ensure_loaded('peopletool.pl').
:- ensure_loaded('elevatortool.pl').
:- ensure_loaded('maintool.pl').
:- ensure_loaded('init.pl').

copy_var(SimPrefix, Var) :-
	(nb_current(Var, _) ->
		atom_concat(SimPrefix, Var, VarName),
		term_string(NewVar, VarName),
		var_getvalue(Var, Value),
		nb_setval(NewVar, Value),
		swritef(CopyLog, 'Copy var \'%t\' into \'%t\' with val \'%t\'',
			[Var, NewVar, Value]),
		logtrace(CopyLog)
	;
		swritef(ErrorLog, 'No such var \'%t\'', [Var]),
		logerror(ErrorLog),
		halt
	).

copy_list(SimPrefix, ListPrefix, Elev) :-
	atom_concat(ListPrefix, Elev, VarName),
	term_string(Var, VarName),
	copy_var(SimPrefix, Var).

copy_elev_list(SimPrefix, Id) :-
	(Id =< 0 ->
		true
	;
		NextId is Id - 1,
		copy_list(SimPrefix, 'elev_rmap_', NextId),
		copy_list(SimPrefix, 'elev_people_', NextId),
		copy_elev_list(SimPrefix, NextId)
	).
