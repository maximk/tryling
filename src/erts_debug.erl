-module(erts_debug).
-compile(export_all).

set_internal_state(_A, _B) ->
	%io:format("IGNORED: erts_debug:set_internal_state(~w, ~w)~n", [A,B]),
	ok.

get_internal_state({term_to_binary_no_funs,Term}) ->
	term_to_binary(Term);
get_internal_state(_) ->
	undefined.

%%EOF
