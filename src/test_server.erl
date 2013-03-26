-module(test_server).
-compile(export_all).

%%
%% Stubs to make test suites run without modifications
%%

lookup_config(Key, Config) ->
	case lists:keyfind(Key, 1, Config) of
	{_,Val} ->
		Val;

	false ->
		io:format("Cannot find element ~w in Config.~n", [Key]),
		undefined
	end.

fail(Reason) ->
	erlang:error(Reason).

fail() ->
	erlang:error(unspecified).

to_int(N) when is_float(N) ->
	round(N);
to_int(N) ->
	N.

minutes(N) ->
	to_int(N * 60 * 1000).

seconds(N) ->
	to_int(N * 1000).

sleep(Millis) ->
	receive after Millis -> ok end.

timetrap(_Millis) -> dog.

timetrap_cancel(dog) -> ok.

do_times(0, _, _, _) ->
	ok;
do_times(N, M, F, A) when is_integer(N), N > 0 ->
	apply(M, F, A),
	do_times(N -1, M, F, A).

do_times(0, _) ->
	ok;
do_times(N, F) when is_integer(N), N > 0 ->
	F(),
	do_times(N -1, F).

timecall(M, F, A) ->
	{M1,N1,O1} = now(),
	Value = apply(M, F, A),
	{M2,N2,O2} = now(),
	Time = (float(M2) -M1) *1000000 + (float(N2) -N1) + (float(O2) -O1) /1000000,
	{Time,Value}.

format(Format) when is_list(Format) ->
   io:format(Format).
format(Format, Args) when is_list(Format) ->
	io:format(Format, Args);
format(Pri, Format) when is_integer(Pri) ->
	io:format(Format).
format(Pri, Format, Args) when is_integer(Pri) ->
	io:format(Format, Args).

temp_name(Prefix) ->
	Prefix ++ integer_to_list(random:uniform(100000)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% m_out_of_n(M,N,Fun) -> ok | exit({m_out_of_n_failed,{R,left_to_do}})
%% M = integer()
%% N = integer()
%% Fun = fun() -> void()
%% R = integer()
%%
%% Repeats evaluating the given function until it succeeded (didn't crash)
%% M times. If, after N times, M successful attempts have not been
%% accomplished, the process crashes with reason {m_out_of_n_failed
%% {R,left_to_do}}, where R indicates how many cases that remained to be
%% successfully completed.
%%
%% For example:
%% m_out_of_n(1,4,fun() -> tricky_test_case() end)
%%                           Tries to run tricky_test_case() up to 4 times,
%%                           and is happy if it succeeds once.
%%
%% m_out_of_n(7,8,fun() -> clock_sanity_check() end)
%%                         Tries running clock_sanity_check() up to 8
%%                         times and allows the function to fail once.
%%                         This might be useful if clock_sanity_check/0
%%                         is known to fail if the clock crosses an hour
%%                         boundary during the test (and the up to 8
%%                         test runs could never cross 2 boundaries)
m_out_of_n(0,_,_) ->
    ok;
m_out_of_n(M,0,_) ->
    exit({m_out_of_n_failed,{M,left_to_do}});
m_out_of_n(M,N,Fun) ->
    case catch Fun() of
	{'EXIT',_} ->
	    m_out_of_n(M,N-1,Fun);
	_Other ->
	    m_out_of_n(M-1,N-1,Fun)
    end.

%%EOF
