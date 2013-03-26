-module(stats_handler).
-export([init/3,handle/2,terminate/2]).

init({_,http}, Req, _Opts) ->
	{ok,Req,none}.

handle(Req, State) ->

	{ContextSwitches,_} = erlang:statistics(context_switches),
	{Reductions,_} = erlang:statistics(reductions),
	{GcRuns,WordsReclaimed,_} = erlang:statistics(garbage_collection),
	{{input,Input},{output,Output}} = erlang:statistics(io),
	RunQueue = erlang:statistics(run_queue),
	{Runtime,_} = erlang:statistics(runtime),
	{WallClock,_} = erlang:statistics(wall_clock),
	MemoryProcesses = erlang:memory(processes),
	MemorySystem = erlang:memory(system),
	MemoryETS = erlang:memory(ets),

	Body = io_lib:format("{"
		"\"context-switches\": ~w,"
		"\"reductions\": ~w,"
		"\"gc-runs\": ~w,"
		"\"words-reclaimed\": ~w,"
		"\"io-input\": ~w,"
		"\"io-output\": ~w,"
		"\"run-queue\": ~w,"
		"\"runtime\": ~w,"
		"\"wall-clock\": ~w,"
		"\"memory-processes\": ~w,"
		"\"memory-system\": ~w,"
		"\"memory-ets\": ~w}",
			[ContextSwitches,
			 Reductions,
			 GcRuns,
			 WordsReclaimed,
			 Input,
			 Output,
			 RunQueue,
			 Runtime,
			 WallClock,
			 MemoryProcesses,
			 MemorySystem,
			 MemoryETS]),

	Hdr = {<<"Content-Type">>,<<"application/json">>},
    {ok,Req2} = cowboy_http_req:reply(200, [Hdr], Body, Req),
    {ok,Req2,State}.

terminate(_Req, _State) ->
    ok.

%%EOF
