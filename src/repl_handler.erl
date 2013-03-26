-module(repl_handler).
-export([init/3]).
-export([websocket_init/3,websocket_handle/3,
    websocket_info/3,websocket_terminate/3]).

-record(ws, {no = 1,
			 binds = [],
		 	 io}).

init({_,http}, _Req, _Opts) ->
    {upgrade,protocol,cowboy_http_websocket}.

websocket_init(_TransName, Req, _Opts) ->
	Self = self(),
	Io = spawn(fun() -> io_loop(Self) end),
	group_leader(Io, self()),
    erlang:start_timer(100, self(), prompt),
	{ok,Req,#ws{io=Io}}.

websocket_handle({text,<<>>}, Req,St) ->
	Msg = io_lib:format("\n~w> ", [St#ws.no]),
	{reply,{text,Msg},Req,St};
websocket_handle({text,BinExpr}, Req, #ws{binds=Binds}=St) ->
	Expr = binary_to_list(BinExpr),
	Ws = self(),
	spawn(fun() ->
		Ws ! {stdout,"\n"},
		case catch eval_expr(Expr, Binds) of
		{'EXIT',Reason} ->
			Chars = io_lib:format("*** ~p\n", [Reason]),
			Ws ! {stdout,Chars},
			Ws ! {done,self(),Binds};
		{value,Val,NewBinds} ->
			Chars = io_lib:format("~p\n", [Val]),
			Ws ! {stdout,Chars},
			Ws ! {done,NewBinds}
		end
	end),
	{ok,Req,St};
websocket_handle(_Data, Req, St) ->
    {ok,Req,St}.

websocket_info({timeout,_Ref,prompt},Req,St) ->
	Prompt = io_lib:format("~w> ", [St#ws.no]),
	{reply,{text,Prompt},Req,St};
websocket_info({stdout,Msg}, Req, St) ->
	{reply,{text,Msg},Req,St};
websocket_info({done,NewBinds}, Req, St) ->
	Prompt = io_lib:format("~w> ", [St#ws.no+1]),
	{reply,{text,Prompt},Req,St#ws{no=St#ws.no+1,binds=NewBinds}};
websocket_info(_Info, Req, St) ->
    {ok,Req,St}.

websocket_terminate(_Reason, _Req, St) ->
	exit(St#ws.io, normal),
    ok.

eval_expr(Expr, Binds) ->
	{ok,Toks,_} = erl_scan:string(Expr),
	{ok,Forms} = erl_parse:parse_exprs(Toks),
	erl_eval:exprs(Forms, Binds).

io_loop(Ws) ->
	receive
	{io_request,From,ReplyAs,Req} ->
		case io_req(Req) of
		{output,Chars} ->
			Ws ! {stdout,Chars},
			From ! {io_reply,ReplyAs,ok}
		end,
		io_loop(Ws)
	end.

io_req({put_chars,_,Chars}) ->
	{output,Chars};
io_req({put_chars,_,M,F,As}) ->
	{output,apply(M, F, As)}.

%%EOF
