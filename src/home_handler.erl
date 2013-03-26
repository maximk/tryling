-module(home_handler).
-export([init/3,handle/2,terminate/2]).

init({_,http}, Req, _Opts) ->
	{ok,Req,none}.

handle(Req, State) ->

	{ok,Body} = demo_dtl:render([]),

    {ok,Req2} = cowboy_http_req:reply(200, [], Body, Req),
    {ok,Req2,State}.

terminate(_Req, _State) ->
    ok.

%%EOF
