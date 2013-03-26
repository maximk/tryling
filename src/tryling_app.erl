-module(tryling_app).

-behaviour(application).

%% Application callbacks
-export([start/0]).
-export([start/2,stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
	%%reloader:start(),

	application:start(cowboy),

	application:start(tryling).

start(_StartType, _StartArgs) ->

	MimeTypes = [{<<".css">>,[<<"text/css">>]},
				 {<<".png">>,[<<"image/png">>]},
				 {<<".js">>,[<<"application/javascript">>]}],

	Dispatch = [
		{'_',[{[],home_handler,[]},
			  {[<<"repl">>],repl_handler,[]},
			  {[<<"stats">>],stats_handler,[]},
			  {[<<"css">>,'...'],
				  cowboy_http_static,[{directory,"priv/www/css"},
				  					  {mimetypes,MimeTypes}]},
			  {[<<"js">>,'...'],
				  cowboy_http_static,[{directory,"priv/www/js"},
				  					  {mimetypes,MimeTypes}]},
			  {[<<"images">>,'...'],
				  cowboy_http_static,[{directory,"priv/www/images"},
				  					  {mimetypes,MimeTypes}]}]}
	],

	_CertFile = "priv/keys/cert.pem",
	_KeyFile = "priv/keys/key.pem",

	{ok,_} = cowboy:start_listener(www, 8,
		cowboy_tcp_transport, [{port,80}],
		cowboy_http_protocol, [{dispatch,Dispatch}]
	),

	%%{ok,_} = cowboy:start_listener(ssl, 8,
	%%	cowboy_ssl_transport, [{port,443},
	%%						   {certfile,CertFile},
	%%					   	   {keyfile,KeyFile}],
	%%	cowboy_http_protocol, [{dispatch,Dispatch}]),

    tryling_sup:start_link().

stop(_State) ->
    ok.

%%EOF
