%% @author Mochi Media <dev@mochimedia.com>
%% @copyright demo Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the demo application.

-module(demo_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for demo.
start(_Type, _StartArgs) ->
    demo_deps:ensure(),
    {ok, Sup} = demo_sup:start_link(),
	demo_web:start(),
	{ok, Sup}.

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for demo.
stop(_State) ->
    ok.
