-module(ai_proxy_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  ai_proxy_sup:start_link(),
	ai_proxy_server:start().

stop(_State) ->
	ai_proxy_server:stop(),
  ok.
