
-module(ai_proxy_config).
-author('james.wu@asiainnovations.com').

-include("ai_proxy_config.hrl").
%%-compile(export_all).

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_server_info/2]).

-spec get_server_info(Service::atom(), Param::atom()) -> any().
get_server_info(proxy, port) ->
	?AI_PROXY_PORT.

%% ====================================================================
%% Internal functions
%% ====================================================================


