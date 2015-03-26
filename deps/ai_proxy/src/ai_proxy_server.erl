%%%----------------------------------------------------------------------
%%% File    : mod_ai_robot.erl
%%% Author  : wugaoping<james.wu@asiainnovations.com>
%%%
%%%----------------------------------------------------------------------

-module(ai_proxy_server).

-author('james.wu@asiainnovations.com').

%% proxy controller
-export([start/0, stop/0]).

%% common server api
-export([handle_function/2, handle_error/2]).

%% proxy api
-export([sendMessage/2, sendBroadcast/2, sendIQ/2, sendKickoff/1]).

-define(AI_PROXY_SERVER, ?MODULE).

start() ->
    thrift_socket_server:start([{handler, ?MODULE},  
                                {service, robotRemoteService_thrift},  
                                {port, ai_proxy_config:get_server_info(proxy, port)},  
                                {name, ?AI_PROXY_SERVER},
								{socket_opts, [{recv_timeout, infinite}]}]).

stop() ->
    thrift_socket_server:stop(?AI_PROXY_SERVER).

%--------------------------------------------------------------------------

handle_function(Function, Args) when is_atom(Function), is_tuple(Args) ->  
    case apply(?MODULE, Function, tuple_to_list(Args)) of
        ok -> ok;  
        Reply -> {reply, Reply}  
    end.

handle_error(_, _) ->
	ok.

%--------------------------------------------------------------------------

-spec sendMessage(SenderId::string() | binary(), Data::string() | binary()) -> any().
sendMessage(SenderId, Data) ->
	mod_ai_robot:sendMessage(SenderId, Data).

-spec sendBroadcast(SenderId::string() | binary(), Data::string() | binary()) -> any().
sendBroadcast(SenderId, Data) ->
	mod_ai_robot:sendBroadcast(SenderId, Data).

-spec sendIQ(SenderId::string() | binary(), Data::string() | binary()) -> any().
sendIQ(SenderId, Data) ->
	mod_ai_robot:sendIQ(SenderId, Data).

-spec sendKickoff(Uid::string() | binary()) -> any().
sendKickoff(Uid) ->
  mod_ai_safe:kickoff_user(Uid).
