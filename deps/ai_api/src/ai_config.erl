%% @author james
%% @doc @todo Add description to ai_config.


-module(ai_config).
-author('james.wu@asiainnovations.com').

-include("ai_config.hrl").
%%-compile(export_all).

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_server_info/2]).

-spec get_server_info(Service::atom(), Param::atom()) -> any().
get_server_info(service, host) ->
	?SERVICE_SERVER_HOST;
get_server_info(service, port) ->
	?SERVICE_SERVER_PORT;
get_server_info(service, service) ->
	?SERVICE_SERVER_SERVICE;
get_server_info(mongo, host) ->
	?MONGO_SERVER_HOST;
get_server_info(mongo, port) ->
	?MONGO_SERVER_PORT;
get_server_info(mongo, service) ->
	?MONGO_SERVER_SERVICE;
get_server_info(apns, host) ->
	?APNS_SERVER_HOST;
get_server_info(apns, port) ->
	?APNS_SERVER_PORT;
get_server_info(apns, service) ->
	?APNS_SERVER_SERVICE;
get_server_info(router, host) ->
	?ROUTER_SERVER_HOST;
get_server_info(router, port) ->
	?ROUTER_SERVER_PORT;
get_server_info(router, service) ->
	?ROUTER_SERVER_SERVICE;
get_server_info(robot, host) ->
	?ROBOT_SERVER_HOST;
get_server_info(robot, port) ->
	?ROBOT_SERVER_PORT;
get_server_info(robot, service) ->
	?ROBOT_SERVER_SERVICE.
%% ====================================================================
%% Internal functions
%% ====================================================================


