%% @author james
%% @doc @todo Add description to ai_thrift.

-module(ai_thrift).
-author('james.wu@asiainnovations.com').

-define(SERVER, "localhost").
-define(PORT, 9090).
-define(SERVICE, thriftService_thrift).

%% ====================================================================
%% API functions
%% ====================================================================
-export([connect/0, call/3, close/1]).

connect() ->
	{ok, Client} = thrift_client_util:new(?SERVER, ?PORT, ?SERVICE, []),
	Client.

call(Client, Func, Args) ->
	{_Client1, {ok, Result}} = thrift_client:call(Client, Func,  Args),
	Result.

close(Client) ->
	{_Client1, ok} = thrift_client:close(Client),
	ok.

%% ====================================================================
%% Internal functions
%% ====================================================================


