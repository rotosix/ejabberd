
-module(ai_api_thrift).
-author('james.wu@asiainnovations.com').

%% ====================================================================
%% API functions
%% ====================================================================
-export([call/3, cast/3, start_cast/3]).

-spec call(Service::atom(), Func::any(), Args::list(any())) -> any().
call(Service, Func, Args) ->
%% use poolboy
%%  poolboy:transaction(Service, fun(Worker) ->
%%    gen_server:call(Worker, {Func, Args})
%%  end).
%% use poolboy

%% use pooler
%%  Worker = pooler:take_member(Service),
%%  try
%%    gen_server:call(Worker, {Func, Args})
%%  after
%%    ok = pooler:return_member(Service, Worker, ok)
%%  end.
%% use pooler

  {ok, Client} = thrift_client_util:new(ai_config:get_server_info(Service, host),
                                        ai_config:get_server_info(Service, port),
                                        ai_config:get_server_info(Service, service),
                                        []),
  {Client1, {ok, Result}} = thrift_client:call(Client, Func, Args),
  Ret = case Result of
          undefined -> [];
          Result -> Result
        end,
  thrift_client:close(Client1),
  Ret.

%% 启动一个临时进程来处理，避免阻塞主业务
cast(Service, Func, Args) ->
  spawn(?MODULE, start_cast, [Service, Func, Args]).

start_cast(Service, Func, Args) ->
%% use poolboy
%%  poolboy:transaction(Service, fun(Worker) ->
%%    gen_server:call(Worker, {Func, Args})
%%  end),
%%  ok.
%% use poolboy

%% use pooler
%%  Worker = pooler:take_member(Service),
%%  try
%%    gen_server:call(Worker, {Func, Args})
%%  after
%%    ok = pooler:return_member(Service, Worker, ok)
%%  end.
%% use pooler

  {ok, Client} = thrift_client_util:new(ai_config:get_server_info(Service, host),
                                        ai_config:get_server_info(Service, port),
                                        ai_config:get_server_info(Service, service),
                                        []),
  {Client1, ok} = thrift_client:send_call(Client, Func, Args),
  thrift_client:close(Client1).
