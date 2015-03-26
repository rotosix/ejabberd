%%%----------------------------------------------------------------------
%%% File    : mod_ai_robot.erl
%%% Author  : wugaoping<james.wu@asiainnovations.com>
%%%
%%%----------------------------------------------------------------------

-module(ai_api_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

-author('james.wu@asiainnovations.com').

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-record(state, {service, client}).

start_link(Args) ->
%% use poolboy
%%  gen_server:start_link(?MODULE, [Args], []).
%% use poolboy

%% use pooler
  gen_server:start_link(?MODULE, [Args], []).
%% use pooler

init([Service | _]) ->
  process_flag(trap_exit, true),
  {ok, Client} = thrift_client_util:new(ai_config:get_server_info(Service, host),
                                        ai_config:get_server_info(Service, port),
                                        ai_config:get_server_info(Service, service),
                                        []),
  error_logger:info_msg("james-thrift-open Client ~p, Service ~p", [Client, Service]),
  {ok, #state{service = Service, client=Client}}.

handle_call({Func, Args}, _From, #state{client=Client} = State) ->
  {Client1, {ok, Result}} = thrift_client:call(Client, Func, Args),
  Ret = case Result of
    undefined -> [];
    Result -> Result
  end,
  {reply, Ret, State#state{client=Client1}}.

handle_cast({Func, Args}, #state{client=Client} = State) ->
  {Client1, ok} = thrift_client:send_call(Client, Func, Args),
  {noreply, State#state{client=Client1}}.

%%handle_info(timeout, #state{service = Service, client=Client} = State) ->
%%  thrift_client:close(Client),
%%  {ok, Client1} = thrift_client_util:new(ai_config:get_server_info(Service, host),
%%    ai_config:get_server_info(Service, port),
%%    ai_config:get_server_info(Service, service),
%%    []),
%%  {noreply, State#state{client=Client1}};
%%  {reply, [], State#state{client=Client1}};
handle_info({'EXIT', _Pid, _Reason}, #state{client=Client} = State) ->
  thrift_client:close(Client),
  error_logger:info_msg("james-thrift-exit Client ~p", [Client]),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, #state{client=Client}) ->
  thrift_client:close(Client),
  error_logger:info_msg("james-thrift-terminate Client ~p", [Client]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
