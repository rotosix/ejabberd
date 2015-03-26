
-module(ai_api_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
%%  {ok, Pools} = application:get_env(ai_api, pools),

%% use poolboy
%%  PoolSpecs = lists:map(fun({Name, Args}) ->
%%    PoolArgs = [{name, {local, Name}}] ++ Args,
%%    poolboy:child_spec(Name, PoolArgs, [Name])
%%  end,
%%    Pools),
%%  {ok, {{one_for_one, 10000, 10}, PoolSpecs}}.
%% use poolboy

%% use pooler
%%  lists:foreach(fun(PoolConfig)->
%%    pooler:new_pool(PoolConfig)
%%  end,
%%  Pools),
%% use pooler

%% don't use pool
  {ok, { {one_for_one, 5, 10}, []} }.
