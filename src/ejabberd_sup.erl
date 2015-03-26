%%%----------------------------------------------------------------------
%%% File    : ejabberd_sup.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Erlang/OTP supervisor
%%% Created : 31 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2014   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License along
%%% with this program; if not, write to the Free Software Foundation, Inc.,
%%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_sup).
-author('alexey@process-one.net').

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Hooks =
	{ejabberd_hooks,
	 {ejabberd_hooks, start_link, []},
	 permanent,
	 brutal_kill,
	 worker,
	 [ejabberd_hooks]},
    NodeGroups =
	{ejabberd_node_groups,
	 {ejabberd_node_groups, start_link, []},
	 permanent,
	 brutal_kill,
	 worker,
	 [ejabberd_node_groups]},
    SystemMonitor =
	{ejabberd_system_monitor,
	 {ejabberd_system_monitor, start_link, []},
	 permanent,
	 brutal_kill,
	 worker,
	 [ejabberd_system_monitor]},
    Router =
	{ejabberd_router,
	 {ejabberd_router, start_link, []},
	 permanent,
	 brutal_kill,
	 worker,
	 [ejabberd_router]},
    SM =
	{ejabberd_sm,
	 {ejabberd_sm, start_link, []},
	 permanent,
	 brutal_kill,
	 worker,
	 [ejabberd_sm]},
    S2S =
	{ejabberd_s2s,
	 {ejabberd_s2s, start_link, []},
	 permanent,
	 brutal_kill,
	 worker,
	 [ejabberd_s2s]},
    Local =
	{ejabberd_local,
	 {ejabberd_local, start_link, []},
	 permanent,
	 brutal_kill,
	 worker,
	 [ejabberd_local]},
    Captcha =
	{ejabberd_captcha,
	 {ejabberd_captcha, start_link, []},
	 permanent,
	 brutal_kill,
	 worker,
	 [ejabberd_captcha]},
  %% begin add by james.wu@asiainnovations.com 2015-03-18
    AiResend =
    {mod_ai_resend,
      {mod_ai_resend, start_link, []},
      permanent,
      brutal_kill,
      worker,
      [mod_ai_resend]},
  %% end add by james.wu@asiainnovations.com 2015-03-18
    Listener =
	{ejabberd_listener,
	 {ejabberd_listener, start_link, []},
	 permanent,
	 infinity,
	 supervisor,
	 [ejabberd_listener]},
    ReceiverSupervisor =
	{ejabberd_receiver_sup,
	 {ejabberd_tmp_sup, start_link,
	  [ejabberd_receiver_sup, ejabberd_receiver]},
	 permanent,
	 infinity,
	 supervisor,
	 [ejabberd_tmp_sup]},
    C2SSupervisor =
	{ejabberd_c2s_sup,
	 {ejabberd_tmp_sup, start_link, [ejabberd_c2s_sup, ejabberd_c2s]},
	 permanent,
	 infinity,
	 supervisor,
	 [ejabberd_tmp_sup]},
    S2SInSupervisor =
	{ejabberd_s2s_in_sup,
	 {ejabberd_tmp_sup, start_link,
	  [ejabberd_s2s_in_sup, ejabberd_s2s_in]},
	 permanent,
	 infinity,
	 supervisor,
	 [ejabberd_tmp_sup]},
    S2SOutSupervisor =
	{ejabberd_s2s_out_sup,
	 {ejabberd_tmp_sup, start_link,
	  [ejabberd_s2s_out_sup, ejabberd_s2s_out]},
	 permanent,
	 infinity,
	 supervisor,
	 [ejabberd_tmp_sup]},
    ServiceSupervisor =
	{ejabberd_service_sup,
	 {ejabberd_tmp_sup, start_link,
	  [ejabberd_service_sup, ejabberd_service]},
	 permanent,
	 infinity,
	 supervisor,
	 [ejabberd_tmp_sup]},
    HTTPSupervisor =
	{ejabberd_http_sup,
	 {ejabberd_tmp_sup, start_link,
	  [ejabberd_http_sup, ejabberd_http]},
	 permanent,
	 infinity,
	 supervisor,
	 [ejabberd_tmp_sup]},
    HTTPPollSupervisor =
	{ejabberd_http_poll_sup,
	 {ejabberd_tmp_sup, start_link,
	  [ejabberd_http_poll_sup, ejabberd_http_poll]},
	 permanent,
	 infinity,
	 supervisor,
	 [ejabberd_tmp_sup]},
    FrontendSocketSupervisor =
	{ejabberd_frontend_socket_sup,
	 {ejabberd_tmp_sup, start_link,
	  [ejabberd_frontend_socket_sup, ejabberd_frontend_socket]},
	 permanent,
	 infinity,
	 supervisor,
	 [ejabberd_tmp_sup]},
    IQSupervisor =
	{ejabberd_iq_sup,
	 {ejabberd_tmp_sup, start_link,
	  [ejabberd_iq_sup, gen_iq_handler]},
	 permanent,
	 infinity,
	 supervisor,
	 [ejabberd_tmp_sup]},
  %% begin add by james.wu@asiainnovations.com 2015-03-18
  %%{ok, {{one_for_one, 10, 1},
  {ok, {{one_for_one, 10000, 10},
    %% end add by james.wu@asiainnovations.com 2015-03-18
    [Hooks,
      NodeGroups,
      SystemMonitor,
      Router,
      SM,
      S2S,
      Local,
      Captcha,
      %% begin add by james.wu@asiainnovations.com 2015-03-18
      AiResend,
      %% end add by james.wu@asiainnovations.com 2015-03-18
	   ReceiverSupervisor,
	   C2SSupervisor,
	   S2SInSupervisor,
	   S2SOutSupervisor,
	   ServiceSupervisor,
	   HTTPSupervisor,
	   HTTPPollSupervisor,
	   IQSupervisor,
	   FrontendSocketSupervisor,
	   Listener]}}.


