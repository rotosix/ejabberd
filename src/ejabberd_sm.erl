%%%----------------------------------------------------------------------
%%% File    : ejabberd_sm.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Session manager
%%% Created : 24 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
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

-module(ejabberd_sm).

-author('alexey@process-one.net').

-behaviour(gen_server).

%% API
-export([start_link/0,
	 route/3,
	 open_session/5,
	 open_session/6,
	 close_session/1,
	 close_session/4,
	 check_in_subscription/6,
	 bounce_offline_message/3,
	 disconnect_removed_user/2,
	 get_user_resources/2,
	 get_user_present_resources/2,
	 get_user_jids/1,
	 set_presence/7,
	 unset_presence/6,
	 close_session_unset_presence/5,
	 dirty_get_sessions_list/0,
	 dirty_get_my_sessions_list/0,
	 get_vh_session_list/1,
	 get_vh_session_number/1,
	 register_iq_handler/4,
	 register_iq_handler/5,
	 unregister_iq_handler/2,
	 force_update_presence/1,
	 connected_users/0,
	 connected_users_number/0,
	 user_resources/2,
	 disconnect_user/2,
	 get_session_pid/3,
	 get_user_info/3,
	 get_user_ip/3,
	 get_max_user_sessions/2,
	 get_all_pids/0,
	 is_existing_resource/3
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
	 handle_info/2, terminate/2, code_change/3]).

-include("ejabberd.hrl").
-include("logger.hrl").

-include("jlib.hrl").

-include("ejabberd_commands.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include("mod_privacy.hrl").

%% begin modify by james.wu@asiainnovations.com 2014-10-30 PM 4:21
-record(session, {sid, usr, us, uid, priority, info}).
%% end modify by james.wu@asiainnovations.com 2014-10-30 PM 4:21

-record(session_counter, {vhost, count}).
-record(state, {}).

%% default value for the maximum number of user connections
-define(MAX_USER_SESSIONS, infinity).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
-type sid() :: {erlang:timestamp(), pid()}.
-type ip() :: {inet:ip_address(), inet:port_number()} | undefined.
-type info() :: [{conn, atom()} | {ip, ip()} | {node, atom()}
                 | {oor, boolean()} | {auth_module, atom()}].
-type prio() :: undefined | integer().

-export_type([sid/0]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [],
			  []).

-spec route(jid(), jid(), xmlel() | broadcast()) -> ok.

route(From, To, Packet) ->
    case catch do_route(From, To, Packet) of
      {'EXIT', Reason} ->
	  ?ERROR_MSG("~p~nwhen processing: ~p",
		     [Reason, {From, To, Packet}]);
      _ -> ok
    end.

-spec open_session(sid(), binary(), binary(), binary(), prio(), info()) -> ok.

open_session(SID, User, Server, Resource, Priority, Info) ->
    check_for_sessions_to_replace(User, Server, Resource),
    set_session(SID, User, Server, Resource, Priority, Info),
    mnesia:dirty_update_counter(session_counter,
				jlib:nameprep(Server), 1),
    %%check_for_sessions_to_replace(User, Server, Resource),
    JID = jlib:make_jid(User, Server, Resource),
    ejabberd_hooks:run(sm_register_connection_hook,
		       JID#jid.lserver, [SID, JID, Info]).

-spec open_session(sid(), binary(), binary(), binary(), info()) -> ok.

open_session(SID, User, Server, Resource, Info) ->
    open_session(SID, User, Server, Resource, undefined, Info).

-spec close_session(sid(), binary(), binary(), binary()) -> ok.

close_session(SID, User, Server, Resource) ->
    Info = case mnesia:dirty_read({session, SID}) of
	[] -> [];
	[#session{info=I}] -> I
    end,
    F = fun() ->
		mnesia:delete({session, SID}),
		mnesia:dirty_update_counter(session_counter,
					    jlib:nameprep(Server), -1)
	end,
    mnesia:sync_dirty(F),
    JID = jlib:make_jid(User, Server, Resource),
    ejabberd_hooks:run(sm_remove_connection_hook,
		       JID#jid.lserver, [SID, JID, Info]).

%% begin add by james.wu@asiainnovations.com 2015-03-13
close_session(Uid) ->
  case catch mnesia:dirty_index_read(session, {Uid}, #session.uid) of
    [] -> ok;
    Ss ->
      lists:foreach(fun(#session{sid = SID, usr = {User, Server, Resource}, info = Info}) ->
        F = fun() ->
          mnesia:delete({session, SID}),
          mnesia:dirty_update_counter(session_counter,
            jlib:nameprep(Server), -1)
        end,
        mnesia:sync_dirty(F),
        JID = jlib:make_jid(User, Server, Resource),
        ejabberd_hooks:run(sm_remove_connection_hook,
          JID#jid.lserver, [SID, JID, Info]),
        Sockmod = proplists:get_value(sockmod, Info),
        Socket = proplists:get_value(socket, Info),
        Sockmod:close(Socket)
        end,
        Ss)
  end,
  ok.
%% end add by james.wu@asiainnovations.com 2015-03-13

check_in_subscription(Acc, User, Server, _JID, _Type, _Reason) ->
    case ejabberd_auth:is_user_exists(User, Server) of
      true -> Acc;
      false -> {stop, false}
    end.

-spec bounce_offline_message(jid(), jid(), xmlel()) -> stop.

%% begin modify by james.wu@asiainnovations.com 2014-10-30 AM 10:56
bounce_offline_message(_From, _To, _Packet) ->
    %%Err = jlib:make_error_reply(Packet,
	%%			?ERR_SERVICE_UNAVAILABLE),
    %%ejabberd_router:route(To, From, Err),
%% end modify by james.wu@asiainnovations.com 2014-10-30 AM 10:56
    stop.

-spec disconnect_removed_user(binary(), binary()) -> ok.

disconnect_removed_user(User, Server) ->
    ejabberd_sm:route(jlib:make_jid(<<"">>, <<"">>, <<"">>),
		      jlib:make_jid(User, Server, <<"">>),
                      {broadcast, {exit, <<"User removed">>}}).

%% begin add by james.wu@asiainnovations.com 2014-10-31 PM 02:16
get_user_resources(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    case catch mnesia:dirty_index_read(session, US, #session.us) of
	{'EXIT', _Reason} ->
	    [];
	Ss ->
	    R1 = [element(3, S#session.usr) || S <- clean_session_list(Ss)],
		case R1 of
			[] ->
				case catch mnesia:dirty_index_read(session, {LUser}, #session.uid) of
				{'EXIT', _Reason1} -> [];
				Ss1 ->
				    [element(3, S#session.usr) || S <- clean_session_list(Ss1)]
			    end;
			R -> R
		end
    end.
%% end add by james.wu@asiainnovations.com 2014-10-31 PM 02:16

-spec get_user_present_resources(binary(), binary()) -> [tuple()].

%% begin add by james.wu@asiainnovations.com 2014-10-31 PM 02:16
get_user_present_resources(LUser, LServer) ->
    US = {LUser, LServer},
    case catch mnesia:dirty_index_read(session, US,
				       #session.us)
	of
      {'EXIT', _Reason} -> [];
      Ss ->
	  PR1 = [{S#session.priority, element(3, S#session.usr)} || S <- clean_session_list(Ss), is_integer(S#session.priority)],
		case PR1 of
			[] -> 
				case catch mnesia:dirty_index_read(session, {LUser}, #session.uid) of
				{'EXIT', _Reason1} -> [];
				Ss1 ->
				     [{S#session.priority, element(3, S#session.usr)} || S <- clean_session_list(Ss1), is_integer(S#session.priority)]
			    end;
			PR2 -> PR2
		end
	end.
%% end add by james.wu@asiainnovations.com 2014-10-31 PM 02:16

get_user_jids(User) ->
    LUser = jlib:nodeprep(User),
	case catch mnesia:dirty_index_read(session, {LUser}, #session.uid) of
	{'EXIT', _Reason} ->
	    [];
	Ss ->
	    [S#session.usr || S <- clean_session_list(Ss)]
    end.

-spec get_user_ip(binary(), binary(), binary()) -> ip().

%% begin add by james.wu@asiainnovations.com 2014-10-31 PM 02:16
get_user_ip(User, Server, Resource) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    LResource = jlib:resourceprep(Resource),
    USR = {LUser, LServer, LResource},
    case mnesia:dirty_index_read(session, USR, #session.usr) of
	[] ->
		case catch mnesia:dirty_index_read(session, {LUser}, #session.uid) of
		[] -> undefined;
		Ss1 ->
		    Session = lists:max(Ss1),
		    proplists:get_value(ip, Session#session.info)
	    end;
	Ss ->
	    Session = lists:max(Ss),
	    proplists:get_value(ip, Session#session.info)
    end.

-spec get_user_info(binary(), binary(), binary()) -> info() | offline.

get_user_info(User, Server, Resource) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    LResource = jlib:resourceprep(Resource),
    USR = {LUser, LServer, LResource},
    case mnesia:dirty_index_read(session, USR, #session.usr) of
	[] ->
		case catch mnesia:dirty_index_read(session, {LUser}, #session.uid) of
		[] -> offline;
		Ss1 ->
		    Session = lists:max(Ss1),
		    Node = node(element(2, Session#session.sid)),
		    Conn = proplists:get_value(conn, Session#session.info),
		    IP = proplists:get_value(ip, Session#session.info),
		    [{node, Node}, {conn, Conn}, {ip, IP}]
	    end;
	Ss ->
	    Session = lists:max(Ss),
	    Node = node(element(2, Session#session.sid)),
	    Conn = proplists:get_value(conn, Session#session.info),
	    IP = proplists:get_value(ip, Session#session.info),
	    [{node, Node}, {conn, Conn}, {ip, IP}]
    end.
%% end add by james.wu@asiainnovations.com 2014-10-31 PM 02:16

-spec set_presence(sid(), binary(), binary(), binary(),
                   prio(), xmlel(), info()) -> ok.

set_presence(SID, User, Server, Resource, Priority,
	     Presence, Info) ->
    set_session(SID, User, Server, Resource, Priority,
		Info),
    ejabberd_hooks:run(set_presence_hook,
		       jlib:nameprep(Server),
		       [User, Server, Resource, Presence]).

-spec unset_presence(sid(), binary(), binary(),
                     binary(), binary(), info()) -> ok.

unset_presence(SID, User, Server, Resource, Status,
	       Info) ->
    set_session(SID, User, Server, Resource, undefined,
		Info),
    ejabberd_hooks:run(unset_presence_hook,
		       jlib:nameprep(Server),
		       [User, Server, Resource, Status]).

-spec close_session_unset_presence(sid(), binary(), binary(),
                                   binary(), binary()) -> ok.

close_session_unset_presence(SID, User, Server,
			     Resource, Status) ->
    close_session(SID, User, Server, Resource),
    ejabberd_hooks:run(unset_presence_hook,
		       jlib:nameprep(Server),
		       [User, Server, Resource, Status]).

-spec get_session_pid(binary(), binary(), binary()) -> none | pid().

get_session_pid(User, _Server, _Resource) ->
    LUser = jlib:nodeprep(User),
%%    LServer = jlib:nameprep(Server),
%%    LResource = jlib:resourceprep(Resource),
%%    USR = {LUser, LServer, LResource},
%%    case catch mnesia:dirty_index_read(session, USR, #session.usr) of
%%	[#session{sid = {_, Pid}}] -> Pid;
%%	_ ->
		case catch mnesia:dirty_index_read(session, {LUser}, #session.uid) of
		[#session{sid = {_, Pid}} | _] -> Pid;
		_ -> none
%%	    end
	end.

-spec dirty_get_sessions_list() -> [ljid()].

dirty_get_sessions_list() ->
    mnesia:dirty_select(
      session,
      [{#session{usr = '$1', _ = '_'},
	[],
	['$1']}]).

dirty_get_my_sessions_list() ->
    mnesia:dirty_select(
      session,
      [{#session{sid = {'_', '$1'}, _ = '_'},
	[{'==', {node, '$1'}, node()}],
	['$_']}]).

-spec get_vh_session_list(binary()) -> [ljid()].

get_vh_session_list(Server) ->
    LServer = jlib:nameprep(Server),
    mnesia:dirty_select(session,
			[{#session{usr = '$1', _ = '_'},
			  [{'==', {element, 2, '$1'}, LServer}], ['$1']}]).

-spec get_all_pids() -> [pid()].

get_all_pids() ->
    mnesia:dirty_select(
      session,
      ets:fun2ms(
        fun(#session{sid = {_, Pid}}) ->
		Pid
        end)).

get_vh_session_number(Server) ->
    LServer = jlib:nameprep(Server),
    Query = mnesia:dirty_select(
		session_counter,
		[{#session_counter{vhost = LServer, count = '$1'},
		  [],
		  ['$1']}]),
    case Query of
	[Count] ->
	    Count;
	_ -> 0
    end.

register_iq_handler(Host, XMLNS, Module, Fun) ->
    ejabberd_sm !
      {register_iq_handler, Host, XMLNS, Module, Fun}.

-spec register_iq_handler(binary(), binary(), atom(), atom(), list()) -> any().

register_iq_handler(Host, XMLNS, Module, Fun, Opts) ->
    ejabberd_sm !
      {register_iq_handler, Host, XMLNS, Module, Fun, Opts}.

-spec unregister_iq_handler(binary(), binary()) -> any().

unregister_iq_handler(Host, XMLNS) ->
    ejabberd_sm ! {unregister_iq_handler, Host, XMLNS}.


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    update_tables(),
    mnesia:create_table(session,
			[{ram_copies, [node()]},
			 {attributes, record_info(fields, session)}]),
    mnesia:create_table(session_counter,
			[{ram_copies, [node()]},
			 {attributes, record_info(fields, session_counter)}]),
    mnesia:add_table_index(session, usr),
    mnesia:add_table_index(session, us),
%% begin add by james.wu@asiainnovations.com 2014-10-31 PM 1:49
    mnesia:add_table_index(session, uid),
%% end add by james.wu@asiainnovations.com 2014-10-31 PM 1:49
    mnesia:add_table_copy(session, node(), ram_copies),
    mnesia:add_table_copy(session_counter, node(), ram_copies),
    mnesia:subscribe(system),
    ets:new(sm_iqtable, [named_table]),
    lists:foreach(
      fun(Host) ->
	      ejabberd_hooks:add(roster_in_subscription, Host,
				 ejabberd_sm, check_in_subscription, 20),
	      ejabberd_hooks:add(offline_message_hook, Host,
				 ejabberd_sm, bounce_offline_message, 100),
	      ejabberd_hooks:add(remove_user, Host,
				 ejabberd_sm, disconnect_removed_user, 100)
      end, ?MYHOSTS),
    ejabberd_commands:register_commands(commands()),

    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok, {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) -> {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({route, From, To, Packet}, State) ->
    case catch do_route(From, To, Packet) of
	{'EXIT', Reason} ->
	    ?ERROR_MSG("~p~nwhen processing: ~p",
		       [Reason, {From, To, Packet}]);
	_ ->
	    ok
    end,
    {noreply, State};
handle_info({mnesia_system_event, {mnesia_down, Node}}, State) ->
    recount_session_table(Node),
    {noreply, State};
handle_info({register_iq_handler, Host, XMLNS, Module, Function}, State) ->
    ets:insert(sm_iqtable, {{XMLNS, Host}, Module, Function}),
    {noreply, State};
handle_info({register_iq_handler, Host, XMLNS, Module,
	     Function, Opts},
	    State) ->
    ets:insert(sm_iqtable,
	       {{XMLNS, Host}, Module, Function, Opts}),
    {noreply, State};
handle_info({unregister_iq_handler, Host, XMLNS},
	    State) ->
    case ets:lookup(sm_iqtable, {XMLNS, Host}) of
      [{_, Module, Function, Opts}] ->
	  gen_iq_handler:stop_iq_handler(Module, Function, Opts);
      _ -> ok
    end,
    ets:delete(sm_iqtable, {XMLNS, Host}),
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ejabberd_commands:unregister_commands(commands()),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% begin modify by james.wu@asiainnovations.com 2014-10-30 PM 4:27
set_session(SID, User, Server, Resource, Priority, Info) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    LResource = jlib:resourceprep(Resource),
    US = {LUser, LServer},
    USR = {LUser, LServer, LResource},
    F = fun () ->
		?INFO_MSG("Total Users:~p, Cluster Nodes:~p, SID:~p, USR:~p, Priority:~p, Info:~p", [connected_users_number()+1, [node()| nodes()], SID, USR, Priority, Info]),
		mnesia:write(#session{sid = SID, usr = USR, us = US,
				      uid = {LUser}, priority = Priority, info = Info})
	end,
    mnesia:sync_dirty(F).
%% end modify by james.wu@asiainnovations.com 2014-10-30 PM 4:27

%% Recalculates alive sessions when Node goes down 
%% and updates session and session_counter tables 
recount_session_table(Node) ->
    F = fun() ->
		Es = mnesia:select(
		       session,
		       [{#session{sid = {'_', '$1'}, _ = '_'},
			 [{'==', {node, '$1'}, Node}],
			 ['$_']}]),
		lists:foreach(fun(E) ->
				      mnesia:delete({session, E#session.sid})
			      end, Es),
		%% reset session_counter table with active sessions
		mnesia:clear_table(session_counter),
		lists:foreach(fun(Server) ->
				LServer = jlib:nameprep(Server),
				Hs = mnesia:select(session,
				    [{#session{usr = '$1', _ = '_'},
				    [{'==', {element, 2, '$1'}, LServer}],
				    ['$1']}]),
				mnesia:write(
				    #session_counter{vhost = LServer, 
						     count = length(Hs)})
			      end, ?MYHOSTS)
	end,
    mnesia:async_dirty(F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_route(From, To, {broadcast, _} = Packet) ->
	case To#jid.lresource of
        <<"">> ->
            lists:foreach(fun(R) ->
                                  do_route(From,
                                           jlib:jid_replace_resource(To, R),
                                           Packet)
                          end,
                          get_user_resources(To#jid.user, To#jid.server));
        _ ->
            USR = jlib:jid_tolower(To),
            case mnesia:dirty_index_read(session, USR, #session.usr) of
                [] ->
                    ?DEBUG("packet dropped~n", []);
                Ss ->
                    Session = lists:max(Ss),
                    Pid = element(2, Session#session.sid),
                    ?DEBUG("sending to process ~p~n", [Pid]),
                    Pid ! {route, From, To, Packet}
            end
    end;
do_route(From, To, #xmlel{} = Packet) ->
    ?DEBUG("session manager~n\tfrom ~p~n\tto ~p~n\tpacket "
	   "~P~n",
	   [From, To, Packet, 8]),
    #jid{user = User, server = Server,
	 luser = LUser, lserver = LServer, lresource = LResource} = To,
    #xmlel{name = Name, attrs = Attrs} = Packet,
    case LResource of
      <<"">> ->
	  case Name of
	    <<"presence">> ->
		{Pass, _Subsc} = case xml:get_attr_s(<<"type">>, Attrs)
				     of
				   <<"subscribe">> ->
				       Reason = xml:get_path_s(Packet,
							       [{elem,
								 <<"status">>},
								cdata]),
				       {is_privacy_allow(From, To, Packet)
					  andalso
					  ejabberd_hooks:run_fold(roster_in_subscription,
								  LServer,
								  false,
								  [User, Server,
								   From,
								   subscribe,
								   Reason]),
					true};
				   <<"subscribed">> ->
				       {is_privacy_allow(From, To, Packet)
					  andalso
					  ejabberd_hooks:run_fold(roster_in_subscription,
								  LServer,
								  false,
								  [User, Server,
								   From,
								   subscribed,
								   <<"">>]),
					true};
				   <<"unsubscribe">> ->
				       {is_privacy_allow(From, To, Packet)
					  andalso
					  ejabberd_hooks:run_fold(roster_in_subscription,
								  LServer,
								  false,
								  [User, Server,
								   From,
								   unsubscribe,
								   <<"">>]),
					true};
				   <<"unsubscribed">> ->
				       {is_privacy_allow(From, To, Packet)
					  andalso
					  ejabberd_hooks:run_fold(roster_in_subscription,
								  LServer,
								  false,
								  [User, Server,
								   From,
								   unsubscribed,
								   <<"">>]),
					true};
				   _ -> {true, false}
				 end,
		if Pass ->
		       PResources = get_user_present_resources(LUser, LServer),
		       lists:foreach(fun ({_, R}) ->
					     do_route(From,
						      jlib:jid_replace_resource(To,
										R),
						      Packet)
				     end,
				     PResources);
		   true -> ok
		end;
	    <<"message">> -> route_message(From, To, Packet);
	    <<"iq">> -> process_iq(From, To, Packet);
	    _ -> ok
	  end;
      _ ->
	  %%USR = {LUser, LServer, LResource},
	  %%case mnesia:dirty_index_read(session, USR, #session.usr)
    case mnesia:dirty_index_read(session, {LUser}, #session.uid)
	      of
	    [] ->
		case Name of
		  <<"message">> -> route_message(From, To, Packet);
		  <<"iq">> ->
		      case xml:get_attr_s(<<"type">>, Attrs) of
			<<"error">> -> ok;
			<<"result">> -> ok;
			_ ->
			    Err = jlib:make_error_reply(Packet,
							?ERR_SERVICE_UNAVAILABLE),
			    ejabberd_router:route(To, From, Err)
		      end;
		  _ -> ?DEBUG("packet droped~n", [])
		end;
	    Ss ->
		Session = lists:max(Ss),
		Pid = element(2, Session#session.sid),
        ?DEBUG("sending to process ~p~n", [Pid]),
		Pid ! {route, From, To, Packet}
	  end
    end.

%% The default list applies to the user as a whole,
%% and is processed if there is no active list set
%% for the target session/resource to which a stanza is addressed,
%% or if there are no current sessions for the user.
is_privacy_allow(From, To, Packet) ->
    User = To#jid.user,
    Server = To#jid.server,
    PrivacyList =
	ejabberd_hooks:run_fold(privacy_get_user_list, Server,
				#userlist{}, [User, Server]),
    is_privacy_allow(From, To, Packet, PrivacyList).

%% Check if privacy rules allow this delivery
%% Function copied from ejabberd_c2s.erl
is_privacy_allow(From, To, Packet, PrivacyList) ->
    User = To#jid.user,
    Server = To#jid.server,
    allow ==
      ejabberd_hooks:run_fold(privacy_check_packet, Server,
			      allow,
			      [User, Server, PrivacyList, {From, To, Packet},
			       in]).

%% modify by james.wu@asiainnovations.com 2014-10-30 PM 02:52
route_message(From, To, OriginPacket) ->
    LUser = To#jid.luser,
	LServer = To#jid.lserver,
    PrioRes = get_user_present_resources(LUser, LServer),
	case mod_ai_robot:is_robot(LUser) of
		true ->
			mod_ai_robot:proxy_msg(From, To, OriginPacket);
		false ->
			Packet = mod_ai_resend:process_asure_Receipts(OriginPacket),
  %% modify by james.wu@asiainnovations.com 2014-09-22 PM 07:57 end
	case catch lists:max(PrioRes) of
      {Priority, _R}
	  when is_integer(Priority), Priority >= 0 ->
	  lists:foreach(fun ({P, R}) when P == Priority ->
				LResource = jlib:resourceprep(R),
				USR = {LUser, LServer, LResource},
				case mnesia:dirty_index_read(session, USR,
							     #session.usr)
				    of
				  [] ->
						case mnesia:dirty_index_read(session, {LUser}, #session.uid) of
						  [] ->
						      ok; % Race condition
						  Ss1 ->
						      Session = lists:max(Ss1),
						      Pid = element(2, Session#session.sid),
						      Pid ! {route, From, To, Packet}
						end;
				  Ss ->
				      Session = lists:max(Ss),
				      Pid = element(2, Session#session.sid),
				      Pid ! {route, From, To, Packet}
				end;
			    %% Ignore other priority:
			    ({_Prio, _Res}) -> ok
			end,
			PrioRes);
      _ ->
	  case xml:get_tag_attr_s(<<"type">>, Packet) of
	    <<"error">> -> ok;
	    <<"groupchat">> -> 
		bounce_offline_message(From, To, Packet);
	    <<"headline">> -> 
		bounce_offline_message(From, To, Packet);
	    _ ->
		case ejabberd_auth:is_user_exists(LUser, LServer) of
		  true ->
		      case is_privacy_allow(From, To, Packet) of
			true ->
			    ejabberd_hooks:run(offline_message_hook, LServer,
					       [From, To, Packet]);
			false -> ok
		      end;
		  _ -> 
		      Err = jlib:make_error_reply(Packet,
						  ?ERR_SERVICE_UNAVAILABLE),
		      ejabberd_router:route(To, From, Err)
		end
	  end
	%% modify by james.wu@asiainnovations.com 2014-09-25 PM 06:21 start
	  end
	%% modify by james.wu@asiainnovations.com 2014-09-25 PM 06:21 end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clean_session_list(Ss) ->
    clean_session_list(lists:keysort(#session.usr, Ss), []).

clean_session_list([], Res) -> Res;
clean_session_list([S], Res) -> [S | Res];
clean_session_list([S1, S2 | Rest], Res) ->
    if S1#session.usr == S2#session.usr ->
	   if S1#session.sid > S2#session.sid ->
		  clean_session_list([S1 | Rest], Res);
	      true -> clean_session_list([S2 | Rest], Res)
	   end;
       true -> clean_session_list([S2 | Rest], [S1 | Res])
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% On new session, check if some existing connections need to be replace
check_for_sessions_to_replace(User, Server, Resource) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    LResource = jlib:resourceprep(Resource),
    check_existing_resources(LUser, LServer, LResource),
    check_max_sessions(LUser, LServer).

check_existing_resources(LUser, LServer, LResource) ->
    SIDs = get_resource_sessions(LUser, LServer, LResource),
    if SIDs == [] -> ok;
       true ->
	   MaxSID = lists:max(SIDs),
     ?DEBUG("LUser:~p, LServer:~p, LResource:~p, MaxSID:~p~n", [LUser, LServer, LResource, MaxSID]),
	   %%lists:foreach(fun ({_, Pid} = S) when S /= MaxSID ->
     lists:foreach(fun ({_, Pid} = S) ->
       ?DEBUG("replaced LUser:~p, LServer:~p, LResource:~p, Session:~p~n", [LUser, LServer, LResource, S]),
				 Pid ! replaced;
			     (_) -> ok
			 end,
			 SIDs)
    end.

-spec is_existing_resource(binary(), binary(), binary()) -> boolean().

is_existing_resource(LUser, LServer, LResource) ->
    Session = get_resource_sessions(LUser, LServer, LResource),
    ?DEBUG("LUser:~p, LServer:~p, LResource:~p, Session:~p~n", [LUser, LServer, LResource, Session]),
    [] /= get_resource_sessions(LUser, LServer, LResource).

%%get_resource_sessions(User, Server, Resource) ->
%%    USR = {jlib:nodeprep(User), jlib:nameprep(Server),
%%	   jlib:resourceprep(Resource)},
%%    mnesia:dirty_select(session,
%%			[{#session{sid = '$1', usr = USR, _ = '_'}, [],
%%			  ['$1']}]).

get_resource_sessions(User, _Server, _Resource) ->
  Uid = {jlib:nodeprep(User)},
  case catch mnesia:dirty_index_read(session, Uid, #session.uid) of
    {'EXIT', _Reason} -> [];
    Ss ->
      [S#session.sid || S <- clean_session_list(Ss)]
  end.

check_max_sessions(LUser, LServer) ->
    SIDs = mnesia:dirty_select(session,
			       [{#session{sid = '$1', us = {LUser, LServer},
					  _ = '_'},
				 [], ['$1']}]),
    MaxSessions = get_max_user_sessions(LUser, LServer),
    if length(SIDs) =< MaxSessions -> ok;
       true -> {_, Pid} = lists:min(SIDs), Pid ! replaced
    end.

%% Get the user_max_session setting
%% This option defines the max number of time a given users are allowed to
%% log in
%% Defaults to infinity
get_max_user_sessions(LUser, Host) ->
    case acl:match_rule(Host, max_user_sessions,
			jlib:make_jid(LUser, Host, <<"">>))
	of
      Max when is_integer(Max) -> Max;
      infinity -> infinity;
      _ -> ?MAX_USER_SESSIONS
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

process_iq(From, To, Packet) ->
    IQ = jlib:iq_query_info(Packet),
    case IQ of
      #iq{xmlns = XMLNS} ->
	  Host = To#jid.lserver,
	  case ets:lookup(sm_iqtable, {XMLNS, Host}) of
	    [{_, Module, Function}] ->
		ResIQ = Module:Function(From, To, IQ),
		if ResIQ /= ignore ->
		       ejabberd_router:route(To, From, jlib:iq_to_xml(ResIQ));
		   true -> ok
		end;
	    [{_, Module, Function, Opts}] ->
		gen_iq_handler:handle(Host, Module, Function, Opts,
				      From, To, IQ);
	    [] ->
		Err = jlib:make_error_reply(Packet,
					    ?ERR_SERVICE_UNAVAILABLE),
		ejabberd_router:route(To, From, Err)
	  end;
      reply -> ok;
      _ ->
	  Err = jlib:make_error_reply(Packet, ?ERR_BAD_REQUEST),
	  ejabberd_router:route(To, From, Err),
	  ok
    end.

-spec force_update_presence({binary(), binary()}) -> any().

force_update_presence({LUser, _LServer} = US) ->
    case catch mnesia:dirty_index_read(session, US,
				       #session.us)
	of
      {'EXIT', _Reason} -> ok;
      Ss ->
	  lists:foreach(fun (#session{sid = {_, Pid}}) ->
				Pid ! {force_update_presence, LUser}
			end,
			Ss)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ejabberd commands

commands() ->
    [#ejabberd_commands{name = connected_users,
			tags = [session],
			desc = "List all established sessions",
			module = ?MODULE, function = connected_users, args = [],
			result = {connected_users, {list, {sessions, string}}}},
     #ejabberd_commands{name = connected_users_number,
			tags = [session, stats],
			desc = "Get the number of established sessions",
			module = ?MODULE, function = connected_users_number,
			args = [], result = {num_sessions, integer}},
     #ejabberd_commands{name = user_resources,
			tags = [session],
			desc = "List user's connected resources",
			module = ?MODULE, function = user_resources,
			args = [{user, binary}, {host, binary}],
			result = {resources, {list, {resource, string}}}},
     #ejabberd_commands{name = disconnect_user,
			tags = [session],
			desc = "Disconnect user's active sessions",
			module = ?MODULE, function = disconnect_user,
			args = [{user, binary}, {host, binary}],
			result = {num_resources, integer}}].

-spec connected_users() -> [binary()].

connected_users() ->
    USRs = dirty_get_sessions_list(),
    SUSRs = lists:sort(USRs),
    lists:map(fun ({U, S, R}) -> <<U/binary, $@, S/binary, $/, R/binary>> end,
	      SUSRs).

connected_users_number() ->
    length(dirty_get_sessions_list()).

user_resources(User, Server) ->
    Resources = get_user_resources(User, Server),
    lists:sort(Resources).

disconnect_user(User, Server) ->
    Resources = get_user_resources(User, Server),
    lists:foreach(
	fun(Resource) ->
		PID = get_session_pid(User, Server, Resource),
		PID ! disconnect
	end, Resources),
    length(Resources).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Update Mnesia tables

update_tables() ->
    case catch mnesia:table_info(session, attributes) of
      [ur, user, node] -> mnesia:delete_table(session);
      [ur, user, pid] -> mnesia:delete_table(session);
      [usr, us, pid] -> mnesia:delete_table(session);
      [usr, us, sid, priority, info] -> mnesia:delete_table(session);
%% begin add by james.wu@asiainnovations.com 2014-10-30 PM 5:51
      [sid, usr, us, priority] -> mnesia:delete_table(session);
      [sid, usr, us, priority, info] -> mnesia:delete_table(session);
      [sid, usr, us, uid, priority, info] -> ok;
%% end add by james.wu@asiainnovations.com 2014-10-30 PM 5:51
      {'EXIT', _} -> ok
    end,
    case lists:member(presence, mnesia:system_info(tables))
	of
      true -> mnesia:delete_table(presence);
      false -> ok
    end,
    case lists:member(local_session, mnesia:system_info(tables)) of
	true ->
	    mnesia:delete_table(local_session);
	false ->
	    ok
    end.
