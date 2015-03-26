%%%----------------------------------------------------------------------
%%% File    : mod_ai_robot.erl
%%% Author  : wugaoping<james.wu@asiainnovations.com>
%%%
%%%----------------------------------------------------------------------

-module(mod_ai_robot).

-author('james.wu@asiainnovations.com').

-behaviour(gen_mod).

%% gen_mod callbacks
-export([start/2, stop/1]).

%% iq handlers
-export([sendMessage/2, sendBroadcast/2, sendIQ/2, proxy_msg/3, is_robot/1]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").
-include("ai.hrl").

start(_Host, _Opts) ->
    ok.

stop(_Host) ->
    ok.

%-------------------------------------------------------------------------

-spec sendMessage(SenderId::string() | binary(), Data::string() | binary()) -> any().
sendMessage(_SenderId, Data) ->
	case rfc4627:decode(Data) of
    	{ok, MessageObject, _} ->
			Rid = rfc4627:get_field(MessageObject, "rid", <<"">>),
			case ejabberd_sm:get_user_jids(Rid) of
				[{_LUser, LServer, LResource} | _] -> 
					Server = LServer,
					Resource = LResource;
				_ ->
					Server = ?AI_LOCAL_HOST,
					Resource = ?AI_LOCAL_RESOURCE
			end,
			ServerResource = binary_to_list(Server)  ++ "/" ++ binary_to_list(Resource),
	    To = list_to_binary(binary_to_list(Rid) ++ "@" ++ ServerResource),
			Fromid = rfc4627:get_field(MessageObject, "sid", <<"">>),
      if Fromid =:= <<"">>; Fromid =:= <<>> ->
          ?DEBUG("Fromid:~p~n", [Fromid]),
          From = Server;
      true ->
          ?DEBUG("Fromid:~p~n", [Fromid]),
          From = list_to_binary(binary_to_list(Fromid) ++ "@" ++ ServerResource)
      end,
	    Type = rfc4627:get_field(MessageObject, "type", <<"chat">>),
	    MType = rfc4627:get_field(MessageObject, "msgType", <<"1">>),
			Content = rfc4627:get_field(MessageObject, "content", <<"">>),
			if is_binary(Content) ->
				Body = Content;
			true ->
				Body = list_to_binary(rfc4627:encode(Content))
			end,
			case rfc4627:get_field(MessageObject, "id", <<"">>) of
				<<"">> -> 
					MsgId = mod_ai_msg:uuid();
				Id -> 
					MsgId = Id
			end,
            Attrs0 = [{<<"to">>, To},
                     {<<"from">>, From},
					 {<<"fromid">>, Fromid},
					 {<<"type">>, Type},
                     {<<"id">>, MsgId},
                     {<<"mtype">>, MType}],
			case rfc4627:get_field(MessageObject, "gid", <<"">>) of
				<<"">> -> 
					Attrs1 =  Attrs0;
				Gid -> 
					Attrs1 =  [{<<"gid">>, Gid} | Attrs0]
			end,
			case rfc4627:get_field(MessageObject, "subject", <<"">>) of
				<<"">> -> 
					SubEl = [#xmlel{name = <<"body">>, children = [{xmlcdata, Body}]}];
				Subject -> 
					SubEl = [#xmlel{name = <<"subject">>, children = [{xmlcdata, Subject}]},
			                 #xmlel{name = <<"body">>, children = [{xmlcdata, Body}]}]
			end,
			?DEBUG("From:~p, To:~p, Attrs1:~p, SubEl:~p~n~n", [From, To, Attrs1, SubEl]),
	        mod_ai_msg:route(jlib:string_to_jid(From),
								  jlib:string_to_jid(To), 
								  #xmlel{name = <<"message">>, attrs = Attrs1, children = SubEl},
                  noack);
		_ -> ok
	end,
	ok.

-spec sendBroadcast(SenderId::string() | binary(), Data::string() | binary()) -> any().
sendBroadcast(_SenderId, Data) ->
	case rfc4627:decode(Data) of
    	{ok, BroadcastMessageObject, _} ->
			Rid = rfc4627:get_field(BroadcastMessageObject, "rid", <<"">>),
			case ejabberd_sm:get_user_jids(Rid) of
				[{_LUser, LServer, LResource} | _] -> 
					Server = LServer,
					Resource = LResource;
				_ ->
					Server = ?AI_LOCAL_HOST,
					Resource = ?AI_LOCAL_RESOURCE
			end,
			ServerResource = binary_to_list(Server)  ++ "/" ++ binary_to_list(Resource),
	    To = list_to_binary(binary_to_list(Rid) ++ "@" ++ ServerResource),
			Fromid = rfc4627:get_field(BroadcastMessageObject, "sid", <<"">>),
      if Fromid =:= <<"">>; Fromid =:= <<>> ->
          From = Server;
      true ->
          From = list_to_binary(binary_to_list(Fromid) ++ "@" ++ ServerResource)
      end,
	    Type = rfc4627:get_field(BroadcastMessageObject, "type", <<"groupchat">>),
	    MType = rfc4627:get_field(BroadcastMessageObject, "msgType", ?AI_MESSAGE_MTYPE_GROUP_SYSTEM_MESSAGE),
			Content = rfc4627:get_field(BroadcastMessageObject, "content", <<"">>),
			if is_binary(Content) ->
				Body = Content;
			true ->
				Body = list_to_binary(rfc4627:encode(Content))
			end,
			case rfc4627:get_field(BroadcastMessageObject, "id", <<"">>) of
				<<"">> -> 
					MsgId = mod_ai_msg:uuid();
				Id -> 
					MsgId = Id
			end,
      Attrs0 = [{<<"to">>, To},
               {<<"from">>, From},
               {<<"fromid">>, Fromid},
               {<<"type">>, Type},
               {<<"id">>, MsgId},
               {<<"mtype">>, MType}],
      Gid = rfc4627:get_field(BroadcastMessageObject, "gid", <<"">>),
			if Gid =:= <<"">> ->
					Attrs1 =  Attrs0;
			true ->
					Attrs1 =  [{<<"gid">>, Gid} | Attrs0]
			end,
      OpUser = rfc4627:get_field(BroadcastMessageObject, "opUser", <<"">>),
			if OpUser =:= <<"">> ->
					Attrs2 =  Attrs1;
			true ->
					Attrs2 =  [{<<"opUser">>, OpUser} | Attrs1]
			end,
      Subject = rfc4627:get_field(BroadcastMessageObject, "subject", <<"">>),
      if Subject =:= <<"">> ->
            if Type =:= <<"chat">> ->
				SubEl = [#xmlel{name = <<"body">>, children = [{xmlcdata, Body}]}];
			true ->
				SubEl = [#xmlel{name = <<"subject">>, children = [{xmlcdata, <<"group_broadcast">>}]},
			                 #xmlel{name = <<"body">>, children = [{xmlcdata, Body}]}]
			end;
      true ->
            SubEl = [#xmlel{name = <<"subject">>, children = [{xmlcdata, Subject}]},
                         #xmlel{name = <<"body">>, children = [{xmlcdata, Body}]}]
	  end,
      ?DEBUG("From:~p, To:~p, Attrs1:~p, SubEl:~p~n~n", [From, To, Attrs1, SubEl]),
      case Type of
        <<"chat">> ->
          mod_ai_msg:route(jlib:string_to_jid(From),
            jlib:string_to_jid(To),
            #xmlel{name = <<"message">>, attrs = Attrs2, children = SubEl},
            noack);
        <<"groupchat">> ->
          mod_ai_group:broadcast_in_group(jlib:string_to_jid(From), Fromid, Gid, <<>>, OpUser, Subject, Body)
      end;
		_ -> ok
	end,
	ok.

-spec sendIQ(SenderId::string() | binary(), Data::string() | binary()) -> any().
sendIQ(_SenderId, Data) ->
	case rfc4627:decode(Data) of
    	{ok, IQObject, _} ->
			ServerResource = binary_to_list(?AI_LOCAL_HOST)  ++ "/" ++ binary_to_list(?AI_LOCAL_RESOURCE),
			To = jlib:string_to_jid(?AI_LOCAL_HOST),
			From = jlib:string_to_jid(list_to_binary(binary_to_list(rfc4627:get_field(IQObject, "from", <<"">>)) ++ "@" ++ ServerResource)),
			Type = rfc4627:get_field(IQObject, "type", <<"get">>),
			Xmlns = rfc4627:get_field(IQObject, "xmlns", <<"">>),
			case rfc4627:get_field(IQObject, "id", <<"">>) of
				<<"">> -> 
					MsgId = mod_ai_msg:uuid();
				Id -> 
					MsgId = Id
			end,
            Attrs = [{<<"id">>, MsgId}, {<<"type">>, Type}],
			{obj, IQParams} = rfc4627:get_field(IQObject, "params", {obj, []}),
			SubEl0 = [#xmlel{name = list_to_binary(Key), attrs = [], children = [{xmlcdata, list_to_binary(Value)}]} || {Key, Value} <- IQParams, is_list(Value) ],
			SubEl1 = [#xmlel{name = list_to_binary(Key), attrs = [], children = [{xmlcdata, Value}]} || {Key, Value} <- IQParams, is_binary(Value) ],
			SubEl2 = [#xmlel{name = list_to_binary(Key), attrs = [], children = [{xmlcdata, list_to_binary(rfc4627:encode(Value))}]} || {Key, Value} <- IQParams, is_tuple(Value) ],
			SubEl3 = SubEl0 ++ SubEl1,
			SubEl = SubEl2 ++ SubEl3,
			Packet = #xmlel{name = <<"iq">>, attrs = Attrs, children = [#xmlel{name = <<"query">>, attrs = [{<<"xmlns">>, Xmlns}], children = SubEl}]},
			?DEBUG("IQParams:~p~n~n, SubEl:~p~n~n, Packet:~p~n~n",[IQParams, SubEl, Packet]),
			ejabberd_router:route(From, To, Packet);
		_ -> ok
	end,
	ok.

-spec proxy_msg(From::jid(), To::jid(), Packet::any()) -> any().
proxy_msg(From, To, Packet = #xmlel{name = <<"message">>, attrs = Attrs, children = _SubEl}) ->
	Type = xml:get_attr_s(<<"type">>, Attrs),
	if Type =/= ?AI_MESSAGE_CHAT_TYPE_ERROR, Type =/= ?AI_MESSAGE_CHAT_TYPE_CANCEL ->
		Id = xml:get_attr_s(<<"id">>, Attrs),
		Tostr = jlib:jid_to_string(To),
		Fromstr = jlib:jid_to_string(From),
		Fromid = xml:get_attr_s(<<"fromid">>, Attrs),
		Gid = xml:get_attr_s(<<"gid">>, Attrs),
		Mtype = xml:get_attr_s(<<"mtype">>, Attrs),
		Body = xml:get_subtag_cdata(Packet, <<"body">>),
		Subject = xml:get_subtag_cdata(Packet, <<"subject">>),
		?DEBUG("params:~p, ~p, ~p, ~p, ~p, ~p, ~p, ~p, ~p", [Id, Type, Tostr, Fromstr, Fromid, Gid, Mtype, Body, Subject]),
		ai_api:proxy_msg(Id, Type, Tostr, Fromstr, Fromid, Gid, Mtype, Body, Subject);
	   true -> ok
	end,
	ok.

% check user is robot
-spec is_robot(Uid::string() | binary()) -> boolean().
is_robot(Uid) ->
	if Uid =/= <<>>, Uid =/= <<"">> ->
		Uid1 = binary_to_list(Uid) ++ ",",
		Uid2 = "," ++ Uid1,
		case string:str(?AI_ROBOT_LIST, Uid2) of
			Pos when Pos > 0 -> true;
			_ -> false
		end;
	true -> false
	end.
