%%%----------------------------------------------------------------------
%%% File    : mod_ai_msg.erl
%%% Author  : wugaoping<james.wu@asiainnovations.com>
%%%
%%%----------------------------------------------------------------------

-module(mod_ai_msg).

-author('james.wu@asiainnovations.com').

-behaviour(gen_mod).

%% gen_mod callbacks
-export([start/2, stop/1]).

%% hook handlers
-export([user_send_packet/3, user_receive_packet/4, set_user_presence/4, unset_user_presence/4]).

%% Api functions
-export([store_offline_msg/3, send_offline_msg/1, get_user_online/1,route/4, uuid/0]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").
-include("ai.hrl").

start(Host, _Opts) ->
    ejabberd_hooks:add(user_send_packet, Host, ?MODULE,
		       user_send_packet, 99),
    ejabberd_hooks:add(user_receive_packet, Host, ?MODULE,
		       user_receive_packet, 99),
    ejabberd_hooks:add(set_presence_hook, Host, ?MODULE,
		       set_user_presence, 99),
    ejabberd_hooks:add(unset_presence_hook, Host, ?MODULE,
               unset_user_presence, 99),
	ok.

stop(Host) ->
    ejabberd_hooks:delete(user_send_packet, Host,
			  ?MODULE, user_send_packet, 99),
    ejabberd_hooks:delete(user_receive_packet, Host,
			  ?MODULE, user_receive_packet, 99),
    ejabberd_hooks:delete(set_presence_hook, Host, 
              ?MODULE, set_user_presence, 99),
    ejabberd_hooks:delete(unset_presence_hook, Host,
              ?MODULE, unset_user_presence, 99),
	ok.

%-------------------------------------------------------------------------

user_send_packet(From, To, Packet = #xmlel{name = <<"message">>}) ->
  Pass = mod_ai_safe:check_do_packet_permit(From, Packet),
  if Pass =:= true ->
	  do_user_packet(From, To, Packet, ack);
  true -> ok
  end;
user_send_packet(_From, _To, _Packet) -> ok.

user_receive_packet(_JID, _From, _To, _Packet = #xmlel{name = <<"message">>}) ->
  %%?INFO_MSG("from:~p, to:~p, packet:~p, ~n~n", [From, To, Packet]),
  %% 接收方用户在线，将消息加入到重发进程
  %%Online = mod_ai_msg:get_user_online(To),
  %%if Online =:= 1 ->
  %%  mod_ai_resend:process_resend_message(From, To, Packet);
  %%true -> ok
  %%end,
  ok;
user_receive_packet(_JID, _From, _To, _Packet) -> ok.

do_user_packet(From, To, Packet = #xmlel{name = <<"message">>, attrs = Attrs, children = _Els}, NeedAck) ->
    Type = xml:get_attr_s(<<"type">>, Attrs),
    ?INFO_MSG("type:~p, from:~p, to:~p~n~n", [Type, From, To]),
    case Type of
        <<"groupchat">> ->
            send_ack(From, To, Packet, 0, ?AI_MESSAGE_CHAT_TYPE_GROUP, NeedAck),
            route_group_packet(From, To, Packet);
        _ ->
		   case mod_ai_resend:is_receipts(Packet) of
			   true -> 
				   mod_ai_resend:process_receipts_message(From, To, Packet);
			   false -> 
		           MType = xml:get_attr_s(<<"mtype">>, Attrs),
		           case MType of
		               <<"ack">> -> ok;
		               _ ->
		                   if Type /= <<"groupchat">>, Type /= <<"error">> ->
                         LUser = To#jid.luser,
                         case mod_ai_robot:is_robot(LUser) of
                           true ->
                             Online = 0,
                             send_ack(From, To, Packet, Online, ?AI_MESSAGE_CHAT_TYPE_SINGLE, NeedAck);
                           false ->
                             Online = get_user_online(To),
                             send_ack(From, To, Packet, Online, ?AI_MESSAGE_CHAT_TYPE_SINGLE, NeedAck),
                             case Online of
                                 0 ->
                                     store_offline_msg(From, To, Packet),
                                     store_msg(From, To, Packet, Online),
                                     push_msg(From, To, Packet);
                                 1 ->
                                     mod_ai_resend:process_resend_message(From, To, Packet),
                                     store_msg(From, To, Packet, Online)
                             end
                         end
		                   end
		           end
		   end
    end,
	ok.

set_user_presence(User, Server, Resource, _Presence) ->
    From = #jid{user = User, server = Server,
             resource = Resource, luser = User, lserver = Server,
             lresource = Resource},
    ?INFO_MSG("user:~p, server:~p, resource:~p, presence:~p~n~n", [User, Server, Resource, _Presence]),
    ai_api:update_online_state(User, 1),
    %% 用户刚上线，暂时不再推送离线消息
    send_offline_msg(From),
    ok.

unset_user_presence(User, _Server, _Resource, _Presence) ->
    ?INFO_MSG("user:~p, server:~p, resource:~p, presence:~p~n~n", [User, _Server, _Resource, _Presence]),
    ai_api:update_online_state(User, 0),
    ok.

%% sendAck <message xmlns="ai:message:ack" from="chat.pengpeng.la" 
%% to="502712@chat.pengpeng.la/Smack" type="chat" mtype="ack" 
%% msgid="67A23434-1D68-417B-9CF7-37F7DDC4EBBB" 
%% msgto="521638@chat.pengpeng.la">
%% <body xmlns="">{"c":{"online":1}}</body>
%% </message>
-spec send_ack(From::jid(), To::jid(), Packet::any(), Online::integer(), ChatType::binary(), NeedAck::integer()) -> any().
send_ack(From, To, Packet = #xmlel{name = <<"message">>, attrs = Attrs, children = _Els}, Online, ChatType, NeedAck) ->
    MType = xml:get_attr_s(<<"mtype">>, Attrs),
	if MType =/= <<"ack">>, From#jid.luser =/= <<"">>, NeedAck =:= ack  ->
		case mod_ai_robot:is_robot(From#jid.luser) of
			false ->
				?DEBUG("send_ack mtype:~p, online:~p, from:~p, to:~p, packet:~p~n~n", [MType, Online, From, To, Packet]),
				MsgId = xml:get_attr_s(<<"id">>, Attrs),
		        Body = "{\"c\":{\"online\":" ++ integer_to_list(Online) ++ "}}",
		        ServerJID = #jid{user = <<"">>, server = From#jid.lserver, resource = <<"">>, 
								 luser = <<"">>, lserver = From#jid.lserver, lresource = <<"">>},
		        ejabberd_router:route(ServerJID,
		                            From,
		                            #xmlel{name = <<"message">>,
		                                   attrs = [{<<"xmlns">>, ?NS_AI_MESSAGE_ACK},
		                                            {<<"to">>, jlib:jid_to_string(From)},
		                                            {<<"from">>, jlib:jid_to_string(ServerJID)},
		                                            {<<"type">>, ChatType},
		                                            {<<"mtype">>, <<"ack">>},
													%%{<<"id">>, mod_ai_msg:uuid()},
		                                            {<<"msgid">>, MsgId},
		                                            {<<"msgto">>, jlib:jid_to_string(To)}],
		                                   children = [#xmlel{name = <<"body">>, attrs = [], children = [{xmlcdata, list_to_binary(Body)}]}]});
			true -> ok
		end;
	true -> ok
    end,
    ok.

%% user online status: 0, offline; 1, online;
-spec get_user_online(JID::jid()) -> integer().
get_user_online(_JID = #jid{luser = User, lserver = Server}) ->
    case ejabberd_sm:get_user_resources(User, Server) of
        [] -> 0;
        _ -> 1
    end.

%% store offline msg to service mysql
-spec store_offline_msg(From::jid(), To::jid(), Packet::any()) -> any().
store_offline_msg(From, To, Packet = #xmlel{name = <<"message">>, attrs = Attrs, children = _Els}) ->
    MsgType = xml:get_attr_s(<<"mtype">>, Attrs),
    ?DEBUG("mtype:~p, from:~p, to:~p, packet:~p~n~n", [MsgType, From, To, Packet]),
    Rid = To#jid.luser,
    case is_support_offline_msg(MsgType, Rid) of
        false -> ok;
        true -> 
            Sid = From#jid.luser,
            Gid = case xml:get_attr_s(<<"gid">>, Attrs) of
                      <<"">> -> <<"0">>;
                      GID -> GID
                  end,
            Msgid = xml:get_attr_s(<<"id">>, Attrs),
            Content = xml:get_subtag_cdata(Packet, <<"body">>),
            Url = <<"">>,
            ?DEBUG("sid:~p, rid:~p, gid:~p, msgid:~p, msgtype:~p, content:~p~n~n", [Sid, Rid, Gid, Msgid, MsgType, Content]),
            ai_api:store_offline_msg(Sid, Rid, Gid, Msgid, MsgType, 0, Content, Url)
    end.

%%<message to="518541@chat02.pengpeng.la/PPIOS" from="chat02.pengpeng.la">
%%<subject>of_msg</subject>
%%<body>{}</body>
%%</message>
%% fetch offline msg from service mysql, and send to user
-spec send_offline_msg(From::jid()) -> any().
send_offline_msg(From) ->
    MsgsJson = ai_api:get_offline_msg(From#jid.luser),
	case MsgsJson of 
		[] -> ok;
    <<"">> -> ok;
		_  ->
		    ServerJID = #jid{user = <<"">>, server = From#jid.lserver,
		             resource = <<"">>, luser = <<"">>, lserver = From#jid.lserver,
		             lresource = <<"">>},
		    ?DEBUG("server:~p, from:~p, msgsJson:~p~n~n", [ServerJID, From, MsgsJson]),
        %% 离线消息不重发，并且不重复记录，避免客户端出现网络问题时滚雪球
        Packet = #xmlel{name = <<"message">>,
          attrs = [{<<"to">>, jlib:jid_to_string(From)},
              {<<"from">>, jlib:jid_to_string(ServerJID)},
              {<<"id">>, mod_ai_msg:uuid()}],
          children =
              [
                #xmlel{name = <<"subject">>, attrs = [], children = [{xmlcdata, <<"of_msg">>}]},
                #xmlel{name = <<"body">>, attrs = [], children = [{xmlcdata, MsgsJson}]},
                #xmlel{name = ?AI_RECEIPTS_REQUEST, attrs = [{<<"xmlns">>, ?AI_RECEIPTS_NAMESPACE}]}
              ]},
        ejabberd_router:route(ServerJID, From, Packet),
        mod_ai_resend:process_resend_message(ServerJID, From, Packet)
	end,
  ok.

-spec push_msg(From::jid(), To::jid(), Packet::any()) -> any().
push_msg(From, To, Packet = #xmlel{name = <<"message">>, attrs = Attrs, children = SubEl}) ->
    MsgType = xml:get_attr_s(<<"mtype">>, Attrs),
    ?DEBUG("msgtype:~p, from:~p, to:~p, packet:~p~n~n", [MsgType, From, To, Packet]),
    Rid = To#jid.luser,
    case is_support_push_msg(MsgType, Rid) of
        false -> ok;
        true ->
            Sid = From#jid.luser,
            Gid = case xml:get_attr_s(<<"gid">>, Attrs) of
                      <<>> -> <<"0">>;
                      GID -> GID
                  end,
            Body = xml:get_subtag_cdata(Packet, <<"body">>), 
            ?DEBUG("msgtype:~p, body:~p~n~n", [MsgType, Body]),
            case MsgType of
                ?AI_MESSAGE_MTYPE_AUDIO ->
                    Content = <<"has_voice_message">>,
                    NeedPush = true,
                    Sid1 = Sid,
                    MsgType1 = MsgType;
                ?AI_MESSAGE_MTYPE_PICTURE ->
                    Content = <<"has_send_photo">>,
                    NeedPush = true,
                    Sid1 = Sid,
                    MsgType1 = MsgType;
                ?AI_MESSAGE_MTYPE_GAME ->
					?DEBUG("Packet:~p~n~nSubEl:~p~n~n",[Packet, SubEl]),
                    Subject = xml:get_subtag_cdata(Packet, <<"subject">>),
                    case Subject of
                        <<"game_update">> ->
                            case rfc4627:decode(Body) of
                                {obj, BodyObject, _} ->
                                    GameType = rfc4627:get_field(BodyObject, "t", <<"">>),
                                    if GameType == <<"TRUTH">>; GameType == <<"DARE">> ->
                                        State = rfc4627:get_field(BodyObject, "state", <<"">>),
                                        Index = string:str(State, "selectedQuestion"),
                                        if Index > 0 ->
                                                Content = <<"has_tod_game">>,
                                                NeedPush = true,
                                                Sid1 = rfc4627:get_field(BodyObject, "u", <<"">>),
                                                MsgType1 = ?AI_MESSAGE_MTYPE_TEXT;
                                        true ->
                                                Content = <<"has_send_photo">>,
                                                NeedPush = false,
                                                Sid1 = Sid,
                                                MsgType1 = MsgType
                                        end;
                                    true  ->
                                        Content = <<"has_send_photo">>,
                                        NeedPush = false,
                                        Sid1 = Sid,
                                        MsgType1 = MsgType
                                    end
                                end;
                        _ ->
                            Content = <<"has_new_message">>,
                            NeedPush = false,
                            Sid1 = Sid,
                            MsgType1 = MsgType
                    end;
                ?AI_MESSAGE_MTYPE_MAGZINE ->
                    case rfc4627:decode(Body) of
                        {ok, MagzineObject, _} ->
                            Content = rfc4627:get_field(MagzineObject, "chatlist", <<"has_new_message">>),
                            NeedPush = true,
                            Sid1 = Sid,
                            MsgType1 = MsgType;
                        _ ->
                            Content = <<"has_new_message">>,
                            NeedPush = false,
                            Sid1 = Sid,
                            MsgType1 = MsgType
                    end;
                ?AI_MESSAGE_MTYPE_GAME_BUBLE ->
                  case rfc4627:decode(Body) of
                    {ok, BodyObject, _} ->
                      BodyContentObject = rfc4627:get_field(BodyObject, "c", <<>>),
                      Content = rfc4627:get_field(BodyContentObject, "gameTitle", <<"has_new_message">>),
                      NeedPush = true,
                      Sid1 = Sid,
                      MsgType1 = MsgType;
                    _ ->
                      Content = <<"has_new_message">>,
                      NeedPush = false,
                      Sid1 = Sid,
                      MsgType1 = MsgType
                  end;
                ?AI_MESSAGE_MTYPE_GAME_PUSH ->
                    case rfc4627:decode(Body) of
                        {ok, BodyObject, _} ->
                            BodyContent = binary:replace(rfc4627:get_field(BodyObject, "c", <<>>), <<"\\\"">>, <<"\"">>, [global]),
                            ?DEBUG("msgtype:~p, from:~p, to:~p, packet:~p, BodyContent:~p~n~n", [MsgType, From, To, Packet, BodyContent]),
                            case rfc4627:decode(BodyContent) of
                            {ok, GamePushObject, _} ->
                                Content = rfc4627:get_field(GamePushObject, "pushMessage", <<"has_new_message">>),
                                NeedPush = true,
                                Sid1 = Sid,
                                MsgType1 = MsgType;
                            _ ->
                                Content = <<"has_new_message">>,
                                NeedPush = false,
                                Sid1 = Sid,
                                MsgType1 = MsgType
                            end;
                        _ ->
                            Content = <<"has_new_message">>,
                            NeedPush = false,
                            Sid1 = Sid,
                            MsgType1 = MsgType
                    end;
                _ ->
                    case rfc4627:decode(Body) of
                        {ok, ChatObject, _} ->
                            Content = rfc4627:get_field(ChatObject, "c", <<"has_new_message">>),
                            NeedPush = true,
                            Sid1 = Sid,
                            MsgType1 = MsgType;
                        _ ->
                            Content = <<"has_new_message">>,
                            NeedPush = false,
                            Sid1 = Sid,
                            MsgType1 = MsgType
                    end
            end,
            ?DEBUG("needpush:~p, msgtype:~p, sid:~p, rid:~p, gid:~p, content:~p~n~n",
                      [NeedPush, MsgType1, Sid1, Rid, Gid, Content]),
            if NeedPush ->
                MsgTime = list_to_binary(integer_to_list(ai_utils:get_time())),
                MsgId = xml:get_attr_s(<<"id">>, Attrs), %% 使用url来保存msgid字段，做push推送消息的去重
                PushToken = <<"">>,
                ?DEBUG("msgtime:~p, msgtype:~p, sid:~p, rid:~p, gid:~p, content:~p~n~n",
                      [MsgTime, MsgType1, Sid1, Rid, Gid, Content]),
                ai_api:push_msg(MsgType1, Sid1, Rid, Gid, MsgId, Content, 1, PushToken, MsgTime);
			      true -> ok
            end
    end.

-spec store_msg(From::jid(), To::jid(), Packet::any(), MsgStatus::integer()) -> any().
store_msg(From, To, Packet = #xmlel{name = <<"message">>, attrs = Attrs, children = _SubEl}, MsgStatus) ->
    MsgType = xml:get_attr_s(<<"mtype">>, Attrs),
    Rid = To#jid.luser,
    ?DEBUG("msgtype:~p, from:~p, to:~p, packet:~p, status:~p~n~n", [MsgType, From, To, Packet, MsgStatus]),
    case is_support_store_msg(MsgType, Rid) of
        false -> ok;
        true ->
            Sid = From#jid.luser,
            Rid = To#jid.luser,
            Gid = case xml:get_attr_s(<<"gid">>, Attrs) of
                      <<>> -> <<"0">>;
                      GID -> GID
                  end,
            Msgid = xml:get_attr_s(<<"id">>, Attrs),
            Content = xml:get_subtag_cdata(Packet, <<"body">>),
            Url = <<"">>,
            ?DEBUG("Sid:~p, Rid:~p, Gid:~p, Msgid:~p, MsgType:~p, MsgStatus:~p, Content:~p, Url:~p~n~n",
                      [Sid, Rid, Gid, Msgid, MsgType, MsgStatus, Content, Url]),
            ai_api:store_msg(Sid, Rid, Gid, Msgid, MsgType, MsgStatus, Content, Url)
    end.

-spec route_group_packet(From::jid(), ToGroup::jid(), Packet::any()) -> any().
route_group_packet(From, ToGroup, Packet = #xmlel{name = <<"message">>, attrs = Attrs, children = _SubEl}) ->
    GroupId = ToGroup#jid.luser,
	SenderUid = From#jid.luser,
    case ai_api:get_group_members(GroupId) of
        [] ->
            ok;
        Members ->
			?INFO_MSG("From:~p, ToGroup:~p, Members:~p~n", [From, ToGroup, Members]),
            Body = xml:get_subtag_cdata(Packet, <<"body">>),
            MsgType = xml:get_attr_s(<<"mtype">>, Attrs),
            ID = xml:get_attr_s(<<"id">>, Attrs),
            lists:foreach(fun(MemberUid) when (MemberUid =/= SenderUid) ->
								case mod_ai_robot:is_robot(MemberUid) of
									false -> 
										case ejabberd_sm:get_user_jids(MemberUid) of
											[{_LUser, LServer, LResource} | _] -> 
												Server = LServer,
												Resource = LResource;
											_ ->
												Server = From#jid.lserver,
												Resource = From#jid.lresource
										end,
										To = #jid{user = MemberUid, server = Server, resource = Resource, 
												luser = MemberUid, lserver = Server, lresource = Resource},
		                                MsgId = list_to_binary(binary_to_list(ID) ++ "-" ++ MemberUid), 
		                                SubEl = [#xmlel{name = <<"subject">>, children = [{xmlcdata, <<"group_broadcast">>}]},
		                                        #xmlel{name = <<"body">>, children = [{xmlcdata, Body}]}],
										Message = #xmlel{name = <<"message">>,
		                                       attrs = [{<<"to">>, jlib:jid_to_string(To)},
		                                                {<<"from">>, jlib:jid_to_string(From)},
														{<<"fromid">>, SenderUid},
		                                                {<<"id">>, MsgId},
		                                                {<<"msgid">>, MsgId},
		                                                %%{<<"type">>, ?AI_MESSAGE_CHAT_TYPE_GROUP},
		                                                {<<"mtype">>, MsgType},
		                                                {<<"gid">>, GroupId}],
				                                        children = SubEl},
                    mod_ai_msg:route(From, To, Message, noack);
									true -> ok
								end;
						  (_Other) -> ok
                          end,
                          Members)
    end,
    ok.

-spec route(From::jid(), To::jid(), Packet::any(), NeedAck::integer()) -> any().
route(From, To, Packet = #xmlel{name = <<"message">>}, NeedAck) ->
  Message = mod_ai_resend:process_asure_Receipts(Packet),
  do_user_packet(From, To, Message, NeedAck),
  ejabberd_router:route(From, To, Message);
route(_From, _To, _Packet, _NeedAck) ->
  ok.

-spec uuid() -> binary().
uuid() ->
  UUID1 = os:cmd("uuidgen"),
  Len = string:len(UUID1),
  UUID2 = string:substr(UUID1, 1, Len - 1),
  list_to_binary(UUID2).

%% push msg is need: true, need; false, no need
-spec is_support_push_msg(MsgType::integer() | binary(), Rid::binary()) -> boolean().
is_support_push_msg(MsgType, Rid) ->
  if Rid =/= <<>> ->
    case mod_ai_robot:is_robot(Rid) of
      true -> false;
      false ->
          case MsgType of
          ?AI_MESSAGE_MTYPE_NIL -> true;
          ?AI_MESSAGE_MTYPE_TEXT -> true;
          ?AI_MESSAGE_MTYPE_AUDIO -> true;
          ?AI_MESSAGE_MTYPE_PICTURE -> true;
          ?AI_MESSAGE_MTYPE_GAME -> true;
          ?AI_MESSAGE_MTYPE_WEB_SHARE -> true;
          ?AI_MESSAGE_MTYPE_EMOTION -> true;
          ?AI_MESSAGE_MTYPE_COMMAND -> true;
          ?AI_MESSAGE_MTYPE_GAME_RESULT -> true;
          ?AI_MESSAGE_MTYPE_MAGZINE -> true;
          ?AI_MESSAGE_MTYPE_GAME_BUBLE -> true;
          ?AI_MESSAGE_MTYPE_GROUP_GAME_BUBLE -> true;
          ?AI_MESSAGE_MTYPE_GAME_PUSH -> true;
          ?AI_MESSAGE_MTYPE_SUGGEST_FRIEND -> true;
          _ -> false
        end
      end;
    true -> false
    end.

%% offline msg is need: true, need; false, no need
-spec is_support_offline_msg(MsgType::integer() | binary(), Rid::binary()) -> boolean().
is_support_offline_msg(MsgType, Rid) ->
  if Rid =/= <<>> ->
    case mod_ai_robot:is_robot(Rid) of
      true -> false;
      false ->
          case MsgType of
          ?AI_MESSAGE_MTYPE_NIL -> true;
          ?AI_MESSAGE_MTYPE_TEXT -> true;
          ?AI_MESSAGE_MTYPE_AUDIO -> true;
          ?AI_MESSAGE_MTYPE_PICTURE -> true;
          ?AI_MESSAGE_MTYPE_GAME -> true;
          ?AI_MESSAGE_MTYPE_WEB_SHARE -> true;
          ?AI_MESSAGE_MTYPE_EMOTION -> true;
          ?AI_MESSAGE_MTYPE_COMMAND -> true;
          ?AI_MESSAGE_MTYPE_GROUP_SYSTEM_MESSAGE -> true;
          ?AI_MESSAGE_MTYPE_CHAT_BACKGROUND -> true;
          ?AI_MESSAGE_MTYPE_GAME_RESULT -> true;
          ?AI_MESSAGE_MTYPE_MAGZINE -> true;
          ?AI_MESSAGE_MTYPE_GAME_BUBLE -> true;
          ?AI_MESSAGE_MTYPE_GROUP_GAME_BUBLE -> true;
          ?AI_MESSAGE_MTYPE_GAME_PUSH -> true;
          ?AI_MESSAGE_MTYPE_SUGGEST_FRIEND -> true;
              _ -> false
        end
    end;
    true -> false
  end.

%% store msg is need: true, need; false, no need
-spec is_support_store_msg(MsgType::integer() | binary(), Rid::binary()) -> boolean().
is_support_store_msg(MsgType, Rid) ->
  if Rid =/= <<>> ->
    case MsgType of
      ?AI_MESSAGE_MTYPE_TEXT -> true;
      ?AI_MESSAGE_MTYPE_AUDIO -> true;
      ?AI_MESSAGE_MTYPE_PICTURE -> true;
      ?AI_MESSAGE_MTYPE_GAME -> true;
      ?AI_MESSAGE_MTYPE_WEB_SHARE -> true;
      ?AI_MESSAGE_MTYPE_EMOTION -> true;
      ?AI_MESSAGE_MTYPE_COMMAND -> true;
      ?AI_MESSAGE_MTYPE_MAGZINE -> true;
      ?AI_MESSAGE_MTYPE_GAME_BUBLE -> true;
      ?AI_MESSAGE_MTYPE_GROUP_GAME_BUBLE -> true;
      ?AI_MESSAGE_MTYPE_GAME_PUSH -> true;
      _ -> false
    end;
    true -> false
  end.
