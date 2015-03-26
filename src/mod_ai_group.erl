%%%----------------------------------------------------------------------
%%% File    : mod_ai_group.erl
%%% Author  : wugaoping<james.wu@asiainnovations.com>
%%%
%%%----------------------------------------------------------------------

-module(mod_ai_group).

-author('james.wu@asiainnovations.com').

-behaviour(gen_mod).

%% gen_mod callbacks
-export([start/2, stop/1]).

%% iq handlers
-export([process_create_group_iq/3, process_update_groupname_iq/3,
         process_add_user_iq/3, process_del_user_iq/3,
         process_users_iq/3, process_user_groups_iq/3, broadcast_in_group/7]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").
-include("ai.hrl").
-include("../deps/ai_api/include/service_types.hrl").

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
                  ?NS_AI_GROUP_CHAT_NEW, ?MODULE, process_create_group_iq,
                  IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
                  ?NS_AI_GROUP_CHAT_UPDATE_GROUP_NAME, ?MODULE, process_update_groupname_iq,
                  IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
                  ?NS_AI_GROUP_CHAT_ADD_USERS, ?MODULE, process_add_user_iq,
                  IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
                  ?NS_AI_GROUP_CHAT_DEL_USERS, ?MODULE, process_del_user_iq,
                  IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
                  ?NS_AI_GROUP_CHAT_USERS, ?MODULE, process_users_iq,
                  IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
                  ?NS_AI_GROUP_CHAT_GROUPS_BY_USER, ?MODULE, process_user_groups_iq,
                  IQDisc).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
                     ?NS_AI_GROUP_CHAT_NEW),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
                     ?NS_AI_GROUP_CHAT_UPDATE_GROUP_NAME),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
                     ?NS_AI_GROUP_CHAT_ADD_USERS),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
                     ?NS_AI_GROUP_CHAT_DEL_USERS),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
                     ?NS_AI_GROUP_CHAT_USERS),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
                     ?NS_AI_GROUP_CHAT_GROUPS_BY_USER).

%-------------------------------------------------------------------------

process_create_group_iq(From, _To, #iq{sub_el = SubEl} = IQ) ->
    CreatorUid = From#jid.luser,
    GroupName = xml:get_subtag_cdata(SubEl, <<"n">>),
    ?INFO_MSG("CreatorUid:~p, GroupName:~p~n", [CreatorUid, GroupName]),
    case ai_api:add_group(CreatorUid, GroupName) of
        #thriftGroup{groupId = Gid} ->
            SubEl0 = [#xmlel{name = <<"c">>, attrs = [], children = [{xmlcdata, ?AI_CODE_SUCCESS}]},
                      #xmlel{name = <<"r">>, attrs = [], children = [{xmlcdata, <<"OK">>}]},
                      #xmlel{name = <<"gid">>, attrs = [], children = [{xmlcdata, Gid}]}];
        _ ->
            SubEl0 = [#xmlel{name = <<"c">>, attrs = [], children = [{xmlcdata, ?AI_CODE_FAIL}]},
                      #xmlel{name = <<"r">>, attrs = [], children = [{xmlcdata, <<"fail">>}]}]
    end,
	Reply = IQ#iq{type = result, sub_el = [#xmlel{name = <<"query">>, attrs = [], children = SubEl0}]},
	?INFO_MSG("Reply:~p~n", [Reply]),
    Reply.

process_update_groupname_iq(From, _To, #iq{sub_el = SubEl} = IQ) ->
    GroupId = xml:get_subtag_cdata(SubEl, <<"gid">>),
    NewName = xml:get_subtag_cdata(SubEl, <<"nm">>),
    OperatorUid = From#jid.luser,
    SubEl0 = [#xmlel{name = <<"gid">>, attrs = [], children = [{xmlcdata, GroupId}]},
              #xmlel{name = <<"gname">>, attrs = [], children = [{xmlcdata, NewName}]}],
    ?INFO_MSG("GroupId:~p, NewName:~p, OperatorUid:~p~n", [GroupId, NewName, OperatorUid]),
    case ai_api:update_group_name(GroupId, NewName, OperatorUid) of
       true ->
            SubEl1 = [#xmlel{name = <<"c">>, children = [{xmlcdata, ?AI_CODE_SUCCESS}]},
                      #xmlel{name = <<"r">>, children = [{xmlcdata, <<"OK">>}]}],
            Subject = <<"groupNameUpdate">>,
            OpUser = xml:get_subtag_cdata(SubEl, <<"opUser">>),
            Body0 = [{"gid", GroupId}, {"name", NewName}, {"opUser", OpUser},
                    {"opUid", OperatorUid}, {"opType", <<"1">>}],
            Body1 = rfc4627:encode({obj, Body0}),
            broadcast_in_group(From, OperatorUid, GroupId, OperatorUid, OpUser, Subject, Body1);
       _ ->
            SubEl1 = [#xmlel{name = <<"c">>, children = [{xmlcdata, ?AI_CODE_FAIL}]},
                      #xmlel{name = <<"r">>, children = [{xmlcdata, <<"fail">>}]}]       
    end,
    SubEl2 = SubEl0 ++ SubEl1,
	Reply = IQ#iq{type = result, sub_el = [#xmlel{name = <<"query">>, attrs = [], children = SubEl2}]},
	?INFO_MSG("Reply:~p~n", [Reply]),
    Reply.

process_add_user_iq(From, _To, #iq{sub_el = SubEl} = IQ) ->
    GroupId = xml:get_subtag_cdata(SubEl, <<"gid">>),
    OperatorUid = From#jid.luser,
    UserJson = xml:get_subtag_cdata(SubEl, <<"us">>),
    ?INFO_MSG("GroupId:~p, OperatorUid:~p, UserJson:~p~n", [GroupId, OperatorUid, UserJson]),
    case ai_api:add_group_users(GroupId, UserJson, OperatorUid) of
        true ->
            case ai_api:get_group(GroupId) of
                #thriftGroup{name = GroupName, creatorUid = CreatorUid} ->
                    OpUser = xml:get_subtag_cdata(SubEl, <<"opUser">>),
					case rfc4627:decode(UserJson) of
						{ok, UsObject, _} -> US = UsObject;
						_ -> US = <<"">>
					end,
                    Body0 = [{"groupid", GroupId}, {"us", US},
                             {"groupName", GroupName}, {"createUid", CreatorUid},
                             {"operatorUid", OperatorUid}, {"opUser", OpUser},
                             {"opUid", OperatorUid}, {"opType", <<"2">>}],
                    Body1 = rfc4627:encode({obj, Body0}),
                    Subject = <<"group_addUsers">>,
                    broadcast_in_group(From, OperatorUid, GroupId, OperatorUid, OpUser, Subject, Body1),
                    SubEl0 = [#xmlel{name = <<"gid">>, children = [{xmlcdata, GroupId}]},
                              #xmlel{name = <<"c">>, children = [{xmlcdata, ?AI_CODE_SUCCESS}]},
                              #xmlel{name = <<"r">>, children = [{xmlcdata, <<"OK">>}]}];
                _ ->
                    SubEl0 = [#xmlel{name = <<"c">>, children = [{xmlcdata, ?AI_CODE_FAIL}]},
                              #xmlel{name = <<"r">>, children = [{xmlcdata, <<"fail">>}]}]        
            end;
        _ ->
            SubEl0 = [#xmlel{name = <<"c">>, children = [{xmlcdata, ?AI_CODE_FAIL}]},
                      #xmlel{name = <<"r">>, children = [{xmlcdata, <<"fail">>}]}]        
    end,
	Reply = IQ#iq{type = result, sub_el = [#xmlel{name = <<"query">>, attrs = [], children = SubEl0}]},
	?INFO_MSG("Reply:~p~n", [Reply]),
    Reply.

process_del_user_iq(From, _To, #iq{sub_el = SubEl} = IQ) ->
    GroupId = xml:get_subtag_cdata(SubEl, <<"gid">>),
    OperatorUid = From#jid.luser,
    DelUid = xml:get_subtag_cdata(SubEl, <<"uid">>),
    Name = xml:get_subtag_cdata(SubEl, <<"nm">>),
    ?INFO_MSG("GroupId:~p, OperatorUid:~p, DelUid:~p, Name~p~n", [GroupId, OperatorUid, DelUid, Name]),
    case ai_api:del_group_user(GroupId, DelUid, OperatorUid) of
        true ->
            case ai_api:get_group(GroupId) of
                #thriftGroup{name = GroupName, creatorUid = CreatorUid} ->
                    OpUser = xml:get_subtag_cdata(SubEl, <<"opUser">>),
                    Body0 = [{"gid", GroupId}, {"uid", DelUid},
                             {"name", Name}, {"groupName", GroupName},
                             {"createUid", CreatorUid}, {"operatorUid", OperatorUid},
                             {"opUser", OpUser}, {"opUid", OperatorUid},
                             {"opType", <<"3">>}],
                    Body1 = rfc4627:encode({obj, Body0}),
                    Subject = <<"group_userdel">>,
                    broadcast_in_group(From, OperatorUid, GroupId, DelUid, OpUser, Subject, Body1),
                    SubEl0 = [#xmlel{name = <<"gid">>, children = [{xmlcdata, GroupId}]},
                              #xmlel{name = <<"uid">>, children = [{xmlcdata, DelUid}]},
                              #xmlel{name = <<"c">>, children = [{xmlcdata, ?AI_CODE_SUCCESS}]},
                              #xmlel{name = <<"r">>, children = [{xmlcdata, <<"OK">>}]}];
                _ ->
                    SubEl0 = [#xmlel{name = <<"c">>, children = [{xmlcdata, ?AI_CODE_FAIL}]},
                              #xmlel{name = <<"r">>, children = [{xmlcdata, <<"fail">>}]}]        
            end;
        _Res ->
            SubEl0 = [#xmlel{name = <<"c">>, children = [{xmlcdata, ?AI_CODE_FAIL}]},
                      #xmlel{name = <<"r">>, children = [{xmlcdata, <<"fail">>}]}]        
    end,
	Reply = IQ#iq{type = result, sub_el = [#xmlel{name = <<"query">>, attrs = [], children = SubEl0}]},
	?INFO_MSG("Reply:~p~n", [Reply]),
    Reply.

process_users_iq(_From, _To, #iq{sub_el = SubEl} = IQ) ->
    GroupId = xml:get_subtag_cdata(SubEl, <<"gid">>),
    ?INFO_MSG("GroupId:~p~n", [GroupId]),
    case ai_api:get_group_members_json(GroupId) of
        [] ->
            SubEl0 = [#xmlel{name = <<"gid">>, children = [{xmlcdata, GroupId}]},
                      #xmlel{name = <<"c">>, children = [{xmlcdata, ?AI_CODE_NODATA}]},
                      #xmlel{name = <<"us">>}];        
        Members ->
			?DEBUG("GroupId:~p, Members:~p~n", [GroupId, Members]),
            SubEl0 = [#xmlel{name = <<"gid">>, children = [{xmlcdata, GroupId}]},
                      #xmlel{name = <<"c">>, children = [{xmlcdata, ?AI_CODE_SUCCESS}]},
                      #xmlel{name = <<"us">>, children = [{xmlcdata, Members}]}]
    end,
	Reply = IQ#iq{type = result, sub_el = [#xmlel{name = <<"query">>, attrs = [], children = SubEl0}]},
	?DEBUG("Reply:~p~n", [Reply]),
    Reply.

process_user_groups_iq(From, _To, #iq{sub_el = _SubEl} = IQ) ->
    Uid = From#jid.luser,
    case ai_api:get_groups_by_uid(Uid) of
        [] ->
            SubEl0 = [#xmlel{name = <<"uid">>, children = [{xmlcdata, Uid}]},
                      #xmlel{name = <<"c">>, children = [{xmlcdata, ?AI_CODE_FAIL}]},
                      #xmlel{name = <<"r">>, children = [{xmlcdata, <<"fail">>}]},
                      #xmlel{name = <<"gs">>, children = [{xmlcdata, <<"noData">>}]}];
        Groups ->
        ?DEBUG("Uid:~p, Groups:~p~n", [Uid, Groups]),
              SubEl0 = [#xmlel{name = <<"uid">>, children = [{xmlcdata, Uid}]},
                        #xmlel{name = <<"c">>, children = [{xmlcdata, ?AI_CODE_SUCCESS}]},
                        #xmlel{name = <<"r">>, children = [{xmlcdata, <<"OK">>}]},
                        #xmlel{name = <<"gs">>, children = [{xmlcdata, Groups}]}]
    end,
	Reply = IQ#iq{type = result, sub_el = [#xmlel{name = <<"query">>, attrs = [], children = SubEl0}]},
	?DEBUG("Reply:~p~n", [Reply]),
    Reply.

-spec broadcast_in_group(jid(), string() | binary(), string() | binary(), string() | binary(), string() | binary(), string() | binary(), string() | binary()) -> any().
broadcast_in_group(From, Uid, GroupId, ExtraUid, OpUser, Subject, Body) ->
    case ai_api:get_group_members(GroupId) of
        [] ->
            ok;
        Members ->
            if ExtraUid =:= <<>>; ExtraUid =:= <<"">> ->
              AllMembers = Members;
            true ->
              AllMembers = [ExtraUid | Members]
            end,
			?INFO_MSG("From:~p, Uid:~p, GroupId:~p, Members:~p~n", [From, Uid, GroupId, AllMembers]),
            ServerJID = #jid{user = Uid, server = From#jid.lserver, resource = From#jid.lresource,
						luser = Uid, lserver = From#jid.lserver, lresource = From#jid.lresource},
            lists:foreach(fun(MemberUid) when (MemberUid =/= Uid) ->
								case mod_ai_robot:is_robot(MemberUid) of
									false -> 
										case ejabberd_sm:get_user_jids(MemberUid) of
											[{_LUser, LServer, LResource} | _] -> 
												Server = LServer,
												Resource = LResource,
												Online = 1;
											_ ->
												Server = From#jid.lserver,
												Resource = From#jid.lresource,
												Online = 0
										end,
                                        To = #jid{user = MemberUid, server = Server, resource = Resource, 
												luser = MemberUid, lserver = Server, lresource = Resource},
                                        SubEl = [#xmlel{name = <<"subject">>, attrs = [], children = [{xmlcdata, iolist_to_binary(Subject)}]},
                                                 #xmlel{name = <<"body">>, attrs = [], children = [{xmlcdata, iolist_to_binary(Body)}]}],
										Message = #xmlel{name = <<"message">>,
	                                                     attrs = [{<<"to">>, jlib:jid_to_string(To)},
	                                                              {<<"from">>, jlib:jid_to_string(ServerJID)},
                                                                {<<"type">>, ?AI_MESSAGE_CHAT_TYPE_SINGLE},
	                                                              {<<"mtype">>, ?AI_MESSAGE_MTYPE_GROUP_SYSTEM_MESSAGE},
	                                                              {<<"id">>, mod_ai_msg:uuid()},
	                                                              {<<"opUser">>, OpUser}],
	                                                     children = SubEl},
										case Online of
											1 -> mod_ai_msg:route(ServerJID, To, Message, noack);
											0 -> mod_ai_msg:store_offline_msg(ServerJID, To, Message)
										end;
									true -> ok
								end;
						  (_Other) -> ok
                          end,
              AllMembers)
    end,
    ok.
