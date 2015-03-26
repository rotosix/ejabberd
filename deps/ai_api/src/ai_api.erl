%% @author james
%% @doc @todo Add description to ai_api_service.


-module(ai_api).

-author('james.wu@asiainnovations.com').

-compile(export_all).

-include("service_types.hrl").
-include("service_constants.hrl").
-include("thriftService_thrift.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
%%-export([]).

%% ================================== call service start  ==================================
-spec check_password(Uid::string() | binary(), Password::string() | binary()) -> boolean().
check_password(Uid, Password) ->
	ai_api_thrift:call(service, 'checkPassword', [Uid, Password]).

-spec update_online_state(Uid::string() | binary(), Online::integer()) -> any().
update_online_state(Uid, Online) ->
	ai_api_thrift:cast(service, 'updateOnlineState', [Uid, Online]).

-spec store_offline_msg(Sid::string() | binary(), Rid::string() | binary(), Gid::string() | binary(),
						Msgid::string() | binary(), MsgType::string() | binary(), MsgStatus::integer(),
						Content::string() | binary(), Url::string() | binary()) -> any().
store_offline_msg(Sid, Rid, Gid, Msgid, MsgType, MsgStatus, Content, Url) ->
	ai_api_thrift:cast(service, 'storeOfflineMsg', [Sid, Rid, Gid, Msgid, MsgType, MsgStatus, Content, Url]).

-spec get_offline_msg(Rid::string() | binary()) -> string() | binary().
get_offline_msg(Rid) ->
	ai_api_thrift:call(service, 'getOfflineMsg', [Rid]).

-spec set_user_offline_msg_num(Uid::string() | binary(), Num::integer()) -> any().
set_user_offline_msg_num(Uid, Num) ->
	ai_api_thrift:cast(service, 'setUserOfflineMsgNum', [Uid, Num]).

-spec get_game_session_id(Uid::string() | binary()) -> string() | binary().
get_game_session_id(Uid) ->
	ai_api_thrift:call(service, 'getGameSessionId', [Uid]).

-spec create_game(SessionId::string() | binary() | nil, GameType::string() | binary(), Uid::string() | binary(),
				  Jid::string() | binary(), Rid::string() | binary(), Question::string() | binary(), State::string() | binary()) -> string() | binary() | nil.
create_game(SessionId, GameType, Uid, Jid, Rid, Question, State) ->
	ai_api_thrift:call(service, 'createGame', [SessionId, GameType, Uid, Jid, Rid, Question, State]).

-spec join_game(SessionId::string() | binary(), GameType::string() | binary(), Uid::string() | binary(),
				  Jid::string() | binary(), Rid::string() | binary()) -> string() | binary() | nil.
join_game(SessionId, GameType, Uid, Jid, Rid) ->
	ai_api_thrift:call(service, 'joinGame', [SessionId, GameType, Uid, Jid, Rid]).

-spec cancel_game(SessionId::string() | binary(), GameType::string() | binary(), Uid::string() | binary(),
				  Jid::string() | binary()) -> string() | binary() | nil.
cancel_game(SessionId, GameType, Uid, Jid) ->
	ai_api_thrift:call(service, 'cancelGame', [SessionId, GameType, Uid, Jid]).

-spec update_game_state(SessionId::string() | binary(), GameType::string() | binary(), Uid::string() | binary(),
				  Jid::string() | binary(), State::string() | binary(), EndFlag::integer()) -> string() | binary() | nil.
update_game_state(SessionId, GameType, Uid, Jid, State, EndFlag) ->
	ai_api_thrift:call(service, 'updateGameState', [SessionId, GameType, Uid, Jid, State, EndFlag]).

-spec get_game_state(SessionId::string() | binary(), GameType::string() | binary(), Uid::string() | binary()) -> string() | binary() | nil.
get_game_state(SessionId, GameType, Uid) ->
	ai_api_thrift:call(service, 'getGameState', [SessionId, GameType, Uid]).

-spec join_feed_game(SessionId::string() | binary(), GameType::string() | binary(), Uid::string() | binary(),
				  Jid::string() | binary(), Rid::string() | binary(), Puid::string() | binary()) -> string() | binary() | nil.
join_feed_game(SessionId, GameType, Uid, Jid, Rid, Puid) ->
	ai_api_thrift:call(service, 'joinFeedGame', [SessionId, GameType, Uid, Jid, Rid, Puid]).

-spec cancel_feed_game(SessionId::string() | binary(), GameType::string() | binary(), Uid::string() | binary(),
				  Jid::string() | binary(), Puid::string() | binary()) -> string() | binary() | nil.
cancel_feed_game(SessionId, GameType, Uid, Jid, Puid) ->
	ai_api_thrift:call(service, 'cancelFeedGame', [SessionId, GameType, Uid, Jid, Puid]).

-spec update_feed_game_state(SessionId::string() | binary(), GameType::string() | binary(), Uid::string() | binary(),
				  Jid::string() | binary(), State::string() | binary(), EndFlag::integer(), Puid::string() | binary()) -> string() | binary() | nil.
update_feed_game_state(SessionId, GameType, Uid, Jid, State, EndFlag, Puid) ->
	ai_api_thrift:call(service, 'updateFeedGameState', [SessionId, GameType, Uid, Jid, State, EndFlag, Puid]).

-spec get_contact_badge(Uid::string() | binary(), StartTime::integer()) -> string() | nil.
get_contact_badge(Uid, StartTime) ->
	ai_api_thrift:call(service, 'getContactBadge', [Uid, StartTime]).

-spec get_visitor_badge(Uid::string() | binary(), StartTime::integer()) -> string() | nil.
get_visitor_badge(Uid, StartTime) ->
	ai_api_thrift:call(service, 'getVisitorBadge', [Uid, StartTime]).

-spec get_feed_badge(Uid::string() | binary(), StartTime::integer()) -> string() | nil.
get_feed_badge(Uid, StartTime) ->
	ai_api_thrift:call(service, 'getFeedBadge', [Uid, StartTime]).

-spec get_shaking_user(Uid::string() | binary(), Lon::float(), Lat::float(), GameType::string() | binary(), QueryTimes::integer()) -> any().
get_shaking_user(Uid, Lon, Lat, GameType, QueryTimes) ->
	ai_api_thrift:call(service, 'getShakingUser', [Uid, Lon, Lat, GameType, QueryTimes]).

-spec cancel_shaking(Uid::string() | binary(), Lon::float(), Lat::float(), GameType::string() | binary()) -> any().
cancel_shaking(Uid, Lon, Lat, GameType) ->
	ai_api_thrift:call(service, 'cancelShaking', [Uid, Lon, Lat, GameType]).

-spec check_and_get_uid_by_token(Token::string() | binary()) -> string() | binary() | nil.
check_and_get_uid_by_token(Token) ->
	ai_api_thrift:call(service, 'checkAndGetUserUidByToken', [Token]).

-spec add_group(CreatorUid::string() | binary(), Name::string() | binary()) -> any().
add_group(CreatorUid, Name) ->
	ai_api_thrift:call(service, 'addGroup', [CreatorUid, Name]).

-spec get_group(GroupId::string() | binary()) -> any().
get_group(GroupId) ->
	ai_api_thrift:call(service, 'getGroup', [GroupId]).

-spec get_group_members_json(GroupId::string() | binary()) -> string() | binary().
get_group_members_json(GroupId) ->
	ai_api_thrift:call(service, 'getGroupMembersJson', [GroupId]).

-spec update_group_name(GroupId::string() | binary(), NewName::string() | binary(), Uid::string() | binary()) -> any().
update_group_name(GroupId, NewName, Uid) ->
	ai_api_thrift:call(service, 'updateGroupName', [GroupId, NewName, Uid]).

-spec updateGroupCreator(GroupId::string() | binary(), CreatorUid::string() | binary()) -> any().
updateGroupCreator(GroupId, CreatorUid) ->
	ai_api_thrift:call(service, 'updateGroupCreator', [GroupId, CreatorUid]).

-spec add_group_users(GroupId::string() | binary(), Users::string() | binary(), Uid::string() | binary()) -> boolean().
add_group_users(GroupId, Users, Uid) ->
	ai_api_thrift:call(service, 'addGroupUsers', [GroupId, Users, Uid]).

-spec del_group_user(GroupId::string() | binary(), Uid::string() | binary(), OperatorUid::string() | binary()) -> boolean().
del_group_user(GroupId, Uid, OperatorUid) ->
	ai_api_thrift:call(service, 'delGroupUser', [GroupId, Uid, OperatorUid]).

-spec get_group_members(GroupId::string() | binary()) -> list(string()) | nil.
get_group_members(GroupId) ->
	ai_api_thrift:call(service, 'getGroupMembers', [GroupId]).

-spec get_groups_by_uid(Uid::string() | binary()) -> string() | nil.
get_groups_by_uid(Uid) ->
	ai_api_thrift:call(service, 'getGroupsByUid', [Uid]).

-spec get_group_forbid_uids() -> list(string) | nil.
get_group_forbid_uids() ->
	ai_api_thrift:call(service, 'getGroupForbidUids', []).

-spec set_forbidden_user(Uid::string() | binary(), ForbidType::integer(), DiffTime::integer(), Count::integer(), AvarageCount::float(), Content::string() | binary()) -> any().
set_forbidden_user(Uid, ForbidType, DiffTime, Count, AvarageCount, Content) ->
  ai_api_thrift:cast(service, 'forbidUser', [Uid, ForbidType, DiffTime, Count, AvarageCount, Content]).

%% ================================== call service end  ==================================



%% ================================== call mongo start  ==================================
-spec store_msg(Sid::string() | binary(), Rid::string() | binary(), Gid::string() | binary(),
				Msgid::string() | binary(), MsgType::string() | binary(), MsgStatus::integer(),
				Content::string() | binary(), Url::string() | binary()) -> any().
store_msg(Sid, Rid, Gid, Msgid, MsgType, MsgStatus, Content, Url) ->
	ai_api_thrift:cast(mongo, 'storeMsg', [Sid, Rid, Gid, Msgid, MsgType, MsgStatus, Content, Url]).
%% ================================== call mongo start  ==================================


%% ================================== call apns start  ==================================
-spec push_msg(Type::string() | binary(), Sid::string() | binary(), Rid::string() | binary(), Gid::string() | binary(),
			   Url::string() | binary(), Content::string() | binary(), Badge::integer(),
			   PushToken::string() | binary(), MsgTime::string() | binary()) -> any().
push_msg(Type, Sid, Rid, Gid, Url, Content, Badge, PushToken, MsgTime) ->
	ai_api_thrift:cast(apns, 'pushMsg', [Type, Sid, Rid, Gid, Url, Content, Badge, PushToken, MsgTime]).
%% ================================== call apns start  ==================================



%% ================================== call router start  ==================================
-spec add_and_get_server_for_user(Uid::string() | binary()) -> string() | binary() | nil.
add_and_get_server_for_user(Uid) ->
	ai_api_thrift:call(router, 'addAndgetServerForUser', [Uid]).

-spec update_user_route(Uid::string() | binary(), ChatDomain::string() | binary()) -> any().
update_user_route(Uid, ChatDomain) ->
	ai_api_thrift:cast(router, 'updateUserRoute', [Uid, ChatDomain]).

-spec get_server_for_user(Uid::string() | binary()) -> string() | binary() | nil.
get_server_for_user(Uid) ->
	ai_api_thrift:call(router, 'getServerForUser', [Uid]).

-spec add_server(ChatDomain::string() | binary(), ServerInfo::string() | binary()) -> any().
add_server(ChatDomain, ServerInfo) ->
	ai_api_thrift:cast(router, 'addServer', [ChatDomain, ServerInfo]).
%% ================================== call router end  ==================================



%% ================================== call robot start  ==================================
-spec proxy_msg(Id::string() | binary(), Type::string() | binary(), To::string() | binary(), From::string() | binary(),
			   Fromid::string() | binary(), Gid::string() | binary(), Mtype::string() | binary(),
			   Body::string() | binary(), Subject::string() | binary()) -> any().
proxy_msg(Id, Type, To, From, Fromid, Gid, Mtype, Body, Subject) ->
	ai_api_thrift:cast(robot, 'proxyMessage', [Id, Type, To, From, Fromid, Gid, Mtype, Body, Subject]).
%% ================================== call robot start  ==================================

