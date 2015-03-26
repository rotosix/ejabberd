%%
%% Autogenerated by Thrift Compiler (0.9.1)
%%
%% DO NOT EDIT UNLESS YOU ARE SURE THAT YOU KNOW WHAT YOU ARE DOING
%%

-module(thriftService_thrift).
-behaviour(thrift_service).


-include("thriftService_thrift.hrl").

-export([struct_info/1, function_info/2]).

struct_info('i am a dummy struct') -> undefined.
%%% interface
% checkPassword(This, Uid, Password)
function_info('checkPassword', params_type) ->
  {struct, [{1, string},
          {2, string}]}
;
function_info('checkPassword', reply_type) ->
  bool;
function_info('checkPassword', exceptions) ->
  {struct, []}
;
% updateOnlineState(This, Uid, Online)
function_info('updateOnlineState', params_type) ->
  {struct, [{1, string},
          {2, i32}]}
;
function_info('updateOnlineState', reply_type) ->
  {struct, []};
function_info('updateOnlineState', exceptions) ->
  {struct, []}
;
% storeOfflineMsg(This, Sid, Rid, Gid, Msgid, MsgType, MsgStatus, Content, Url)
function_info('storeOfflineMsg', params_type) ->
  {struct, [{1, string},
          {2, string},
          {3, string},
          {4, string},
          {5, string},
          {6, i32},
          {7, string},
          {8, string}]}
;
function_info('storeOfflineMsg', reply_type) ->
  {struct, []};
function_info('storeOfflineMsg', exceptions) ->
  {struct, []}
;
% getOfflineMsg(This, Rid)
function_info('getOfflineMsg', params_type) ->
  {struct, [{1, string}]}
;
function_info('getOfflineMsg', reply_type) ->
  string;
function_info('getOfflineMsg', exceptions) ->
  {struct, []}
;
% setUserOfflineMsgNum(This, Uid, Num)
function_info('setUserOfflineMsgNum', params_type) ->
  {struct, [{1, string},
          {2, i32}]}
;
function_info('setUserOfflineMsgNum', reply_type) ->
  {struct, []};
function_info('setUserOfflineMsgNum', exceptions) ->
  {struct, []}
;
% getGameSessionId(This, Uid)
function_info('getGameSessionId', params_type) ->
  {struct, [{1, string}]}
;
function_info('getGameSessionId', reply_type) ->
  string;
function_info('getGameSessionId', exceptions) ->
  {struct, []}
;
% createGame(This, SessionId, GameType, Uid, Jid, Rid, Question, State)
function_info('createGame', params_type) ->
  {struct, [{1, string},
          {2, string},
          {3, string},
          {4, string},
          {5, string},
          {6, string},
          {7, string}]}
;
function_info('createGame', reply_type) ->
  string;
function_info('createGame', exceptions) ->
  {struct, []}
;
% joinGame(This, SessionId, GameType, Uid, Jid, Rid)
function_info('joinGame', params_type) ->
  {struct, [{1, string},
          {2, string},
          {3, string},
          {4, string},
          {5, string}]}
;
function_info('joinGame', reply_type) ->
  string;
function_info('joinGame', exceptions) ->
  {struct, []}
;
% cancelGame(This, SessionId, GameType, Uid, Jid)
function_info('cancelGame', params_type) ->
  {struct, [{1, string},
          {2, string},
          {3, string},
          {4, string}]}
;
function_info('cancelGame', reply_type) ->
  string;
function_info('cancelGame', exceptions) ->
  {struct, []}
;
% updateGameState(This, SessionId, GameType, Uid, Jid, State, EndFlag)
function_info('updateGameState', params_type) ->
  {struct, [{1, string},
          {2, string},
          {3, string},
          {4, string},
          {5, string},
          {6, i32}]}
;
function_info('updateGameState', reply_type) ->
  string;
function_info('updateGameState', exceptions) ->
  {struct, []}
;
% getGameState(This, SessionId, GameType, Uid)
function_info('getGameState', params_type) ->
  {struct, [{1, string},
          {2, string},
          {3, string}]}
;
function_info('getGameState', reply_type) ->
  string;
function_info('getGameState', exceptions) ->
  {struct, []}
;
% joinFeedGame(This, SessionId, GameType, Uid, Jid, Rid, Puid)
function_info('joinFeedGame', params_type) ->
  {struct, [{1, string},
          {2, string},
          {3, string},
          {4, string},
          {5, string},
          {6, string}]}
;
function_info('joinFeedGame', reply_type) ->
  string;
function_info('joinFeedGame', exceptions) ->
  {struct, []}
;
% cancelFeedGame(This, SessionId, GameType, Uid, Jid, Puid)
function_info('cancelFeedGame', params_type) ->
  {struct, [{1, string},
          {2, string},
          {3, string},
          {4, string},
          {5, string}]}
;
function_info('cancelFeedGame', reply_type) ->
  string;
function_info('cancelFeedGame', exceptions) ->
  {struct, []}
;
% updateFeedGameState(This, SessionId, GameType, Uid, Jid, State, EndFlag, Puid)
function_info('updateFeedGameState', params_type) ->
  {struct, [{1, string},
          {2, string},
          {3, string},
          {4, string},
          {5, string},
          {6, i32},
          {7, string}]}
;
function_info('updateFeedGameState', reply_type) ->
  string;
function_info('updateFeedGameState', exceptions) ->
  {struct, []}
;
% getContactBadge(This, Uid, StartTime)
function_info('getContactBadge', params_type) ->
  {struct, [{1, string},
          {2, i32}]}
;
function_info('getContactBadge', reply_type) ->
  string;
function_info('getContactBadge', exceptions) ->
  {struct, []}
;
% getVisitorBadge(This, Uid, StartTime)
function_info('getVisitorBadge', params_type) ->
  {struct, [{1, string},
          {2, i32}]}
;
function_info('getVisitorBadge', reply_type) ->
  string;
function_info('getVisitorBadge', exceptions) ->
  {struct, []}
;
% getFeedBadge(This, Uid, StartTime)
function_info('getFeedBadge', params_type) ->
  {struct, [{1, string},
          {2, i32}]}
;
function_info('getFeedBadge', reply_type) ->
  string;
function_info('getFeedBadge', exceptions) ->
  {struct, []}
;
% getShakingUser(This, Uid, Lon, Lat, GameType, QueryTimes)
function_info('getShakingUser', params_type) ->
  {struct, [{1, string},
          {2, double},
          {3, double},
          {4, string},
          {5, i32}]}
;
function_info('getShakingUser', reply_type) ->
  string;
function_info('getShakingUser', exceptions) ->
  {struct, []}
;
% cancelShaking(This, Uid, Lon, Lat, GameType)
function_info('cancelShaking', params_type) ->
  {struct, [{1, string},
          {2, double},
          {3, double},
          {4, string}]}
;
function_info('cancelShaking', reply_type) ->
  bool;
function_info('cancelShaking', exceptions) ->
  {struct, []}
;
% checkAndGetUserUidByToken(This, Token)
function_info('checkAndGetUserUidByToken', params_type) ->
  {struct, [{1, string}]}
;
function_info('checkAndGetUserUidByToken', reply_type) ->
  string;
function_info('checkAndGetUserUidByToken', exceptions) ->
  {struct, []}
;
% addGroup(This, CreatorUid, GroupName)
function_info('addGroup', params_type) ->
  {struct, [{1, string},
          {2, string}]}
;
function_info('addGroup', reply_type) ->
  {struct, {'service_types', 'thriftGroup'}};
function_info('addGroup', exceptions) ->
  {struct, []}
;
% getGroup(This, GroupId)
function_info('getGroup', params_type) ->
  {struct, [{1, string}]}
;
function_info('getGroup', reply_type) ->
  {struct, {'service_types', 'thriftGroup'}};
function_info('getGroup', exceptions) ->
  {struct, []}
;
% getGroupMembersJson(This, GroupId)
function_info('getGroupMembersJson', params_type) ->
  {struct, [{1, string}]}
;
function_info('getGroupMembersJson', reply_type) ->
  string;
function_info('getGroupMembersJson', exceptions) ->
  {struct, []}
;
% updateGroupName(This, GroupId, NewName, OperatorUid)
function_info('updateGroupName', params_type) ->
  {struct, [{1, string},
          {2, string},
          {3, string}]}
;
function_info('updateGroupName', reply_type) ->
  bool;
function_info('updateGroupName', exceptions) ->
  {struct, []}
;
% addGroupUsers(This, GroupId, UserJson, OperatorUid)
function_info('addGroupUsers', params_type) ->
  {struct, [{1, string},
          {2, string},
          {3, string}]}
;
function_info('addGroupUsers', reply_type) ->
  bool;
function_info('addGroupUsers', exceptions) ->
  {struct, []}
;
% delGroupUser(This, GroupId, Uid, OperatorUid)
function_info('delGroupUser', params_type) ->
  {struct, [{1, string},
          {2, string},
          {3, string}]}
;
function_info('delGroupUser', reply_type) ->
  bool;
function_info('delGroupUser', exceptions) ->
  {struct, []}
;
% getGroupMembers(This, GroupId)
function_info('getGroupMembers', params_type) ->
  {struct, [{1, string}]}
;
function_info('getGroupMembers', reply_type) ->
  {list, string};
function_info('getGroupMembers', exceptions) ->
  {struct, []}
;
% getGroupsByUid(This, Uid)
function_info('getGroupsByUid', params_type) ->
  {struct, [{1, string}]}
;
function_info('getGroupsByUid', reply_type) ->
  string;
function_info('getGroupsByUid', exceptions) ->
  {struct, []}
;
% getGroupForbidUids(This)
function_info('getGroupForbidUids', params_type) ->
  {struct, []}
;
function_info('getGroupForbidUids', reply_type) ->
  {list, string};
function_info('getGroupForbidUids', exceptions) ->
  {struct, []}
;
% forbidUser(This, Uid, ForbidType, DiffTime, Count, AvarageCount, Content)
function_info('forbidUser', params_type) ->
  {struct, [{1, string},
          {2, i32},
          {3, i32},
          {4, i32},
          {5, double},
          {6, string}]}
;
function_info('forbidUser', reply_type) ->
  {struct, []};
function_info('forbidUser', exceptions) ->
  {struct, []}
;
% storeMsg(This, Sid, Rid, Gid, Msgid, MsgType, MsgStatus, Content, Url)
function_info('storeMsg', params_type) ->
  {struct, [{1, string},
          {2, string},
          {3, string},
          {4, string},
          {5, string},
          {6, i32},
          {7, string},
          {8, string}]}
;
function_info('storeMsg', reply_type) ->
  {struct, []};
function_info('storeMsg', exceptions) ->
  {struct, []}
;
% addAndgetServerForUser(This, Uid)
function_info('addAndgetServerForUser', params_type) ->
  {struct, [{1, string}]}
;
function_info('addAndgetServerForUser', reply_type) ->
  string;
function_info('addAndgetServerForUser', exceptions) ->
  {struct, []}
;
% updateUserRoute(This, Uid, ChatDomain)
function_info('updateUserRoute', params_type) ->
  {struct, [{1, string},
          {2, string}]}
;
function_info('updateUserRoute', reply_type) ->
  {struct, []};
function_info('updateUserRoute', exceptions) ->
  {struct, []}
;
% getServerForUser(This, Uid)
function_info('getServerForUser', params_type) ->
  {struct, [{1, string}]}
;
function_info('getServerForUser', reply_type) ->
  string;
function_info('getServerForUser', exceptions) ->
  {struct, []}
;
% addServer(This, ChatDomain, ServerInfo)
function_info('addServer', params_type) ->
  {struct, [{1, string},
          {2, string}]}
;
function_info('addServer', reply_type) ->
  {struct, []};
function_info('addServer', exceptions) ->
  {struct, []}
;
% pushMsg(This, MsgType, Sid, Rid, Gid, Url, Content, Badge, PushToken, MsgTime)
function_info('pushMsg', params_type) ->
  {struct, [{1, string},
          {2, string},
          {3, string},
          {4, string},
          {5, string},
          {6, string},
          {7, i32},
          {8, string},
          {9, string}]}
;
function_info('pushMsg', reply_type) ->
  {struct, []};
function_info('pushMsg', exceptions) ->
  {struct, []}
;
% proxyMessage(This, Id, Type, To, From, Fromid, Gid, Mtype, Body, Subject)
function_info('proxyMessage', params_type) ->
  {struct, [{1, string},
          {2, string},
          {3, string},
          {4, string},
          {5, string},
          {6, string},
          {7, string},
          {8, string},
          {9, string}]}
;
function_info('proxyMessage', reply_type) ->
  {struct, []};
function_info('proxyMessage', exceptions) ->
  {struct, []}
;
function_info(_Func, _Info) -> no_function.
