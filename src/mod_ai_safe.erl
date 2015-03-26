%%%----------------------------------------------------------------------
%%% File    : mod_ai_msg.erl
%%% Author  : wugaoping<james.wu@asiainnovations.com>
%%%
%%%----------------------------------------------------------------------

-module(mod_ai_safe).

-author('james.wu@asiainnovations.com').

-behaviour(gen_mod).

%% gen_mod callbacks
-export([start/2, stop/1]).

%% hook handlers
%%-export([user_send_packet/3]).

%% Api functions
-export([check_send_permit/2, check_do_packet_permit/2, kickoff_user/1]).

%%-record(user_send_count_info, {uid, count, time}).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").
-include("ai.hrl").

start(_Host, _Opts) ->
  case ets:info(?AI_USER_SEND_MESSAGE_COUNT_TABLE) of
    undefined ->
      ets:new(?AI_USER_SEND_MESSAGE_COUNT_TABLE, [named_table, set, public]);
    _ -> ok
  end,
	ok.

stop(_Host) ->
  case ets:info(?AI_USER_SEND_MESSAGE_COUNT_TABLE) of
    undefined -> ok;
    _ ->
      ets:delete(?AI_USER_SEND_MESSAGE_COUNT_TABLE)
  end,
	ok.

%-------------------------------------------------------------------------

%%check_do_packet_permit(From, Packet=#xmlel{name = <<"message">>}) ->
%%  Uid = From#jid.luser,
%%  NeedCheck = is_need_check_msg(From, Packet),
%%  if NeedCheck =:= false  -> true;
%%  true ->
%%    case ets:lookup(?AI_USER_SEND_MESSAGE_COUNT_TABLE, Uid) of
%%      [] ->
%%        CurrentTime = ai_utils:get_time(),
%%        ets:insert(?AI_USER_SEND_MESSAGE_COUNT_TABLE, {Uid, 1, CurrentTime});
%%      [{Uid, Count, Time} | _] ->
%%        CurrentTime = ai_utils:get_time(),
%%        DiffTime = CurrentTime - Time + 1,
%%        AvarageCount = (Count * 60) / DiffTime,

%%        if DiffTime >= 5, AvarageCount >= 120 ->
%%          Overload = true;
%%        true ->
%%          Overload = false
%%        end,

%%        if Overload =:= true ->
%%          OverloadContent = xml:get_subtag_cdata(Packet, <<"body">>),
%%          ai_api:set_forbidden_user(Uid, ?AI_USER_SEND_MESSAGE_OVERLOAD_WARN, DiffTime, Count, AvarageCount, OverloadContent),

%%          ?DEBUG("overload Uid:~p, DiffTime:~p, Count:~p, AvarageCount:~p, OverloadContent:~p~n",
%%            [Uid, DiffTime, Count, AvarageCount, OverloadContent]);
%%        true -> ok
%%        end,

%%        if DiffTime >= 60, AvarageCount >= 240 ->
%%          Forbidden = true;
%%        DiffTime >= 40, AvarageCount >= 360 ->
%%          Forbidden = true;
%%        DiffTime >= 20, AvarageCount >= 480 ->
%%          Forbidden = true;
%%        DiffTime >= 5, AvarageCount >= 600 ->
%%          Forbidden = true;
%%        DiffTime >= 1, AvarageCount >= 7200 ->
%%          Forbidden = true;
%%        true ->
%%          Forbidden = false
%%        end,

%%        if Forbidden =:= true ->
%%          ForbiddenContent = xml:get_subtag_cdata(Packet, <<"body">>),
%%          ai_api:set_forbidden_user(Uid, ?AI_USER_SEND_MESSAGE_FORBID_USER, DiffTime, Count, AvarageCount, ForbiddenContent),
%%          kickoff_user(Uid),
%%          ets:delete(?AI_USER_SEND_MESSAGE_COUNT_TABLE, Uid),
%%          Pass = false,

%%          ?WARNING_MSG("kickoff Uid:~p, DiffTime:~p, Count:~p, AvarageCount:~p, permit:~p, ForbiddenContent:~p~n",
%%            [Uid, DiffTime, Count, AvarageCount, ?AI_USER_SEND_MESSAGE_COUNT_FORBID, ForbiddenContent]);
%%        true ->
%%          if DiffTime > 60 ->
%%            ets:insert(?AI_USER_SEND_MESSAGE_COUNT_TABLE, {Uid, 1, CurrentTime});
%%          true ->
%%            ets:insert(?AI_USER_SEND_MESSAGE_COUNT_TABLE, {Uid, Count + 1, Time})
%%          end,

%%          if DiffTime >= 60, AvarageCount >= ?AI_USER_SEND_MESSAGE_COUNT_DROP ->
%%            Pass = false;
%%          DiffTime >= 40, AvarageCount >= (?AI_USER_SEND_MESSAGE_COUNT_DROP * 2) ->
%%            Pass = false;
%%          DiffTime >= 20, AvarageCount >= (?AI_USER_SEND_MESSAGE_COUNT_DROP * 3) ->
%%            Pass = false;
%%          true ->
%%            Pass = true
%%          end
%%        end,
%%        Pass
%%    end
%%  end;
check_do_packet_permit(_From, _Packet) -> true.

check_send_permit(From, Packet=#xmlel{name = <<"message">>}) ->
%%  Uid = From#jid.luser,
%%  NeedCheck = is_need_check_msg(From, Packet),
%%  if NeedCheck =:= false  -> Pass = true;
%%  true ->
%%    case ets:lookup(?AI_USER_SEND_MESSAGE_COUNT_TABLE, Uid) of
%%      [] -> Pass = false;
%%      [{Uid, Count, Time} | _] ->
%%        CurrentTime = ai_utils:get_time(),
%%        DiffTime = CurrentTime - Time + 1,
%%        AvarageCount = (Count * 60) / DiffTime,

%%        if DiffTime >= 60, AvarageCount >= ?AI_USER_SEND_MESSAGE_COUNT_DROP ->
%%          Pass = false;
%%          DiffTime >= 40, AvarageCount >= (?AI_USER_SEND_MESSAGE_COUNT_DROP * 2) ->
%%            Pass = false;
%%          DiffTime >= 20, AvarageCount >= (?AI_USER_SEND_MESSAGE_COUNT_DROP * 3) ->
%%            Pass = false;
%%        true ->
%%            Pass = true
%%        end,

%%        if Pass =/= true ->
%%          DropContent = xml:get_subtag_cdata(Packet, <<"body">>),
%%          ai_api:set_forbidden_user(Uid, ?AI_USER_SEND_MESSAGE_OVERLOAD_DROP, DiffTime, Count, AvarageCount, DropContent),

%%          ?WARNING_MSG("drop Uid:~p, DiffTime:~p, Count:~p, AvarageCount:~p, Pass:~p, DropContent:~p~n",
%%            [Uid, DiffTime, Count, AvarageCount, Pass, DropContent]);
%%        true -> ok
%%        end

%%    end
%%  end,
%%  Pass;

  NeedCheck = is_need_check_msg(From, Packet),
  if NeedCheck =:= false  -> true;
  true ->
    Uid = From#jid.luser,
    case ets:lookup(?AI_USER_SEND_MESSAGE_COUNT_TABLE, Uid) of
      [] ->
        CurrentTime = ai_utils:get_time(),
        ets:insert(?AI_USER_SEND_MESSAGE_COUNT_TABLE, {Uid, 1, CurrentTime});
      [{Uid, Count, Time} | _] ->
        CurrentTime = ai_utils:get_time(),
        DiffTime = CurrentTime - Time + 1,
        AvarageCount = (Count * 60) / DiffTime,

        if DiffTime >= 60, AvarageCount >= 240 ->
          Forbidden = true;
        DiffTime >= 40, AvarageCount >= 360 ->
          Forbidden = true;
        DiffTime >= 20, AvarageCount >= 480 ->
          Forbidden = true;
        DiffTime >= 5, AvarageCount >= 600 ->
          Forbidden = true;
        DiffTime >= 1, AvarageCount >= 7200 ->
          Forbidden = true;
        true ->
          Forbidden = false
        end,

        if DiffTime > 60 ->
          ets:insert(?AI_USER_SEND_MESSAGE_COUNT_TABLE, {Uid, 1, CurrentTime});
        true ->
          ets:insert(?AI_USER_SEND_MESSAGE_COUNT_TABLE, {Uid, Count + 1, Time})
        end,

        if Forbidden =:= true ->
          ForbiddenContent = xml:get_subtag_cdata(Packet, <<"body">>),
          ai_api:set_forbidden_user(Uid, ?AI_USER_SEND_MESSAGE_OVERLOAD_WARN, DiffTime, Count, AvarageCount, ForbiddenContent),
          Pass = false,

          ?WARNING_MSG("drop Uid:~p, DiffTime:~p, Count:~p, AvarageCount:~p, ForbiddenContent:~p~n",
            [Uid, DiffTime, Count, AvarageCount, ForbiddenContent]);
        true -> Pass = true
        end,
        Pass
    end
  end;
  check_send_permit(_From, _Packet) -> true.

kickoff_user(Uid) ->
  JIDS = ejabberd_sm:get_user_jids(Uid),
  %% 删除该用户
  lists:foreach(fun({_LUser, LServer, _LResource}) ->
    %%ejabberd_auth_ai:set_password(Uid, LServer, <<>>)
    ejabberd_auth_ai:remove_user(Uid, LServer)
    end,
  JIDS),
  %% 删除该用户的会话信息
  ?WARNING_MSG("kickoff Uid:~p, close session", [Uid]),
  ejabberd_sm:close_session(Uid).

%% msg is need check: true, need; false, no need
-spec is_need_check_msg(MsgType::integer() | binary(), Rid::binary()) -> boolean().
is_need_check_msg(From, Packet = #xmlel{name = <<"message">>, attrs = Attrs}) ->
  Uid = From#jid.luser,
  Len = string:len(binary_to_list(Uid)),
  Receipt = mod_ai_resend:is_receipts(Packet),
  if Len < 7; Receipt =:= true  -> false;
  true ->
    MsgType = xml:get_attr_s(<<"mtype">>, Attrs),
    case MsgType of
      ?AI_MESSAGE_MTYPE_NIL -> true;
      ?AI_MESSAGE_MTYPE_TEXT -> true;
      ?AI_MESSAGE_MTYPE_AUDIO -> true;
      ?AI_MESSAGE_MTYPE_PICTURE -> true;
      ?AI_MESSAGE_MTYPE_WEB_SHARE -> true;
      ?AI_MESSAGE_MTYPE_EMOTION -> true;
      ?AI_MESSAGE_MTYPE_COMMAND -> true;
      ?AI_MESSAGE_MTYPE_GROUP_CHAT -> true;
      ?AI_MESSAGE_MTYPE_GROUP_SYSTEM_MESSAGE -> true;
      ?AI_MESSAGE_MTYPE_MAGZINE -> true;
      ?AI_MESSAGE_MTYPE_GAME_BUBLE -> true;
      ?AI_MESSAGE_MTYPE_GROUP_GAME_BUBLE -> true;
      ?AI_MESSAGE_MTYPE_GAME_PUSH -> true;
      _ -> false
    end
  end.
