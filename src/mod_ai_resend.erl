%%%----------------------------------------------------------------------
%%% File    : mod_ai_resend.erl
%%% Author  : wugaoping<james.wu@asiainnovations.com>
%%%
%%%----------------------------------------------------------------------

-module(mod_ai_resend).

-author('james.wu@asiainnovations.com').

-behaviour(gen_mod).
-behaviour(gen_server).

%% gen_mod callbacks
-export([start/2, stop/1]).

%% process entry
-export([is_receipts/1, process_resend_message/3, process_receipts_message/3, process_asure_Receipts/1]).
-export([start_resend/3]).

%% gen_server callbacks
-record(state, {rsmt}).
-record(resend_message_info, {msgid, pid}).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		terminate/2, code_change/3]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").
-include("ai.hrl").

start(_Host, _Opts) ->
%%  case ets:info(?AI_RESEND_MESSAGE_TABLE) of
%%    undefined ->
%%      ets:new(?AI_RESEND_MESSAGE_TABLE, [named_table, set, public]);
  case catch mnesia:table_info(resend_message_info, attributes) of
    {'EXIT', _Reason} ->
      ?DEBUG("create table resend_message_info _Host:~p, _Opts:~p~n", [_Host, _Opts]),
      mnesia:create_table(resend_message_info,
        [{ram_copies, [node()]},
          {attributes, record_info(fields, resend_message_info)}]),
      %%mnesia:add_table_index(resend_message_info, msgid),
      mnesia:add_table_copy(resend_message_info, node(), ram_copies);
    _ -> ok
  end,
  ok.

stop(_Host) ->
%%  case ets:info(?AI_RESEND_MESSAGE_TABLE) of
%%    undefined -> ok;
%%    _ ->
%%      ets:delete(?AI_RESEND_MESSAGE_TABLE)
%%  end,
  case catch mnesia:table_info(resend_message_info, attributes) of
    {'EXIT', _Reason} ->
      ?DEBUG("no delete table resend_message_info _Host:~p~n", [_Host]),
      ok;
    Status ->
      ?DEBUG("delete table resend_message_info _Host:~p, Status:~p~n", [_Host, Status]),
      mnesia:delete_table(resend_message_info)
  end,
  ok.

%-------------------------------------------------------------------------

process_resend_message(From, To, Packet = #xmlel{name = <<"message">>}) ->
	case is_need_resend(Packet) of
		true ->
			gen_server:cast(?AI_RESEND_MASTER, {message, From, To, Packet});
		false -> ok
	end,
  ok;
process_resend_message(_From, _To, _Packet) ->
  ok.

process_receipts_message(From, To, Packet = #xmlel{name = <<"message">>}) ->
  gen_server:cast(?AI_RESEND_MASTER, {receipts, From, To, Packet});
process_receipts_message(_From, _To, _Packet) ->
  ok.

process_asure_Receipts(Packet = #xmlel{name = <<"message">>, attrs = Attrs, children = Els}) ->
	?DEBUG("Packet:~p~n", [Packet]),
  ReceiptsPacket = case is_need_resend(Packet) of
		true ->
			case is_receipts_request(Packet) of
				true -> 
					Packet;
				false -> 
					ReceiptsEls = [#xmlel{name = ?AI_RECEIPTS_REQUEST, attrs = [{<<"xmlns">>, ?AI_RECEIPTS_NAMESPACE}]} | Els],
					#xmlel{name = <<"message">>, attrs = Attrs, children = ReceiptsEls}
			end;
		false -> 
			Packet
	end,
	?DEBUG("ReceiptsPacket:~p~n", [ReceiptsPacket]),
	ReceiptsPacket.

-spec is_need_resend(Packet::any()) -> any().
is_need_resend(Packet = #xmlel{name = <<"message">>, attrs = Attrs}) ->
  Is_Receipts = is_receipts(Packet),
  TypeCheck = if Is_Receipts =:= false ->
    Type = xml:get_attr_s(<<"type">>, Attrs),
    case Type of
			<<>> -> true;
			?AI_MESSAGE_CHAT_TYPE_SINGLE -> true;
			?AI_MESSAGE_CHAT_TYPE_GROUP -> true;
			_ -> false
		end;
	true -> false
	end,
  Ret = if TypeCheck =:= true ->
		MType = xml:get_attr_s(<<"mtype">>, Attrs),
		case MType of
	        ?AI_MESSAGE_MTYPE_ACK -> false;
	        ?AI_MESSAGE_MTYPE_POINT -> false;
	        ?AI_MESSAGE_MTYPE_NEW_CONTACT -> false;
	        ?AI_MESSAGE_MTYPE_NEW_VISITOR -> false;
	        ?AI_MESSAGE_MTYPE_NEW_FEED -> false;
	        _ -> true
	    end;
	true -> false
	end,
	Ret.

-spec is_receipts(Packet::any()) -> any().
is_receipts(_Packet = #xmlel{name = <<"message">>, attrs = _Attrs, children = Els}) ->
	SubEl = [Name || #xmlel{name=Name} <- Els, Name =:= ?AI_RECEIPTS_RECEIVED],
  IsReceipts = case SubEl of
		[] -> false;
		_ -> true
	end,
	?DEBUG("params:~p, IsReceipts:~p, packet:~p~n~n", [?AI_RECEIPTS_RECEIVED, IsReceipts, _Packet]),
	IsReceipts.

-spec is_receipts_request(Packet::any()) -> any().
is_receipts_request(_Packet = #xmlel{name = <<"message">>, attrs = _Attrs, children = Els}) ->
	SubEl = [Name || #xmlel{name=Name} <- Els, Name =:= ?AI_RECEIPTS_REQUEST],
  IsReceiptsRequest = case SubEl of
		[] -> false;
		_ -> true
	end,
	?DEBUG("params:~p, IsReceipts:~p, packet:~p~n~n", [?AI_RECEIPTS_REQUEST, IsReceiptsRequest, _Packet]),
	IsReceiptsRequest.

%-------------------------------------------------------------------------
%% 实现通用重发消息管理服务器
start_link() ->
  ?INFO_MSG("Stack:~p~n", [erlang:get_stacktrace()]),
	gen_server:start_link({local, ?AI_RESEND_MASTER}, ?MODULE, [?AI_RESEND_MESSAGE_TABLE], []).

init(State) ->
  process_flag(trap_exit, true),
	{ok, #state{rsmt = State}}.

handle_call(_Msg, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

handle_cast({message, From, To, Packet = #xmlel{name = <<"message">>, attrs = Attrs}}, State) ->
	MsgId = xml:get_attr_s(<<"id">>, Attrs),
	if MsgId =/= <<>> ->
    case get_resend_pid(MsgId) of
			undefined ->
				Pid = spawn(?MODULE, start_resend, [From, To, Packet]),
        set_resend_info(MsgId, Pid);
			_ ->
				ok
		end;
	true -> ok
	end,
	{noreply, State};
handle_cast({receipts, _From, _To, _Packet = #xmlel{name = <<"message">>, children = Els}}, State) ->
	ReceiptsEls = [SublEl || SublEl = #xmlel{name=Name} <- Els, Name =:= ?AI_RECEIPTS_RECEIVED],
  [#xmlel{attrs = ReceiptsAttrs} | _] = ReceiptsEls,
	MsgId = xml:get_attr_s(<<"id">>, ReceiptsAttrs),
	?DEBUG("receipts from:~p, to:~p, packet:~p~n~n", [_From, _To, _Packet]),
	if MsgId =/= <<>> ->
		case get_resend_pid(MsgId) of
      undefined ->
				?DEBUG("MsgId:~p is not exist~n~n", [MsgId]),
				ok;
      Pid ->
				?DEBUG("MsgId:~p, Pid:~p~n~n", [MsgId, Pid]),
				Pid ! {ok, receipts},
        delete_resend_info(MsgId)
		end;
	true -> ok
	end,
	{noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Reason, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%-------------------------------------------------------------------------
%% 处理消息的重发
-spec start_resend(From::jid(), To::jid(), Packet::any()) -> any().
start_resend(From, To, Packet) ->
    start_resend(From, To, Packet, ?AI_RESEND_MAX_NUM).

-spec start_resend(From::jid(), To::jid(), Packet::any(), Count::integer()) -> any().
start_resend(From, To, Packet = #xmlel{name = <<"message">>, attrs = Attrs, children = _SubEl}, Count) ->
	receive
		{ok, receipts} -> ok
	after ?AI_RESEND_WAIT_TIMEOUT ->
		if Count > 0 ->
      %% 直接发送，避免重复存储消息
      %%SubEl1 = #xmlel{name = <<"count">>, attrs = [], children = [{xmlcdata, integer_to_binary(Count)}]},
      %%NewPacket = Packet#xmlel{name = <<"message">>, attrs = Attrs, children=[SubEl1 | SubEl]},
      %%ejabberd_router:route(From, To, NewPacket),
      ejabberd_router:route(From, To, Packet),

			%%ejabberd_router:route(From, To, Packet),
      ?DEBUG("from:~p, to:~p, Count:~p, NewPacket:~p~n~n", [From, To, Count]),
			start_resend(From, To, Packet, (Count - 1));
		true -> 
			MsgId = xml:get_attr_s(<<"id">>, Attrs),
      delete_resend_info(MsgId),
      ?DEBUG("from:~p, to:~p, packet:~p, Count:~p~n~n", [From, To, Packet, Count]),
			mod_ai_msg:store_offline_msg(From, To, Packet)
		end
	end,
    ok.

%-------------------------------------------------------------------------
%% 操作重发记录表

%% 根据消息ID获取重发进程标识
-spec get_resend_pid(MsgId::string() | binary()) -> pid() | undefined.
get_resend_pid(MsgId) ->
%%  case ets:lookup(?AI_RESEND_MESSAGE_TABLE, MsgId) of
%%    [{MsgId, Pid} | _] ->
%%      ?DEBUG("find MsgId:~p, Pid:~p~n", [MsgId, Pid]),
%%      Pid;
  case catch mnesia:dirty_read(resend_message_info, MsgId) of
    [#resend_message_info{msgid=MsgId, pid=Pid} | _] ->
      ?DEBUG("from mnesia resend_message_info find MsgId:~p, Pid:~p~n", [MsgId, Pid]),
      Pid;
  _ ->
      undefined
  end.

%% 保存消息ID和重发进程标识的对应关系记录
-spec set_resend_info(MsgId::string() | binary(), Pid::pid()) -> any().
set_resend_info(MsgId, Pid) ->
  ?DEBUG("set MsgId:~p, Pid:~p~n", [MsgId, Pid]),
%%  ets:insert(?AI_RESEND_MESSAGE_TABLE, {MsgId, Pid}).
  F = fun () -> mnesia:write(#resend_message_info{msgid = MsgId, pid = Pid}) end,
  mnesia:sync_dirty(F).

%% 删除消息ID和重发进程标识的对应关系记录
-spec delete_resend_info(MsgId::string() | binary()) -> any().
delete_resend_info(MsgId) ->
%%  case ets:lookup(?AI_RESEND_MESSAGE_TABLE, MsgId) of
%%    [{MsgId, Pid} | _] ->
%%      ?DEBUG("delete MsgId:~p, Pid:~p~n", [MsgId, Pid]),
%%      ets:delete(?AI_RESEND_MESSAGE_TABLE, MsgId);
%%    _ -> undefined
%%    end.
  F = fun () -> mnesia:delete({resend_message_info, MsgId}) end,
  mnesia:sync_dirty(F).
