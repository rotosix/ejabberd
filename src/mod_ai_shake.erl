%%%----------------------------------------------------------------------
%%% File    : mod_ai_shake.erl
%%% Author  : wugaoping<james.wu@asiainnovations.com>
%%%
%%%----------------------------------------------------------------------

-module(mod_ai_shake).

-author("james.wu@asiainnovations.com").

-behaviour(gen_mod).

%% gen_mod callbacks
-export([start/2, stop/1]).

%% iq handlers
-export([process_game_shaking_iq/3, process_cancel_game_shaking_iq/3]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").
-include("ai.hrl").

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
                  ?NS_AI_GAME_SHAKING, ?MODULE, process_game_shaking_iq,
                  IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
                  ?NS_AI_GAME_SHAKING_CANCEL, ?MODULE, process_cancel_game_shaking_iq,
                  IQDisc).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
                     ?NS_AI_GAME_SHAKING),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
                     ?NS_AI_GAME_SHAKING_CANCEL).

%-------------------------------------------------------------------------

process_game_shaking_iq(From, _To, #iq{sub_el = SubEl} = IQ) ->
    OperatorUid = From#jid.luser,
    Lon = binary_to_float(xml:get_subtag_cdata(SubEl, <<"lon">>)),
    Lat = binary_to_float(xml:get_subtag_cdata(SubEl, <<"lat">>)),
    GameType = xml:get_subtag_cdata(SubEl, <<"gtype">>),
    QueryTimes = binary_to_integer(xml:get_subtag_cdata(SubEl, <<"gtimes">>)),
    ?INFO_MSG("From:~p, GameType:~p, QueryTimes:~p~n", [From, GameType, QueryTimes]),
    case ai_api:get_shaking_user(OperatorUid, Lon, Lat, GameType, QueryTimes) of
        [] ->
            Reply = IQ#iq{type = error, sub_el = [SubEl, ?ERR_ITEM_NOT_FOUND]};
        ShakingInfo ->
            case rfc4627:decode(ShakingInfo) of
                {ok, ShakingInfoObject, _} ->
                    SubEl0 = [#xmlel{name = <<"c">>, children = [{xmlcdata, ?AI_CODE_SUCCESS}]},
                              #xmlel{name = <<"r">>, children = [{xmlcdata, ShakingInfo}]}],
                    RUid = rfc4627:get_field(ShakingInfoObject, "ruid", <<"">>),
                    if RUid =/= <<"">> ->
                        notify_peer_shaking_ok(From, RUid, ShakingInfo);
					true ->
						ok
                    end,
                    Reply = IQ#iq{type = result, sub_el = [#xmlel{name = <<"query">>, attrs = [], children = SubEl0}]};
                _ ->
                    Reply = IQ#iq{type = error, sub_el = [SubEl, ?ERR_ITEM_NOT_FOUND]}
            end
    end,
    ?INFO_MSG("From:~p, GameType:~p, QueryTimes:~p, Reply:~p~n", [From, GameType, QueryTimes, Reply]),
    Reply.

process_cancel_game_shaking_iq(From, _To, #iq{sub_el = SubEl} = IQ) ->
    Uid = From#jid.luser,
    Lon = binary_to_float(xml:get_subtag_cdata(SubEl, <<"lon">>)),
    Lat = binary_to_float(xml:get_subtag_cdata(SubEl, <<"lat">>)),
    GameType = xml:get_subtag_cdata(SubEl, <<"gtype">>),
    ?INFO_MSG("From:~p, GameType:~p~n", [From, GameType]),
    case ai_api:cancel_shaking(Uid, Lon, Lat, GameType) of
        true ->
            SubEl0 = [#xmlel{name = <<"c">>, children = [{xmlcdata, ?AI_CODE_SUCCESS}]},
                      #xmlel{name = <<"r">>, children = [{xmlcdata, <<"ok">>}]}],
            Reply = IQ#iq{type = result, sub_el = [#xmlel{name = <<"query">>, attrs = [], children = SubEl0}]};
        _ ->
            Reply = IQ#iq{type = error, sub_el = [SubEl, ?ERR_ITEM_NOT_FOUND]}
    end,
    ?INFO_MSG("From:~p, GameType:~p, Reply:~p~n", [From, GameType, Reply]),
    Reply.

-spec notify_peer_shaking_ok(From::jid(), Uid::string() | binary(), ShakingInfo::string() | binary()) -> any().
notify_peer_shaking_ok(From, Uid, ShakingInfo) ->
    ServerJID = #jid{server = From#jid.lserver, lserver = From#jid.lserver},
    To = #jid{user = Uid, server = From#jid.lserver, resource = From#jid.resource, 
			  luser = Uid, lserver = From#jid.lserver, lresource = From#jid.lresource},
    SubEl = [#xmlel{name = <<"subject">>, children = [{xmlcdata, <<"shaking">>}]},
             #xmlel{name = <<"body">>, children = [{xmlcdata, ShakingInfo}]}],
	  ?INFO_MSG("From:~p, Uid:~p, ShakingInfo:~p~n", [From, Uid, ShakingInfo]),
    mod_ai_msg:route(ServerJID,
                      To,
                      #xmlel{name = <<"message">>,
                             attrs = [{<<"to">>, jlib:jid_to_string(To)},
                                      {<<"from">>, jlib:jid_to_string(ServerJID)},
										  {<<"id">>, mod_ai_msg:uuid()}],
                                 children = SubEl},
                      noack),
    ok.

