%%%----------------------------------------------------------------------
%%% File    : mod_ai_game.erl
%%% Author  : wugaoping<james.wu@asiainnovations.com>
%%%
%%%----------------------------------------------------------------------

-module(mod_ai_game).

-author("james.wu@asiainnovations.com").

-behaviour(gen_mod).

%% gen_mod callbacks
-export([start/2, stop/1]).

%% iq handlers
-export([process_create_game_iq/3, process_join_game_iq/3,
         process_cancel_game_iq/3, process_game_states_iq/3,
         process_join_feed_game_iq/3, process_cancel_feed_game_iq/3,
         process_feed_game_states_iq/3, process_game_session_iq/3]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").
-include("ai.hrl").

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
				  ?NS_AI_GAME_NEW, ?MODULE, process_create_game_iq,
				  IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
                  ?NS_AI_GAME_JOIN, ?MODULE, process_join_game_iq,
                  IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
                  ?NS_AI_GAME_CANCEL, ?MODULE, process_cancel_game_iq,
                  IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
                  ?NS_AI_GAME_STATES, ?MODULE, process_game_states_iq,
                  IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
                  ?NS_AI_GAME_FEED_JOIN, ?MODULE, process_join_feed_game_iq,
                  IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
                  ?NS_AI_GAME_FEED_CANCEL, ?MODULE, process_cancel_feed_game_iq,
                  IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
                  ?NS_AI_GAME_FEED_STATES, ?MODULE, process_feed_game_states_iq,
                  IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
                  ?NS_AI_GAME_SESSION, ?MODULE, process_game_session_iq,
                  IQDisc).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
				     ?NS_AI_GAME_NEW),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
                     ?NS_AI_GAME_JOIN),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
                     ?NS_AI_GAME_CANCEL),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
                     ?NS_AI_GAME_STATES),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
                     ?NS_AI_GAME_FEED_JOIN),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
                     ?NS_AI_GAME_FEED_CANCEL),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
                     ?NS_AI_GAME_FEED_STATES),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
                     ?NS_AI_GAME_SESSION).

%-------------------------------------------------------------------------

process_create_game_iq(From, _To, #iq{sub_el = SubEl} = IQ) ->
    SessionId = xml:get_subtag_cdata(SubEl, <<"s">>),
    GameType = xml:get_subtag_cdata(SubEl, <<"t">>),
    Uid = xml:get_subtag_cdata(SubEl, <<"u">>),
    Jid = iolist_to_binary(jlib:jid_to_string(From)),
    Rid = xml:get_subtag_cdata(SubEl, <<"r">>),
    Question = xml:get_subtag_cdata(SubEl, <<"q">>),
    State = xml:get_subtag_cdata(SubEl, <<"state">>),
	?DEBUG("SessionId:~p, GameType:~p, Uid:~p, Jid:~p, Rid:~p, Question:~p, State:~p~n~nSubEl:~p~n~nIQ:~p~n~n", [SessionId, GameType, Uid, Jid, Rid, Question, State, SubEl, IQ]),
    CreateGameJson = ai_api:create_game(SessionId, GameType, Uid, Jid, Rid, Question, State),
    case CreateGameJson of
        [] ->
            Reply = IQ#iq{type = error, sub_el = [SubEl, ?ERR_ITEM_NOT_FOUND]};
        _ ->
            case rfc4627:decode(CreateGameJson) of
                {ok, CreateGameObject, _} ->
                    GameSessionId = rfc4627:get_field(CreateGameObject, "sessionId", <<"">>),
		            SubEl0 = [#xmlel{name = <<"avatar">>, attrs = [], children = [{xmlcdata, rfc4627:get_field(CreateGameObject, "avatar", <<"">>)}]},
                              #xmlel{name = <<"nickname">>, attrs = [], children = [{xmlcdata, rfc4627:get_field(CreateGameObject, "nickname", <<"">>)}]},
                              #xmlel{name = <<"s">>, attrs = [], children = [{xmlcdata, GameSessionId}]},
                              #xmlel{name = <<"c">>, attrs = [], children = [{xmlcdata, iolist_to_binary(rfc4627:encode(rfc4627:get_field(CreateGameObject, "content", <<"">>)))}]}],
                    GameState = rfc4627:get_field(CreateGameObject, "gameState", <<"">>),
                    case GameState of
                        ?AI_GAME_STATE_CANCEL ->
                            SubEl1 = [#xmlel{name = <<"cancel">>} | SubEl0];
                        ?AI_GAME_STATE_END ->
                            SubEl1 = [#xmlel{name = <<"end">>} | SubEl0];
                        _ ->
                            SubEl1 = SubEl0
                    end,
                    if SessionId =/= <<"">>, SessionId =:= GameSessionId ->
                        PeerUid = xml:get_subtag_cdata(SubEl, <<"pu">>),
                        case GameState of
                            ?AI_GAME_STATE_CANCEL -> Cancel = 1, End = 0;
                            ?AI_GAME_STATE_END -> Cancel = 0, End = 1;
                            _ -> Cancel = 0, End = 0
                        end,
                        notify_create_state_chage(SessionId, GameType, PeerUid, From, State, Cancel, End);
                    true -> ok
                    end,
                    Reply = IQ#iq{type = result, sub_el = [#xmlel{name = <<"query">>, attrs = [], children = SubEl1}]};
                _ ->
                    Reply = IQ#iq{type = error, sub_el = [SubEl, ?ERR_ITEM_NOT_FOUND]}
            end
    end,
    ?INFO_MSG("Reply:~p~n", [Reply]),
    Reply.

process_join_game_iq(From, _To, #iq{sub_el = SubEl} = IQ) ->
    SessionId = xml:get_subtag_cdata(SubEl, <<"s">>),
    GameType = xml:get_subtag_cdata(SubEl, <<"t">>),
    Uid = xml:get_subtag_cdata(SubEl, <<"u">>),
    Jid = iolist_to_binary(jlib:jid_to_string(From)),
    Rid = xml:get_subtag_cdata(SubEl, <<"r">>),
	?DEBUG("SessionId:~p, GameType:~p, Uid:~p, Jid:~p, Rid:~p~n~nSubEl:~p~n~nIQ:~p~n~n", [SessionId, GameType, Uid, Jid, Rid, SubEl, IQ]),
    JoinGameJson = ai_api:join_game(SessionId, GameType, Uid, Jid, Rid),
    case JoinGameJson of
        [] ->
            Reply = Reply = IQ#iq{type = error, sub_el = [SubEl, ?ERR_ITEM_NOT_FOUND]};
        _ -> 
            case rfc4627:decode(JoinGameJson) of
                {ok, JoinGameObject, _} ->
                    SubEl0 = [#xmlel{name = <<"avatar">>, attrs = [], children = [{xmlcdata, rfc4627:get_field(JoinGameObject, "avatar", <<"">>)}]},
                              #xmlel{name = <<"nickname">>, attrs = [], children = [{xmlcdata, rfc4627:get_field(JoinGameObject, "nickname", <<"">>)}]},
                              #xmlel{name = <<"states">>, attrs = [], children = [{xmlcdata, iolist_to_binary(rfc4627:encode(rfc4627:get_field(JoinGameObject, "states", <<"">>)))}]},
                              #xmlel{name = <<"s">>, attrs = [], children = [{xmlcdata, rfc4627:get_field(JoinGameObject, "sessionId", <<"">>)}]},
                              #xmlel{name = <<"c">>, attrs = [], children = [{xmlcdata, iolist_to_binary(rfc4627:encode(rfc4627:get_field(JoinGameObject, "content", <<"">>)))}]}],
                    Winner = rfc4627:get_field(JoinGameObject, "winner", <<"">>),
                    case Winner of
                        <<"">> ->
                            SubEl1 = SubEl0;
                        _ ->
                            SubEl1 = [#xmlel{name = <<"winner">>, children = [{xmlcdata, <<"1">>}]} | SubEl0] 
                    end,
                    GameState = rfc4627:get_field(JoinGameObject, "gameState", <<"">>),
                    case GameState of
                        ?AI_GAME_STATE_END -> 
							End = true;
                        _ -> 
							End = false
                    end,
                    AllPlayers = rfc4627:get_field(JoinGameObject, "allPlayers", <<"">>),
                    case xml:get_subtag_cdata(SubEl, <<"ftue">>) of
                        <<"">> -> Ftue = false;
                        _ -> Ftue = true
                    end,
                    State = <<"{}">>,
                    notify_state_chage(SessionId, GameType, Uid, From, State, true, End, AllPlayers, Ftue),
                    Reply = IQ#iq{type = result, sub_el = [#xmlel{name = <<"query">>, attrs = [], children = SubEl1}]};
                _ ->
                    Reply = IQ#iq{type = error, sub_el = [SubEl, ?ERR_ITEM_NOT_FOUND]}
            end
    end,
    ?INFO_MSG("Reply:~p~n", [Reply]),
    Reply.

process_cancel_game_iq(From, _To, #iq{sub_el = SubEl} = IQ) ->
    SessionId = xml:get_subtag_cdata(SubEl, <<"s">>),
    GameType = xml:get_subtag_cdata(SubEl, <<"t">>),
    Uid = xml:get_subtag_cdata(SubEl, <<"u">>),
    Jid = iolist_to_binary(jlib:jid_to_string(From)),
    CancelGameJson = ai_api:cancel_game(SessionId, GameType, Uid, Jid),
    case CancelGameJson of
        [] ->
            Reply = IQ#iq{type = error, sub_el = [SubEl, ?ERR_ITEM_NOT_FOUND]};
        _ -> 
            case rfc4627:decode(CancelGameJson) of
                {ok, CancelGameObject, _} ->
                    AllPlayers = rfc4627:get_field(CancelGameObject, "allPlayers", <<"">>),
                    notify_cancel_game(SessionId, GameType, Uid, From, AllPlayers),
                    Reply = IQ#iq{type = result, sub_el = [#xmlel{name = <<"query">>, attrs = [], children = []}]};
            _ ->
                Reply = IQ#iq{type = error, sub_el = [SubEl, ?ERR_ITEM_NOT_FOUND]}
            end
    end,
    ?INFO_MSG("Reply:~p~n", [Reply]),
    Reply.

process_game_states_iq(From, _To, #iq{type = Type, sub_el = SubEl} = IQ) ->
    SessionId = xml:get_subtag_cdata(SubEl, <<"s">>),
    GameType = xml:get_subtag_cdata(SubEl, <<"t">>),
    Uid = xml:get_subtag_cdata(SubEl, <<"u">>),
    Jid = iolist_to_binary(jlib:jid_to_string(From)),
    case Type of
        get ->
            GetGameStateJson = ai_api:get_game_state(SessionId, GameType, Uid),
            case GetGameStateJson of
                [] ->
                    Reply = IQ#iq{type = error, sub_el = [SubEl, ?ERR_ITEM_NOT_FOUND]};
                _ ->
                    case rfc4627:decode(GetGameStateJson) of
                        {ok, GetGameStateObject, _} ->
                            case rfc4627:get_field(GetGameStateObject, "gameState", <<"">>) of
                                ?AI_GAME_STATE_CANCEL ->
                                    SubEl0 = [#xmlel{name = <<"cancel">>, attrs = [], children = []}];
                                ?AI_GAME_STATE_END ->
                                    SubEl0 = [#xmlel{name = <<"end">>, attrs = [], children = []},
                                              #xmlel{name = <<"result">>, children = [{xmlcdata, iolist_to_binary(rfc4627:encode(rfc4627:get_field(GetGameStateObject, "result", <<"">>)))}]}];
                                _ ->
                                    SubEl0 = [#xmlel{name = <<"s">>, attrs = [], children = [{xmlcdata, SessionId}]},
                                              #xmlel{name = <<"states">>, children = [{xmlcdata, iolist_to_binary(rfc4627:encode(rfc4627:get_field(GetGameStateObject, "states", <<"">>)))}]}]
                            end,
                            Reply = IQ#iq{type = result, sub_el = [#xmlel{name = <<"query">>, attrs = [], children = SubEl0}]};
                        _ ->
                            Reply = IQ#iq{type = error, sub_el = [SubEl, ?ERR_ITEM_NOT_FOUND]}
                    end
            end;
        set ->
            State = xml:get_subtag_cdata(SubEl, <<"state">>),
            case xml:get_subtag_cdata(SubEl, <<"end">>) of
                <<"">> -> EndFlag = 0;
                _ -> EndFlag = 1
            end,
            SetGameStateJson = ai_api:update_game_state(SessionId, GameType, Uid, Jid, State, EndFlag),
            case SetGameStateJson of
                [] ->
                    Reply = IQ#iq{type = error, sub_el = [SubEl, ?ERR_ITEM_NOT_FOUND]};
                _ ->
                    case rfc4627:decode(SetGameStateJson) of
                        {ok, SetGameStateObject, _} ->
                            GameState = rfc4627:get_field(SetGameStateObject, "gameState", <<"">>),
                            case GameState of
                                ?AI_GAME_STATE_CANCEL ->
                                    End = false,
                                    SubEl0 = [#xmlel{name = <<"cancel">>, attrs = [], children = []}];
                                ?AI_GAME_STATE_END ->
                                    End = true,
                                    SubEl0 = [#xmlel{name = <<"end">>, attrs = [], children = []}];
                                _ ->
                                    End = false,
                                    SubEl0 = []
                            end,
                            Reply = IQ#iq{type = result, sub_el = [#xmlel{name = <<"query">>, attrs = [], children = SubEl0}]}, 
                            case xml:get_subtag_cdata(SubEl, <<"s">>) of
                                <<"">> -> Ftue = false;
                                _ -> Ftue = true
                            end,
                            AllPlayers = rfc4627:get_field(SetGameStateObject, "allPlayers", <<"">>),
                            notify_state_chage(SessionId, GameType, Uid, From, State, true, End, AllPlayers, Ftue),
                            case GameState of
                                ?AI_GAME_STATE_END ->
                                    notify_game_result(From, SetGameStateObject);
                                _ ->
                                    GameState
                            end;
                        _ ->
                            Reply = IQ#iq{type = error, sub_el = [SubEl, ?ERR_ITEM_NOT_FOUND]}
                    end
            end;
        _ ->
            Reply = IQ#iq{type = error, sub_el = [SubEl, ?ERR_ITEM_NOT_FOUND]}
    end,
    ?INFO_MSG("Reply:~p~n", [Reply]),
    Reply.

process_join_feed_game_iq(From, _To, #iq{sub_el = SubEl} = IQ) ->
    SessionId = xml:get_subtag_cdata(SubEl, <<"s">>),
    GameType = xml:get_subtag_cdata(SubEl, <<"t">>),
    Uid = xml:get_subtag_cdata(SubEl, <<"u">>),
    Puid = xml:get_subtag_cdata(SubEl, <<"pu">>),
    Jid = iolist_to_binary(jlib:jid_to_string(From)),
    Rid = xml:get_subtag_cdata(SubEl, <<"r">>),
    JoinGameJson = ai_api:join_feed_game(SessionId, GameType, Uid, Jid, Rid, Puid),
    case JoinGameJson of
        [] ->
            Reply = jlib:make_error_reply(IQ, ?ERR_ITEM_NOT_FOUND);
        _ -> 
            case rfc4627:decode(JoinGameJson) of
                {ok, JoinGameObject, _} ->
                    GameState = rfc4627:get_field(JoinGameObject, "gameState", <<"">>),
                    case GameState of
                        <<"">> ->
                            Reply = jlib:make_error_reply(IQ, ?ERR_ITEM_NOT_FOUND);
                        ?AI_GAME_STATE_CANCEL ->
                            SubEl0 = [#xmlel{name = <<"cancel">>}],
                            Reply = IQ#iq{type = result, sub_el = [#xmlel{name = <<"query">>, attrs = [], children = SubEl0}]};
                        ?AI_GAME_STATE_END ->
                            SubEl0 = [#xmlel{name = <<"end">>}],
                            Reply = IQ#iq{type = result, sub_el = [#xmlel{name = <<"query">>, attrs = [], children = SubEl0}]}; 
                        _ ->
                            SubEl0 = [#xmlel{name = <<"avatar">>, children = [{xmlcdata, rfc4627:get_field(JoinGameObject, "avatar", <<"">>)}]},
                                      #xmlel{name = <<"nickname">>, children = [{xmlcdata, rfc4627:get_field(JoinGameObject, "nickname", <<"">>)}]},
                                      #xmlel{name = <<"states">>, children = [{xmlcdata, iolist_to_binary(rfc4627:encode(rfc4627:get_field(JoinGameObject, "states", <<"">>)))}]},
                                      #xmlel{name = <<"s">>, children = [{xmlcdata, rfc4627:get_field(JoinGameObject, "sessionId", <<"">>)}]},
                                      #xmlel{name = <<"c">>, children = [{xmlcdata, iolist_to_binary(rfc4627:encode(rfc4627:get_field(JoinGameObject, "content", <<"">>)))}]}],
                            Winner = rfc4627:get_field(JoinGameObject, "winner", <<"">>),
                            case Winner of
                                <<"">> ->
                                    SubEl1 = SubEl0;
                                _ ->
                                    SubEl1 = [#xmlel{name = <<"winner">>, children = [{xmlcdata, Winner}]} | SubEl0]
                            end,
                            Reply = IQ#iq{type = result, sub_el = [#xmlel{name = <<"query">>, attrs = [], children = SubEl1}]}
                    end;
                _ ->
                    Reply = IQ#iq{type = error, sub_el = [SubEl, ?ERR_ITEM_NOT_FOUND]}
            end
    end,
    ?INFO_MSG("Reply:~p~n", [Reply]),
    Reply.

process_cancel_feed_game_iq(From, _To, #iq{sub_el = SubEl} = IQ) ->
    SessionId = xml:get_subtag_cdata(SubEl, <<"s">>),
    GameType = xml:get_subtag_cdata(SubEl, <<"t">>),
    Uid = xml:get_subtag_cdata(SubEl, <<"u">>),
    Puid = xml:get_subtag_cdata(SubEl, <<"pu">>),
    Jid = iolist_to_binary(jlib:jid_to_string(From)),
    CancelGameJson = ai_api:cancel_feed_game(SessionId, GameType, Uid, Jid, Puid),
    case CancelGameJson of
        [] ->
            Reply = IQ#iq{type = error, sub_el = [SubEl, ?ERR_ITEM_NOT_FOUND]};
        _ -> 
            Reply = IQ#iq{type = result, sub_el = [#xmlel{name = <<"query">>, attrs = []}]}
    end,
    ?INFO_MSG("Reply:~p~n", [Reply]),
    Reply.

process_feed_game_states_iq(From, _To, #iq{type = Type, sub_el = SubEl} = IQ) ->
    SessionId = xml:get_subtag_cdata(SubEl, <<"s">>),
    GameType = xml:get_subtag_cdata(SubEl, <<"t">>),
    Uid = xml:get_subtag_cdata(SubEl, <<"u">>),
    Puid = xml:get_subtag_cdata(SubEl, <<"pu">>),
    Jid = iolist_to_binary(jlib:jid_to_string(From)),
    case Type of
        get ->
            GetGameStateJson = ai_api:get_game_state(SessionId, GameType, Uid),
            case GetGameStateJson of
                [] ->
                    Reply = IQ#iq{type = error, sub_el = [SubEl, ?ERR_ITEM_NOT_FOUND]};
                _ ->
                    case rfc4627:decode(GetGameStateJson) of
                        {ok, GetGameStateObject, _} ->
                            case rfc4627:get_field(GetGameStateObject, "gameState", <<"">>) of
                                ?AI_GAME_STATE_CANCEL ->
                                    SubEl0 = [#xmlel{name = <<"cancel">>}],
                                    Reply = IQ#iq{type = result, sub_el = [#xmlel{name = <<"query">>, attrs = [], children = SubEl0}]};
                                ?AI_GAME_STATE_END ->
                                    SubEl0 = [#xmlel{name = <<"s">>, children = [{xmlcdata, SessionId}]},
                                              #xmlel{name = <<"result">>, children = [{xmlcdata, iolist_to_binary(rfc4627:encode(rfc4627:get_field(GetGameStateObject, "result", <<"">>)))}]}
                                             ],
                                    Reply = IQ#iq{type = result, sub_el = [#xmlel{name = <<"query">>, attrs = [], children = SubEl0}]};
                                _ ->
                                    SubEl0 = [#xmlel{name = <<"end">>},
                                              #xmlel{name = <<"states">>, children = [{xmlcdata, iolist_to_binary(rfc4627:encode(rfc4627:get_field(GetGameStateObject, "states", <<"">>)))}]}
                                             ],
                                    Reply = IQ#iq{type = result, sub_el = [#xmlel{name = <<"query">>, attrs = [], children = SubEl0}]}
                            end;
                        _ -> 
            				Reply = IQ#iq{type = error, sub_el = [SubEl, ?ERR_ITEM_NOT_FOUND]}
                   end
            end;
        set ->
            State = xml:get_subtag_cdata(SubEl, <<"state">>),
            case xml:get_subtag_cdata(SubEl, <<"end">>) of
                <<"">> -> 
					EndFlag = 0;
                _ -> 
					EndFlag = 1
            end,
            SetGameStateJson = ai_api:update_feed_game_state(SessionId, GameType, Uid, Jid, State, EndFlag, Puid),
			case SetGameStateJson of
                [] ->
                    Reply = IQ#iq{type = error, sub_el = [SubEl, ?ERR_ITEM_NOT_FOUND]};
                _ ->
                    case rfc4627:decode(SetGameStateJson) of
                        {ok, SetGameStateObject, _} ->
                            GameState = rfc4627:get_field(SetGameStateObject, "gameState", <<"">>),
                            case GameState of
                                ?AI_GAME_STATE_CANCEL ->
                                    SubEl0 = [#xmlel{name = <<"cancel">>}],
                                    Reply = IQ#iq{type = result, sub_el = [#xmlel{name = <<"query">>, attrs = [], children = SubEl0}]}; 
                                ?AI_GAME_STATE_END ->
                                    SubEl0 = [#xmlel{name = <<"end">>}],
                                    Reply = IQ#iq{type = result, sub_el = [#xmlel{name = <<"query">>, attrs = [], children = SubEl0}]}, 
									notify_feed_game_result(From, SetGameStateObject, Puid),
                                    case Fuid = xml:get_subtag_cdata(SubEl, <<"fu">>) of
                                        <<"">> -> ok;
                                        _ when GameType =:= <<"RUSH_ANSWER">> ->
                                            notify_feed_game_ra_result(From, SessionId, GameType, Uid, Fuid)
                                    end;
                                _ ->
									Reply = IQ#iq{type = result, sub_el = [#xmlel{name = <<"query">>, attrs = []}]}
                            end;
                        _ ->
                            Reply = IQ#iq{type = error, sub_el = [SubEl, ?ERR_ITEM_NOT_FOUND]}
                    end
            end;
        _ ->
            Reply = IQ#iq{type = error, sub_el = [SubEl, ?ERR_ITEM_NOT_FOUND]}
    end,
    ?INFO_MSG("Reply:~p~n", [Reply]),
    Reply.

process_game_session_iq(_From, _To, #iq{sub_el = SubEl} = IQ) ->
    Uid = xml:get_subtag_cdata(SubEl, <<"u">>),
    GameSessionJson = ai_api:get_game_session_id(Uid),
    case GameSessionJson of
        [] ->
            Reply = IQ#iq{type = error, sub_el = [SubEl, ?ERR_ITEM_NOT_FOUND]};
        SessionId ->
            SubEl0 = [#xmlel{name = <<"s">>, children = [{xmlcdata, SessionId}]}],
            Reply = IQ#iq{type = result, sub_el = [#xmlel{name = <<"query">>, attrs = [], children = SubEl0}]}
    end,
    ?INFO_MSG("Reply:~p~n", [Reply]),
    Reply.

-spec notify_create_state_chage(string() | binary(), string() | binary(), jid(), 
                         string() | binary(), string() | binary(), boolean(), boolean()) -> any().
notify_create_state_chage(SessionId, GameType, PeerUid, From, State, Cancel, End) ->
    ServerJID = #jid{user = <<"">>, server = From#jid.lserver,
             resource = <<"">>, luser = <<"">>, lserver = From#jid.lserver,
             lresource = <<"">>},
    To = #jid{user = PeerUid, server = From#jid.lserver, resource = From#jid.resource, 
			  luser = PeerUid, lserver = From#jid.lserver, lresource = From#jid.lresource},
    Body0 = [{"s", SessionId}, {"t", GameType}, {"gc", 1}, {"u", From#jid.luser}],
	case rfc4627:decode(State) of
		{ok, StateObject, _} ->
            Body1 = [{"state", StateObject} | Body0];
        _ ->
            Body1 = Body0
	end,
    case Cancel of
        true ->
            Body2 = [{"cancel", 1} | Body1];
        _ ->
            Body2 = Body1
    end,
    case End of
        true ->
            Body3 = [{"end", 1} | Body2];
        _ ->
            Body3 = Body2
    end,
    Body4 = rfc4627:encode({obj, Body3}),
    SubEl = [#xmlel{name = <<"subject">>, attrs = [], children = [{xmlcdata, <<"game_updated">>}]},
              #xmlel{name = <<"body">>, attrs = [], children = [{xmlcdata, iolist_to_binary(Body4)}]}],
	?INFO_MSG("To:~p, SubEl:~p~n", [To, SubEl]),
  mod_ai_msg:route(ServerJID,
                    To,
                    #xmlel{name = <<"message">>,
                           attrs = [{<<"to">>, jlib:jid_to_string(To)},
                                    {<<"from">>, jlib:jid_to_string(ServerJID)},
										{<<"id">>, mod_ai_msg:uuid()},
                    {<<"type">>, <<"chat">>}],
                    children = SubEl},
                    noack),
  ok.

-spec notify_state_chage(string() | binary(), string() | binary(), jid(), 
                         string() | binary(), string() | binary(), boolean(), boolean(),
                         any(), boolean) -> any().
notify_state_chage(SessionId, GameType, ExcludePlayer, JID, State, Join, End, AllPlayers, Ftue) ->
    ServerJID = #jid{user = <<"">>, server = JID#jid.lserver,
             resource = <<"">>, luser = <<"">>, lserver = JID#jid.lserver,
             lresource = <<"">>},
    lists:foreach(fun(Player) -> 
                          case rfc4627:get_field(Player, "uid", <<"">>) of
                              <<"">> -> ok;
                              ExcludePlayer -> ok;
                              _Uid ->
                                  To = rfc4627:get_field(Player, "jid", <<"">>),
								  case catch rfc4627:decode(State) of
								  {ok, StateObject, _} -> 
                                      NewState = StateObject;
								  _ -> 
                                      NewState = State
								  end,
								  Body0 = [{"s", SessionId}, {"t", GameType}, {"u", ExcludePlayer}, {"j", jlib:jid_to_string(JID)}, {"state", NewState}],
                                  case Join of
                                      true -> Body1 = [{"join", 1}] ++ Body0;
                                      _ -> Body1 = Body0
                                  end,
                                  case End of
                                      true -> Body2 = [{"end", 1}] ++ Body1;
                                      _ -> Body2 = Body1
                                  end,
                                  case Ftue of
                                      true -> Body3 = [{"ftue", 1}] ++ Body2;
                                      _ -> Body3 = Body2
                                  end,
                                  Body = rfc4627:encode({obj, Body3}),
                                  SubEl = [#xmlel{name = <<"subject">>, children = [{xmlcdata, <<"game_updated">>}]},
                                          #xmlel{name = <<"body">>, children = [{xmlcdata, iolist_to_binary(Body)}]}],
								                ?INFO_MSG("To:~p, SubEl:~p~n", [To, SubEl]),
                                mod_ai_msg:route(ServerJID,
                                                  jlib:string_to_jid(To),
                                                  #xmlel{name = <<"message">>,
                                                         attrs = [{<<"to">>, To},
                                                                  {<<"from">>, jlib:jid_to_string(ServerJID)},
                                                  {<<"id">>, mod_ai_msg:uuid()},
                                                  {<<"type">>, <<"chat">>}],
                                                  children = SubEl},
                                                  noack)
                          end
                  end,
                  AllPlayers),    
    ok.

-spec notify_cancel_game(string() | binary(), string() | binary(), jid(), string() | binary(), any()) -> any().
notify_cancel_game(SessionId, GameType, ExcludePlayer, JID, AllPlayers) ->
    ServerJID = #jid{user = <<"">>, server = JID#jid.lserver,
             resource = <<"">>, luser = <<"">>, lserver = JID#jid.lserver,
             lresource = <<"">>},
    lists:foreach(fun(Player) ->
                          case rfc4627:get_field(Player, "uid", <<"">>) of
                              <<"">> -> ok;
                              ExcludePlayer -> ok;
                              _Uid ->
                                  To = rfc4627:get_field(Player, "jid", <<"">>),
                                  Body0 = [{"s", SessionId}, {"t", GameType}, {"u", ExcludePlayer}, {"j", jlib:jid_to_string(JID)}],
                                  Body = rfc4627:encode({obj, Body0}),
                                  SubEl = [#xmlel{name = <<"subject">>, children = [{xmlcdata, <<"game_canceled">>}]},
                                          #xmlel{name = <<"body">>, children = [{xmlcdata, iolist_to_binary(Body)}]}],
								  ?INFO_MSG("To:~p, SubEl:~p~n", [To, SubEl]),
                                mod_ai_msg:route(ServerJID,
                                                  jlib:string_to_jid(To),
                                                  #xmlel{name = <<"message">>,
                                                         attrs = [{<<"to">>, To},
                                                                  {<<"from">>, jlib:jid_to_string(ServerJID)},
                                                         {<<"id">>, mod_ai_msg:uuid()},
                                                         {<<"type">>, <<"chat">>}],
                                                         children = SubEl},
                                                  noack)
                          end
                  end,
                  AllPlayers),
    ok.

-spec notify_game_result(jid(), any()) -> any().
notify_game_result(From, GameSessionStateObject) ->
    ServerJID = #jid{user = <<"">>, server = From#jid.lserver,
             resource = <<"">>, luser = <<"">>, lserver = From#jid.lserver,
             lresource = <<"">>},
    ResultObject = rfc4627:get_field(GameSessionStateObject, "result", <<"">>),
    Body0 = [{"resultState", rfc4627:get_field(ResultObject, "resultState", <<"">>)},
             {"question", rfc4627:get_field(ResultObject, "question", <<"">>)},
             {"gameId", rfc4627:get_field(ResultObject, "gameId", <<"">>)},
             {"gameType", rfc4627:get_field(ResultObject, "gameType", <<"">>)}],
    case Hint = rfc4627:get_field(ResultObject, "hint", <<"">>) of
        <<"">> ->
            Body1 = Body0;
        _ ->
            Body1 = [{"hint", Hint} | Body0]
    end,
    case Dimension = rfc4627:get_field(ResultObject, "dimension", <<"">>) of
        <<"">> ->
            Body2 = Body1;
        _ ->
            Body2 = [{"dimension", Dimension} | Body1] 
    end,
    case Label = rfc4627:get_field(ResultObject, "label", <<"">>) of
        <<"">> ->
            Body3 = Body2;
        _ ->
            Body3 = [{"label", Label} | Body2]
    end,
    AllPlayerObject = rfc4627:get_field(GameSessionStateObject, "allPlayers", <<"">>),
    lists:foreach(fun(Player) ->
                          Uid = rfc4627:get_field(Player, "uid", <<"">>),
                          Rid = rfc4627:get_field(Player, "rid", <<"">>),
                          To = From#jid{user = Uid, server = From#jid.lserver, resource = Rid, 
										luser = Uid, lserver = From#jid.lserver, lresource = Rid},
                          case rfc4627:get_field(GameSessionStateObject, "gameLevel" ++ binary_to_list(Uid), <<"">>) of
                              <<"">> ->
                                  Body4 = Body3;
                              GameLevel ->
                                  Body4 = [{"gameLevel", GameLevel} | Body3]
                          end,
                          GameGap = rfc4627:get_field(GameSessionStateObject, "gameGap" ++ binary_to_list(Uid), <<"">>),
                          Body5 = rfc4627:encode({obj, [{"gameGap", GameGap} | Body4]}),
						              GameContent = iolist_to_binary("{\"c\":" ++ Body5 ++ "}"),
                          SubEl = [#xmlel{name = <<"subject">>, children = [{xmlcdata, <<"game_result">>}]},
                                  #xmlel{name = <<"body">>, children = [{xmlcdata, GameContent}]}],
                          MsgId = mod_ai_msg:uuid(),
                          ?INFO_MSG("To:~p, SubEl:~p~n", [To, SubEl]),
                          mod_ai_msg:route(ServerJID,
                                            To,
                                            #xmlel{name = <<"message">>,
                                                   attrs = [{<<"to">>, jlib:jid_to_string(To)},
                                                            {<<"from">>, jlib:jid_to_string(ServerJID)},
                                                            {<<"id">>, MsgId},
                                                            {<<"msgid">>, MsgId},
                                                            {<<"type">>, <<"chat">>},
                                                            {<<"mtype">>, ?AI_MESSAGE_MTYPE_GAME_RESULT}],
                                                   children = SubEl},
                                            noack)
                      end,
                      AllPlayerObject),
    ok.

-spec notify_feed_game_result(jid(), any(), string() | binary()) -> any().
notify_feed_game_result(From, GameSessionStateObject, PkedUid) ->
    ServerJID = #jid{user = <<"">>, server = From#jid.lserver,
             resource = <<"">>, luser = <<"">>, lserver = From#jid.lserver,
             lresource = <<"">>},
    ResultObject = rfc4627:get_field(GameSessionStateObject, "result", <<"">>),
    Body0 = [{"resultState", rfc4627:get_field(ResultObject, "resultState", <<"">>)},
             {"question", rfc4627:get_field(ResultObject, "question", <<"">>)},
             {"gameId", rfc4627:get_field(ResultObject, "gameId", <<"">>)},
             {"gameType", rfc4627:get_field(ResultObject, "gameType", <<"">>)},
			 {"isFeedResult", 1}],
    case Hint = rfc4627:get_field(ResultObject, "hint", <<"">>) of
        <<"">> ->
            Body1 = Body0;
        _ ->
            Body1 = [{"hint", Hint}] ++ Body0
    end,
    AllPlayerObject = rfc4627:get_field(GameSessionStateObject, "allPlayers", []),
    lists:foreach(fun(Player) ->
                          Uid = rfc4627:get_field(Player, "uid", <<"">>),
                          Rid = rfc4627:get_field(Player, "rid", <<"">>),
                          To = From#jid{user = Uid, server = From#jid.lserver, resource = Rid, 
										luser = Uid, lserver = From#jid.lserver, lresource = Rid},
                          case Uid of
                              PkedUid ->
                                  Body2 = [{"pkedUid", PkedUid} | Body1],
                                  Body3 = Body2,
                                  Body4 = [{"pkUid", Uid} | Body3];
                              _ ->
                                  GameLevel = rfc4627:get_field(Player, "gameLevel" ++ binary_to_list(Uid), <<"">>),
                                  case GameLevel of
                                      <<"">> ->
                                          Body2 = Body1;
                                      _ ->
                                          Body2 = [{"gameLevel", GameLevel} | Body1]
                                  end,
                                  Body3 = [{"pkUid", Uid} | Body2],
                                  GameGap = rfc4627:get_field(Player, "gameGap" ++ binary_to_list(Uid), <<"">>),
                                  Body4 = [{"gameGap", GameGap} | Body3]
                          end,
                          Body5 = rfc4627:encode({obj, Body4}),
                          GameContent = iolist_to_binary(Body5),
                          SubEl = [#xmlel{name = <<"subject">>, attrs = [], children = [{xmlcdata, <<"game_result">>}]},
                                  #xmlel{name = <<"body">>, attrs = [], children = [{xmlcdata, GameContent}]}],
                          MsgId = mod_ai_msg:uuid(),
                          ?INFO_MSG("To:~p, SubEl:~p~n", [To, SubEl]),
                          mod_ai_msg:route(ServerJID,
                                            To,
                                            #xmlel{name = <<"message">>,
                                                   attrs = [{<<"to">>, jlib:jid_to_string(To)},
                                                            {<<"from">>, jlib:jid_to_string(ServerJID)},
                                                            {<<"id">>, MsgId},
                                                            {<<"msgid">>, MsgId},
                                                            {<<"type">>, <<"chat">>},
                                                            {<<"mtype">>, ?AI_MESSAGE_MTYPE_GAME_RESULT}],
                                                   children = SubEl},
                                            noack)
                      end,
                      AllPlayerObject),
    ok.

-spec notify_feed_game_ra_result(jid(), string() | binary(), string() | binary(), string() | binary(), string() | binary()) -> any().
notify_feed_game_ra_result(From, SessionId, GameType, Uid, Fuid) ->
    ServerJID = #jid{user = <<"">>, server = From#jid.lserver,
             resource = <<"">>, luser = <<"">>, lserver = From#jid.lserver,
             lresource = <<"">>},
    To = From#jid{user = Fuid, server = From#jid.lserver, luser = Fuid, lserver = From#jid.lserver},
    Body0 = [{"uid", iolist_to_binary(Uid)},
             {"gameId", SessionId},
             {"gameType", GameType},
             {"gamestatus", iolist_to_binary(integer_to_list(100))}],
    Body1 = rfc4627:encode({obj, Body0}),
    SubEl = [#xmlel{name = <<"subject">>, children = [{xmlcdata, <<"game_result">>}]},
            #xmlel{name = <<"body">>, children = [{xmlcdata, iolist_to_binary(Body1)}]}],
	MsgId = mod_ai_msg:uuid(),
	?INFO_MSG("To:~p, SubEl:~p~n", [To, SubEl]),
  mod_ai_msg:route(ServerJID,
                  To,
                  #xmlel{name = <<"message">>,
                         attrs = [{<<"to">>, To},
                                  {<<"from">>, jlib:jid_to_string(ServerJID)},
                                  {<<"type">>, <<"chat">>},
                                  {<<"mtype">>, ?AI_MESSAGE_MTYPE_GAME},
                                  {<<"id">>, MsgId},
                                  {<<"msgid">>, MsgId}],
                         children = SubEl},
                  noack),
    ok.
