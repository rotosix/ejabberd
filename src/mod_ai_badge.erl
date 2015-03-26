%%%----------------------------------------------------------------------
%%% File    : mod_ai_badge.erl
%%% Author  : wugaoping<james.wu@asiainnovations.com>
%%%
%%%----------------------------------------------------------------------

-module(mod_ai_badge).

-author('james.wu@asiainnovations.com').

-behaviour(gen_mod).

%% gen_mod callbacks
-export([start/2, stop/1]).

%% iq handlers
-export([process_badge_states_iq/3]).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").
-include("ai.hrl").

start(Host, Opts) ->
    IQDisc = gen_mod:get_opt(iqdisc, Opts, fun gen_iq_handler:check_type/1,
                             one_queue),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host,
                  ?NS_AI_BADGE_STATES, ?MODULE, process_badge_states_iq,
                  IQDisc).

stop(Host) ->
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host,
                     ?NS_AI_BADGE_STATES).

%-------------------------------------------------------------------------

process_badge_states_iq(From, _To, #iq{sub_el = SubEl} = IQ) ->
    Uid = From#jid.luser,
    BadgeType = xml:get_subtag_cdata(SubEl, <<"t">>),
    Badge = binary_to_integer(xml:get_subtag_cdata(SubEl, <<"b">>)),
    case BadgeType of
        ?AI_BADGE_TYPE_FEED ->
            StartTime = Badge,
            Result = ai_api:get_feed_badge(Uid, StartTime);
        ?AI_BADGE_TYPE_CONTACT ->
            StartTime = Badge,
            Result = ai_api:get_contact_badge(Uid, StartTime);
        ?AI_BADGE_TYPE_VISITOR ->
            StartTime = Badge,
            Result = ai_api:get_visitor_badge(Uid, StartTime);
        ?AI_BADGE_TYPE_OFFLINE_MSG ->
            Num = Badge,
            ai_api:set_user_offline_msg_num(Uid, Num),
            Result = <<"">>;
        _ ->
            Result = []
    end,
    case Result of
        [] ->
            Reply = IQ#iq{type = error, sub_el = [SubEl, ?ERR_ITEM_NOT_FOUND]};
        _ ->
            SubEl0 = [#xmlel{name = <<"c">>, children = [{xmlcdata, ?AI_CODE_SUCCESS}]},
                      #xmlel{name = <<"r">>, children = [{xmlcdata, Result}]}],
            Reply = IQ#iq{type = result, sub_el = [#xmlel{name = <<"query">>, attrs = [], children = SubEl0}]}
    end,
	?INFO_MSG("Reply:~p~n", [Reply]),
    Reply.
