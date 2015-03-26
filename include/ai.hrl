%%%----------------------------------------------------------------------
%%% File    : ai.hrl
%%% Author  : wugaoping <james.wu@asiainnovations.com>
%%%
%%%----------------------------------------------------------------------

-define(NS_AI_GAME_NEW, <<"ai:game:new">>).
-define(NS_AI_GAME_JOIN, <<"ai:game:join">>).
-define(NS_AI_GAME_CANCEL, <<"ai:game:cancel">>).
-define(NS_AI_GAME_STATES, <<"ai:game:states">>).
-define(NS_AI_GAME_FEED_JOIN, <<"ai:game:feed:join">>).
-define(NS_AI_GAME_FEED_CANCEL, <<"ai:game:feed:cancel">>).
-define(NS_AI_GAME_FEED_STATES, <<"ai:game:feed:states">>).
-define(NS_AI_GAME_SESSION, <<"ai:game:session">>).
-define(NS_AI_GAME_SHAKING, <<"ai:game:shaking">>).
-define(NS_AI_GAME_SHAKING_CANCEL, <<"ai:game:shakingCancel">>).
-define(NS_AI_GROUP_CHAT_NEW, <<"ai:groupChat:new">>).
-define(NS_AI_GROUP_CHAT_UPDATE_GROUP_NAME, <<"ai:groupChat:updateGpName">>).
-define(NS_AI_GROUP_CHAT_ADD_USERS, <<"ai:groupChat:addUsers">>).
-define(NS_AI_GROUP_CHAT_DEL_USERS, <<"ai:groupChat:delUser">>).
-define(NS_AI_GROUP_CHAT_USERS, <<"ai:groupChat:users">>).
-define(NS_AI_GROUP_CHAT_GROUPS_BY_USER, <<"ai:groupChat:groupsByUser">>).
-define(NS_AI_BADGE_STATES, <<"ai:badge:states">>).
-define(NS_AI_MESSAGE_ACK, <<"ai:message:ack">>).

-define(AI_MESSAGE_CHAT_TYPE_SINGLE, <<"chat">>).
-define(AI_MESSAGE_CHAT_TYPE_GROUP, <<"groupchat">>).
-define(AI_MESSAGE_CHAT_TYPE_ERROR, <<"error">>).
-define(AI_MESSAGE_CHAT_TYPE_CANCEL, <<"cancel">>).

-define(AI_MESSAGE_MTYPE_NIL, <<"">>).
-define(AI_MESSAGE_MTYPE_ACK, <<"ack">>).
-define(AI_MESSAGE_MTYPE_TEXT, <<"1">>).
-define(AI_MESSAGE_MTYPE_AUDIO, <<"2">>).
-define(AI_MESSAGE_MTYPE_PICTURE, <<"3">>).
-define(AI_MESSAGE_MTYPE_GAME, <<"4">>).
-define(AI_MESSAGE_MTYPE_POINT, <<"6">>).
-define(AI_MESSAGE_MTYPE_WEB_SHARE, <<"7">>).
-define(AI_MESSAGE_MTYPE_EMOTION, <<"8">>).
-define(AI_MESSAGE_MTYPE_COMMAND, <<"9">>).
-define(AI_MESSAGE_MTYPE_GROUP_CHAT, <<"19">>).
-define(AI_MESSAGE_MTYPE_GROUP_SYSTEM_MESSAGE, <<"23">>).
-define(AI_MESSAGE_MTYPE_CHAT_BACKGROUND, <<"50">>).
-define(AI_MESSAGE_MTYPE_GAME_RESULT, <<"100">>).
-define(AI_MESSAGE_MTYPE_MAGZINE, <<"200">>).
-define(AI_MESSAGE_MTYPE_GAME_BUBLE, <<"300">>).
-define(AI_MESSAGE_MTYPE_GROUP_GAME_BUBLE, <<"301">>).
-define(AI_MESSAGE_MTYPE_GAME_PUSH, <<"400">>).
-define(AI_MESSAGE_MTYPE_GROUP_BROADCAST, <<"401">>).
-define(AI_MESSAGE_MTYPE_NEW_CONTACT, <<"401">>).
-define(AI_MESSAGE_MTYPE_NEW_VISITOR, <<"402">>).
-define(AI_MESSAGE_MTYPE_NEW_FEED, <<"403">>).
-define(AI_MESSAGE_MTYPE_SUGGEST_FRIEND, <<"405">>).

-define(AI_GAME_STATE_CREATE, 0).
-define(AI_GAME_STATE_CANCEL, 1).
-define(AI_GAME_STATE_JOIN, 2).
-define(AI_GAME_STATE_CONFIRM_QUESTION, 3).
-define(AI_GAME_STATE_CONFIRM_ANSWER, 4).
-define(AI_GAME_STATE_WIN, 5).
-define(AI_GAME_STATE_LOSS, 6).
-define(AI_GAME_STATE_END, 99).

-define(AI_CODE_SUCCESS, <<"N000000">>).
-define(AI_CODE_NODATA, <<"N000001">>).
-define(AI_CODE_FAIL, <<"N000999">>).

-define(AI_BADGE_TYPE_FEED, <<"1">>).
-define(AI_BADGE_TYPE_CONTACT, <<"2">>).
-define(AI_BADGE_TYPE_VISITOR, <<"3">>).
-define(AI_BADGE_TYPE_OFFLINE_MSG, <<"4">>).

-define(AI_ROBOT_LIST, ",529543,521418,526635,522663,525932,520921,528938,525127,523817,524141,527138,526190,522696,524385,522856,523979,524874,521638,526168,527307,528286,527707,523653,520909,526784,523114,525001,522812,526973,526028,522518,522064,529199,525796,523486,528412,529543,527732,525876,523294,106210,105360,105347,105348,105358,105351,105352,528626,").

-define(AI_RESEND_MASTER, 'ai_resend_master').
-define(AI_RESEND_MESSAGE_TABLE, 'ai_resend_mt').
-define(AI_RESEND_MAX_NUM, 2).
-define(AI_RESEND_WAIT_TIMEOUT, 3000).

-define(AI_RECEIPTS_REQUEST, <<"request">>).
-define(AI_RECEIPTS_RECEIVED, <<"received">>).
-define(AI_RECEIPTS_NAMESPACE, <<"urn:xmpp:receipts">>).

-define(AI_USER_SEND_MESSAGE_COUNT_TABLE, 'ai_user_send_message_count_table').
-define(AI_USER_SEND_MESSAGE_COUNT_DROP, 120).
-define(AI_USER_SEND_MESSAGE_COUNT_FORBID, 240).

-define(AI_USER_SEND_MESSAGE_FORBID_USER, 2).
-define(AI_USER_SEND_MESSAGE_OVERLOAD_WARN, 4).
-define(AI_USER_SEND_MESSAGE_OVERLOAD_DROP, 5).

-define(AI_PROXY_PORT, 7222).

-define(AI_LOCAL_HOST, <<"chatcn.stage.pengpeng.la">>).
-define(AI_LOCAL_RESOURCE, <<"PPIOS">>).

