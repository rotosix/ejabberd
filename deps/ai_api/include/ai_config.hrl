-ifndef(_ai_config_included).
-define(_ai_config_included, yeah).

-define(SERVICE_SERVER_HOST, "service.stage.pengpeng.la").
-define(SERVICE_SERVER_PORT, 9090).
-define(SERVICE_SERVER_SERVICE, 'thriftService_thrift').

-define(MONGO_SERVER_HOST, "mongo.stage.pengpeng.la").
-define(MONGO_SERVER_PORT, 8090).
-define(MONGO_SERVER_SERVICE, 'thriftService_thrift').

-define(APNS_SERVER_HOST, "apns.stage.pengpeng.la").
-define(APNS_SERVER_PORT, 8090).
-define(APNS_SERVER_SERVICE, 'thriftService_thrift').

-define(SENSITIVE_WORDS_SERVER_HOST, "sensitive-words.stage.pengpeng.la").
-define(SENSITIVE_WORDS_SERVER_PORT, 9090).
-define(SENSITIVE_WORDS_SERVER_SERVICE, 'thriftService_thrift').

-define(ROUTER_SERVER_HOST, "r.stage.pengpeng.la").
-define(ROUTER_SERVER_PORT, 9090).
-define(ROUTER_SERVER_SERVICE, 'thriftService_thrift').

-define(ROBOT_SERVER_HOST, "robot.stage.pengpeng.la").
-define(ROBOT_SERVER_PORT, 9090).
-define(ROBOT_SERVER_SERVICE, 'thriftService_thrift').

-endif.
