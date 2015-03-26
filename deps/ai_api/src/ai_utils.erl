%% @author james
%% @doc @todo Add description to ai_api_service.

-module(ai_utils).

-author('james.wu@asiainnovations.com').


%% API
-compile(export_all).


%% ===================================================================
%% API functions
%% ===================================================================

get_time() ->
    {MegaSec, Sec, _MilliSec} = os:timestamp(),
    MegaSec * 1000000 + Sec.


