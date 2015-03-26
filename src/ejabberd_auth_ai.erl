
-module(ejabberd_auth_ai).

-author('james.wu@asiainnovations.com').

-behaviour(ejabberd_auth).

%% External exports
-export([start/1, set_password/3, check_password/3,
	 check_password/5, try_register/3,
	 dirty_get_registered_users/0, get_vh_registered_users/1,
	 get_vh_registered_users/2,
	 get_vh_registered_users_number/1,
	 get_vh_registered_users_number/2, get_password/2,
	 get_password_s/2, is_user_exists/2, remove_user/2,
	 remove_user/3, store_type/0,
	 plain_password_required/0]).

-include("ejabberd.hrl").
-include("logger.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(Host) ->
    check_cache_last_options(Host),
    ejabberd_auth_internal:start(Host).

check_cache_last_options(Server) ->
    case get_cache_option(Server) of
      false -> no_cache;
      {true, _CacheTime} ->
	  case get_mod_last_configured(Server) of
	    no_mod_last ->
		?ERROR_MSG("In host ~p extauth is used, extauth_cache "
			   "is enabled but mod_last is not enabled.",
			   [Server]),
		no_cache;
	    _ -> cache
	  end
    end.

plain_password_required() -> true.

store_type() -> external.

check_password(User, Server, Password) ->
  %%check_password_ai(User, Server, Password).
  case get_cache_option(Server) of
    false ->
      ?DEBUG("thrift User:~p, Server:~p, Password:~p~n~n", [User, Server, Password]),
      check_password_ai(User, Server, Password);
    {true, CacheTime} ->
      ?DEBUG("cache User:~p, Server:~p, Password:~p~n~n", [User, Server, Password]),
      check_password_cache(User, Server, Password, CacheTime)
  end.

check_password(User, Server, Password, _Digest,
	       _DigestGen) ->
    check_password(User, Server, Password).

set_password(User, Server, Password) ->
	set_password_internal(User, Server, Password).

try_register(User, Server, Password) ->
    case get_cache_option(Server) of
      false -> try_register_ai(User, Server, Password);
      {true, _CacheTime} ->
	  try_register_external_cache(User, Server, Password)
    end.

dirty_get_registered_users() ->
    ejabberd_auth_internal:dirty_get_registered_users().

get_vh_registered_users(Server) ->
    ejabberd_auth_internal:get_vh_registered_users(Server).

get_vh_registered_users(Server, Data) ->
    ejabberd_auth_internal:get_vh_registered_users(Server,
						   Data).

get_vh_registered_users_number(Server) ->
    ejabberd_auth_internal:get_vh_registered_users_number(Server).

get_vh_registered_users_number(Server, Data) ->
    ejabberd_auth_internal:get_vh_registered_users_number(Server,
							  Data).

%% The password can only be returned if cache is enabled, cached info exists and is fresh enough.
get_password(User, Server) ->
    case get_cache_option(Server) of
      false -> false;
      {true, CacheTime} ->
	  get_password_cache(User, Server, CacheTime)
    end.

get_password_s(User, Server) ->
    case get_password(User, Server) of
      false -> <<"">>;
      Other -> Other
    end.

%% @spec (User, Server) -> true | false | {error, Error}
is_user_exists(_User, _Server) ->
    true.

remove_user(User, Server) ->
	  case get_cache_option(Server) of
	    false -> false;
	    {true, _CacheTime} ->
		ejabberd_auth_internal:remove_user(User, Server)
	  end.

remove_user(User, Server, Password) ->
	  case get_cache_option(Server) of
	    false -> false;
	    {true, _CacheTime} ->
		ejabberd_auth_internal:remove_user(User, Server,
						   Password)
	  end.

%%%
%%% Extauth cache management
%%%

%% @spec (Host::string()) -> false | {true, CacheTime::integer()}
get_cache_option(Host) ->
    case ejabberd_config:get_option(
           {extauth_cache, Host},
           fun(false) -> undefined;
              (I) when is_integer(I), I >= 0 -> I
           end) of
        undefined -> false;
        CacheTime -> {true, CacheTime}
    end.

%% @spec (User, Server, Password) -> true | false
check_password_ai(User, _Server, Password) ->
    case ai_api:check_password(User, Password) of
        true when Password /= <<"">> -> true;
        _ -> false
    end.

%% @spec (User, Server, Password) -> true | false
try_register_ai(_User, _Server, _Password) ->
    false.

check_password_cache(User, Server, Password,
		     CacheTime) ->
    case get_last_access(User, Server) of
      online ->
	  check_password_internal(User, Server, Password);
      never ->
	  check_password_external_cache(User, Server, Password);
      mod_last_required ->
	  ?ERROR_MSG("extauth is used, extauth_cache is enabled "
		     "but mod_last is not enabled in that "
		     "host",
		     []),
	  check_password_external_cache(User, Server, Password);
      TimeStamp ->
	  case is_fresh_enough(TimeStamp, CacheTime) of
	    %% If no need to refresh, check password against Mnesia
	    true ->
		case check_password_internal(User, Server, Password) of
		  %% If password valid in Mnesia, accept it
		  true -> true;
		  %% Else (password nonvalid in Mnesia), check in extauth and cache result
		  false ->
		      check_password_external_cache(User, Server, Password)
		end;
	    %% Else (need to refresh), check in extauth and cache result
	    false ->
		check_password_external_cache(User, Server, Password)
	  end
    end.

get_password_internal(User, Server) ->
    ejabberd_auth_internal:get_password(User, Server).

%% @spec (User, Server, CacheTime) -> false | Password::string()
get_password_cache(User, Server, CacheTime) ->
    case get_last_access(User, Server) of
      online -> get_password_internal(User, Server);
      never -> false;
      mod_last_required ->
	  ?ERROR_MSG("extauth is used, extauth_cache is enabled "
		     "but mod_last is not enabled in that "
		     "host",
		     []),
	  false;
      TimeStamp ->
	  case is_fresh_enough(TimeStamp, CacheTime) of
	    true -> get_password_internal(User, Server);
	    false -> false
	  end
    end.

%% Check the password using extauth; if success then cache it
check_password_external_cache(User, Server, Password) ->
    case check_password_ai(User, Server, Password) of
      true ->
	      set_password_internal(User, Server, Password), true;
      false -> false
    end.

%% Try to register using extauth; if success then cache it
try_register_external_cache(User, Server, Password) ->
    case try_register_ai(User, Server, Password) of
      {atomic, ok} = R ->
	  set_password_internal(User, Server, Password), R;
      _ -> {error, not_allowed}
    end.

%% @spec (User, Server, Password) -> true | false
check_password_internal(User, Server, Password) ->
    ejabberd_auth_internal:check_password(User, Server,
					  Password).

%% @spec (User, Server, Password) -> ok | {error, invalid_jid}
set_password_internal(User, Server, Password) ->
%% @spec (TimeLast, CacheTime) -> true | false
%%       TimeLast = online | never | integer()
%%       CacheTime = integer() | false
    ejabberd_auth_internal:set_password(User, Server,
					Password).

is_fresh_enough(TimeStampLast, CacheTime) ->
    {MegaSecs, Secs, _MicroSecs} = now(),
    Now = MegaSecs * 1000000 + Secs,
    TimeStampLast + CacheTime > Now.

%% @spec (User, Server) -> online | never | mod_last_required | TimeStamp::integer()
%% Code copied from mod_configure.erl
%% Code copied from web/ejabberd_web_admin.erl
%% TODO: Update time format to XEP-0202: Entity Time
get_last_access(User, Server) ->
    case ejabberd_sm:get_user_resources(User, Server) of
      [] ->
	  _US = {User, Server},
	  case get_last_info(User, Server) of
	    mod_last_required -> mod_last_required;
	    not_found -> never;
	    {ok, Timestamp, _Status} -> Timestamp
	  end;
      _ -> online
    end.
%% @spec (User, Server) -> {ok, Timestamp, Status} | not_found | mod_last_required

get_last_info(User, Server) ->
    case get_mod_last_enabled(Server) of
      mod_last -> mod_last:get_last_info(User, Server);
      no_mod_last -> mod_last_required
    end.

%% @spec (Server) -> mod_last | no_mod_last
get_mod_last_enabled(Server) ->
    case gen_mod:is_loaded(Server, mod_last) of
      true -> mod_last;
      false -> no_mod_last
    end.

get_mod_last_configured(Server) ->
    case is_configured(Server, mod_last) of
      true -> mod_last;
      false -> no_mod_last
    end.

is_configured(Host, Module) ->
    gen_mod:is_loaded(Host, Module).
