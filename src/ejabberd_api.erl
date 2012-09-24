-module(ejabberd_api).

-author('skruger@chatmongers.com').

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("web/ejabberd_http.hrl").


-export([process/2]).


%% @spec (LocalPath, Request) -> {HTTPCode::integer(), [Header], Page::string()}
process(Path, #request{auth=UserInfo}=Request) ->
    case check_auth(UserInfo) of
        false ->
            {401, [{"WWW-Authenticate","basic"}],"Authentication required."};
        true ->
            try
                process_safe(Path, Request)
            catch
                _:Err ->
                    ?ERROR_MSG("Uncaught API error!~n~p~n~p~n",
                                [Err, erlang:get_stacktrace()]),
                    {500, [], json_out([{"result", <<"uncaught_error">>},
                                        {"message", <<"500 error">>}])}
            end
    end.

process_safe(["domain","register",Domain], #request{data=Data}=_Request) ->
    Conf =
    case mjson:decode(Data) of
        {struct, JProps} ->
            case proplist:get_value(<<"domain_config">>, JProps, <<"[].">>) of
                CfgBin when is_binary(CfgBin) ->
                    binary_to_list(CfgBin);
                CfgErr ->
                    ?ERROR_MSG("Error receiving config for ~p in api: ~p~n",[Domain, CfgErr]),
                    "[]."
            end;
        _ ->
            "[]."
    end,
    ejabberd_config:load_host_config_str(Domain, Conf),
    case ejabberd_hosts:register_host(Domain) of
        {atomic, ok} ->
            {200, [], "Host registered"};
        {error, Err} ->
            {400, [], io_lib:format("Host not registered: ~p",[ Err])}
    end;
process_safe(Path, Request) ->
    ?ERROR_MSG("Path not found: ~p~n~p~n", [Path, Request]),
    {404, [], "Not Found."}.


check_auth({User0,Pass}) ->
    {User, Server} = auth_parse(User0),
    Jid = jlib:make_jid(User, Server, ""),
    case ejabberd_auth:check_password(User,Server, Pass) of
        true ->
            case acl:match_rule(Server, api_admin, Jid) of
                allow ->
                    true;
                deny ->
                    ?ERROR_MSG("~p access denied for user ~p~n",[api_admin, Jid]),
                    false;
                Other ->
                    ?ERROR_MSG("Unexpected acl:match_rule() result:~n~p~n",[Other]),
                    false
            end;
        false ->
            false
    end;
check_auth(_) ->
    false.
                

auth_parse(User0) ->
    case jlib:string_to_jid(User0) of
        #jid{luser=undefined} ->
            {User0, "localhost"};
        #jid{luser=User,lserver=Server} ->
            {User, Server}
    end.

json_out(StructList) ->
    mjson:encode({struct, StructList}).

