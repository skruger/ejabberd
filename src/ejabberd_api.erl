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
    case catch mjson:decode(Data) of
        {struct, JProps} ->
            case proplists:get_value(<<"domain_config">>, JProps, <<"[].">>) of
                CfgBin when is_binary(CfgBin) ->
                    binary_to_list(CfgBin);
                CfgErr ->
                    ?ERROR_MSG("Error receiving config for ~p in api: ~p~n",[Domain, CfgErr]),
                    "[]."
            end;
        _ ->
            "[]."
    end,
    case ejabberd_hosts:register_host(Domain) of
        {atomic, ok} ->
            ejabberd_config:load_host_config_str(Domain, Conf),
            {200, [], json_out([{"result",<<"ok">>},
                                {"domain", list_to_binary(Domain)},
                                {"registered",ejabberd_hosts:is_valid_host(Domain)},
                                {"running", ejabberd_hosts:is_running_host(Domain)}])};
        {error, Err} ->
            {400, [], json_out([{"result",<<"error">>},
                                {"domain", list_to_binary(Domain)},
                                {"error", iolist_to_binary(io_lib:format("~p",[Err]))}])}
    end;
process_safe(["domain","unregister",Domain], _Request) ->
    case ejabberd_hosts:unregister_host(Domain) of
        {atomic, ok} ->
            {200, [], json_out([{"result",<<"removed">>},
                                {"domain",list_to_binary(Domain)}])};
        {error, Reason} ->
            {400, [], json_out([{"result",<<"error">>},
                                {"domain", list_to_binary(Domain)},
                                {"error", iolist_to_binary(io_lib:format("~p",[Reason]))}])}
    end;
process_safe(["domain","start",Domain], _Req) ->
    case ejabberd_hosts:start_host(Domain) of
        ok ->
            {200, [], json_out([{"result",<<"ok">>},
                                {"domain",list_to_binary(Domain)}])};
        {error, Reason} ->
            {400, [], json_out([{"result",<<"error">>},
                                {"domain",list_to_binary(Domain)},
                                {"error",iolist_to_binary(io_lib:format("~p",[Reason]))}])}
    end;
process_safe(["domain","stop",Domain],_Req) ->
    case ejabberd_hosts:stop_host(Domain) of
        ok ->
            {200, [], json_out([{"result",<<"ok">>},
                                {"domain",list_to_binary(Domain)}])};
        {error, Reason} ->
            {400, [], json_out([{"result",<<"error">>},
                                {"domain",list_to_binary(Domain)},
                                {"error",iolist_to_binary(io_lib:format("~p",[Reason]))}])}
    end;
    
process_safe(["domain","status",Domain], _Req) ->
    {200, [], json_out([{"result",<<"ok">>},
                        {"domain", list_to_binary(Domain)},
                        {"running",ejabberd_hosts:is_running_host(Domain)},
                        {"registered",ejabberd_hosts:is_valid_host(Domain)},
                        {"online_users",ejabberd_sm:get_vh_session_number(Domain)},
                        {"registered_users", ejabberd_auth:get_vh_registered_users_number(Domain)}])};
process_safe(["domain","list"], _Req) ->
    Domains =
    [ {struct,[{"domain", list_to_binary(D)},
               {"running", ejabberd_hosts:is_running_host(D)}]}
            || D <- ejabberd_hosts:get_dynamic_hosts()],
    json_out([{"result",<<"ok">>},
              {"domains", Domains}]);
process_safe(["domain","userlist",Domain], _Req) ->
    case ejabberd_hosts:is_valid_host(Domain) of
        true ->
            Users = [list_to_binary(U) || {U,_S} <- ejabberd_auth:get_vh_registered_users(Domain)],
            {200, [], json_out([{"result",<<"ok">>},
                                {"domain", list_to_binary(Domain)},
                                {"users", Users}])};
        Err ->
            ErrStr = io_lib:format("~p",[Err]),
            {400, [], json_out([{"result",<<"error">>},
                                {"error",iolist_to_binary(ErrStr)}])}
    end;
%process_safe(["domain","userlist",Domain,"register",User], _Req) ->
    

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

