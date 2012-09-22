-module(ejabberd_hosts).

-behaviour(gen_server).

-include("ejabberd.hrl").

-export([start_link/0]).

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([get_host_list/0,stop_all_hosts/0,start_host/1,stop_host/1]).
-export([get_running_hosts/0]).
-export([is_valid_host/1,is_stopped_host/1,is_running_host/1]).
-export([get_dynamic_hosts/0,register_host/1,unregister_host/1]).

-record(hosts_state,{active_hosts}).

start_link() ->
    gen_server:start_link({local, ?MODULE},?MODULE,[],[]).

get_host_list() ->
    StaticHosts = ejabberd_config:get_global_option(hosts),
    DynamicHosts = get_dynamic_hosts(),
    StaticHosts ++ DynamicHosts.

get_running_hosts() ->
    {Replies, _Badnodes} = gen_server:multi_call([node()|nodes()], ?MODULE, get_running_hosts, 1000),
    {_Nodes, HostLists} = lists:unzip(Replies),
    lists:usort(lists:append(HostLists)).

get_dynamic_hosts() ->
    case ejabberd_config:get_global_option(dynamic_hosts) of
        DHosts when is_list(DHosts) -> DHosts;
        _ -> []
    end.

register_host(Host) ->
    HostList = get_dynamic_hosts(),
    case lists:member(Host, HostList) of
        true ->
            {error, host_exists};
        false ->
            ejabberd_config:add_global_option(dynamic_hosts, [Host|HostList])
    end.

unregister_host(Host) ->
    HostList = get_dynamic_hosts(),
    case {lists:member(Host, HostList), is_running_host(Host)} of
        {true, true} ->
            {error, host_running};
        {true, _} ->
            NewHostList = HostList--[Host],
            ejabberd_config:add_global_option(dynamic_hosts, NewHostList);
        {false, _} ->
            {error, host_not_registered}
    end.

start_host(Host) ->
    gen_server:call(?MODULE, {start_host, Host}).

stop_host(Host) ->
    gen_server:call(?MODULE, {stop_host, Host}).

stop_all_hosts() ->
    lists:map(fun stop_host/1, ?MYHOSTS).

init(_) ->
    ?INFO_MSG("Starting ~p~n~p~n",[?MODULE, application:which_applications()]),
    {ok, #hosts_state{active_hosts=[]}}.

handle_call({start_host, Host}, ReplyTo, State) ->
    gen_server:cast(self(), {start_host, Host, ReplyTo}),
    {noreply, State};
handle_call({stop_host, Host}, ReplyTo, State) ->
    gen_server:cast(self(), {stop_host, Host, ReplyTo}),
    {noreply, State};
handle_call(get_running_hosts, _From, State) ->
    {reply, get_local_running_hosts(), State};
handle_call(_Msg, _From, State) ->
    {reply, {error, invalid}, State}.

handle_cast({start_host, Host, ReplyTo}, State) ->
    ?ERROR_MSG("~p~n", [ {start_host, Host, ReplyTo} ]),
    Reply =
    case is_stopped_host(Host) of
        undefined ->
            {error, nohost};
        true ->
            HostSupName = ejabberd_host_sup:get_name(Host),
            HostSup = {HostSupName, {ejabberd_host_sup, start_link, [Host]},
                       permanent, 10000, supervisor, [ejabberd_host_sup]},
            SupStart = supervisor:start_child(ejabberd_sup, HostSup),
            ejabberd_rdbms:start_host(Host),
            Auth = ejabberd_auth:start(Host),
            start_modules(Host),
            register_routes(Host),
            ejabberd_sm:start_host(Host),
            set_host_active(Host),
            ?INFO_MSG("Supervisor result: ~n~p~nAuth results: ~n~p~n",[SupStart,Auth]),
            ok;
        false ->
            {error, already_running}
    end,
    gen_server:cast(self(),{reply, Reply, ReplyTo}),
    {noreply, State};
handle_cast({stop_host, Host, ReplyTo}, State) ->
    ?ERROR_MSG("~p~n", [ {stop_host, Host, ReplyTo} ]),
    Reply =
    case is_running_host(Host) of
        undefined ->
            {error, nohost};
        true ->
            set_host_inactive(Host),
            ejabberd_sm:stop_host(Host),
            stop_modules(Host),
            unregister_routes(Host),
            Auth = ejabberd_auth:stop(Host),
            ejabberd_rdbms:stop_host(Host),
            HostSupName = ejabberd_host_sup:get_name(Host),
            catch supervisor:terminate_child(ejabberd_sup, HostSupName),
            catch supervisor:delete_child(ejabberd_sup, HostSupName),
            ?INFO_MSG("Auth results: ~n~p~n",[Auth]),
            ok;
        false ->
            {error, not_running}
    end,
    gen_server:cast(self(),{reply, Reply, ReplyTo}),
    {noreply, State};
handle_cast({reply, _Value, false}, State) ->
    {noreply, State};
handle_cast({reply, Value, ReplyTo}, State) ->
    gen_server:reply(ReplyTo, Value),
    {noreply, State};
handle_cast(Msg, State) ->
    ?ERROR_MSG("Invalid message to ~p~n~p~n",[?MODULE, Msg]),
    {noreply, State}.

handle_info(finish_init, State) ->
    case is_app_running() of
        true ->
            [ gen_server:cast(self(), {start_host, H, false}) || H <- ?MYHOSTS];
        false ->
            erlang:send_after(250, self(), finish_init)
    end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


is_app_running() ->
    {Apps, _, _} = lists:unzip3(application:which_applications()),
    lists:any(fun(A) -> A == ejabberd end, Apps).


%% Internal functions

register_routes(Host) ->
    ?INFO_MSG("Registering routes for ~p~n", [Host]),
    ejabberd_local:register_host(Host).

unregister_routes(Host) ->
    ?INFO_MSG("Unregistering routes for ~p~n", [Host]),
    ejabberd_local:unregister_host(Host).

start_modules(Host) ->
    ?INFO_MSG("Starting modules for ~p~n", [Host]),
    Modules = gen_mod:get_host_modules(Host),
    lists:foreach(
        fun({Module,Args}) ->
            gen_mod:start_module(Host, Module, Args)
        end, Modules).

stop_modules(Host) ->
    ?INFO_MSG("Stopping modules for ~p~n", [Host]),
    Modules = gen_mod:loaded_modules(Host),
    lists:foreach(
        fun(Module) ->
	        gen_mod:stop_module_keep_config(Host, Module)
    	end, Modules).

get_local_running_hosts() ->
    case ejabberd_config:get_local_option(running_hosts) of
        undefined -> [];
        HostList -> HostList
    end.

set_host_active(Host) ->
    Hosts = [Host|get_local_running_hosts()],
    ejabberd_config:add_local_option(running_hosts, lists:usort(Hosts)).

set_host_inactive(Host) ->
    Hosts = get_local_running_hosts()--[Host],
    ejabberd_config:add_local_option(running_hosts, lists:usort(Hosts)).

is_valid_host(Host) ->
    lists:member(Host,get_host_list()).

is_stopped_host(Host) ->
    case is_valid_host(Host) of
        true ->
            not lists:member(Host,get_local_running_hosts());
        _ -> undefined
    end.
            
is_running_host(Host) ->
    case is_valid_host(Host) of
        true ->
            lists:member(Host,get_local_running_hosts());
        _ -> undefined
    end.


