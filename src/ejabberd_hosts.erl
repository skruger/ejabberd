-module(ejabberd_hosts).

-behaviour(gen_server).

-include("ejabberd.hrl").

-export([start_link/0]).

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([stop_all_hosts/0,start_host/1,stop_host/1]).

-record(hosts_state,{active_hosts}).

start_link() ->
    gen_server:start_link({local, ?MODULE},?MODULE,[],[]).

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
handle_call(_Msg, _From, State) ->
    {reply, {error, invalid}, State}.

handle_cast({start_host, Host, ReplyTo}, State) ->
    ?ERROR_MSG("~p~n", [ {start_host, Host, ReplyTo} ]),
    HostSupName = ejabberd_host_sup:get_name(Host),
    HostSup = {HostSupName, {ejabberd_host_sup, start_link, [Host]},
               permanent, 10000, supervisor, [ejabberd_host_sup]},
    SupStart = supervisor:start_child(ejabberd_sup, HostSup),
    ejabberd_rdbms:start_host(Host),
    Auth = ejabberd_auth:start(Host),
    start_modules(Host),
    register_routes(Host),
    gen_server:cast(self(),{reply, ok, ReplyTo}),
    ?INFO_MSG("Supervisor result: ~n~p~nAuth results: ~n~p~n",[SupStart,Auth]),
    {noreply, State};
handle_cast({stop_host, Host, ReplyTo}, State) ->
    ?ERROR_MSG("~p~n", [ {stop_host, Host, ReplyTo} ]),
    stop_modules(Host),
    unregister_routes(Host),
    Auth = ejabberd_auth:stop(Host),
    ejabberd_rdbms:stop_host(Host),
    gen_server:cast(self(),{reply, ok, ReplyTo}),
    HostSupName = ejabberd_host_sup:get_name(Host),
    catch supervisor:terminate_child(ejabberd_sup, HostSupName),
    catch supervisor:delete_child(ejabberd_sup, HostSupName),
    ?INFO_MSG("Auth results: ~n~p~n",[Auth]),
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

