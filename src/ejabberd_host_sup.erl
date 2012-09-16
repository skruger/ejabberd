-module(ejabberd_host_sup).

-behaviour(supervisor).

-export([start_link/1,init/1,get_name/1]).
-export([start_child/2,stop_child/2]).


get_name(Host) ->
    list_to_atom("ejabberd_host_"++Host++"_sup").

start_link(Host) ->
    Name = get_name(Host),
    supervisor:start_link({local,Name}, ?MODULE, Host).

start_child(Host, ChildSpec) ->
    Name = get_name(Host),
    supervisor:start_child(Name, ChildSpec).

stop_child(Host, ChildName) ->
    Name = get_name(Host),
    {catch supervisor:terminate_child(Name, ChildName),
     catch supervisor:delete_child(Name, ChildName)}.

init(_Host) ->
    {ok, {{one_for_one, 5, 1000}, []}}.

