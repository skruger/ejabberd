%%%----------------------------------------------------------------------
%%% File    : ejabberd_rdbms.erl
%%% Author  : Mickael Remond <mickael.remond@process-one.net>
%%% Purpose : Manage the start of the database modules when needed
%%% Created : 31 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2013   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_rdbms).
-author('alexey@process-one.net').

-export([start_host/1,stop_host/1]).
-include("ejabberd.hrl").

start_host(Host) ->
    case catch ejabberd_odbc_sup:module_info() of
	{'EXIT',{undef,_}} ->
	    ?INFO_MSG("ejabberd has not been compiled with relational database support. Skipping database startup.", []),
        no_odbc;
	_ ->
        case needs_odbc(Host) of
            true -> start_odbc(Host);
            false -> ok
        end
    end.

stop_host(Host) ->
    Supervisor_name = gen_mod:get_module_proc(Host, ejabberd_odbc_sup),
    ejabberd_host_sup:stop_child(Host, Supervisor_name).

%% Start the ODBC module on the given host
start_odbc(Host) ->
    Supervisor_name = gen_mod:get_module_proc(Host, ejabberd_odbc_sup),
    ChildSpec =
	{Supervisor_name,
	 {ejabberd_odbc_sup, start_link, [Host]},
	 transient,
	 infinity,
	 supervisor,
	 [ejabberd_odbc_sup]},
    case ejabberd_host_sup:start_child(Host, ChildSpec) of
	{ok, _PID} ->
	    ok;
	_Error ->
	    ?ERROR_MSG("Start of supervisor ~p failed:~n~p~nRetrying...~n", [Supervisor_name, _Error]),
	    start_odbc(Host)
    end.

%% Returns true if we have configured odbc_server for the given host
needs_odbc(Host) ->
    LHost = jlib:nameprep(Host),
    case ejabberd_config:get_local_option({odbc_server, LHost}) of
	undefined ->
	    false;
	_ -> true
    end.
