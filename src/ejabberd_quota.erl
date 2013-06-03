%%%----------------------------------------------------------------------
%%% File    : ejabberd_quota.erl
%%% Author  : Shaun Kruger <skruger@chatmongers.com>
%%% Purpose : Authentification
%%% Created : 2 June 2013 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% fork21 contributed, Copyright (C) 2013   Chatmongers, LLC
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

-module(ejabberd_quota).
-author('skruger@chatmongers.com').

-export([
    allow_domain_login/1
    ]).

allow_domain_login(Domain) ->
    QuotaCt = 
    case ejabberd_config:get_local_option({concurrent_user_quota, Domain}) of
        undefined -> infinity;
        Ct -> Ct
    end,
    ?DEBUG("Got concurrent_user_quota = ~p for ~p~n", [QuotaCt, Domain]),
    CurrentUserCt = ejabberd_sm:get_vh_session_number(Domain),
    QuotaCt > CurrentUserCt.


