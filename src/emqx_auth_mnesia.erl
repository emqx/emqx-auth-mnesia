%%--------------------------------------------------------------------
%% Copyright (c) 2020 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(emqx_auth_mnesia).

-include("emqx_auth_mnesia.hrl").

-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/logger.hrl").
-include_lib("emqx/include/types.hrl").

-export([ register_metrics/0
        , check/3
        , description/0
        ]).

-define(EMPTY(Username), (Username =:= undefined orelse Username =:= <<>>)).

-spec(register_metrics() -> ok).
register_metrics() ->
    lists:foreach(fun emqx_metrics:new/1, ?AUTH_METRICS).

check(ClientInfo = #{password := Password}, AuthResult, #{hash_type := HashType}) ->
    CheckPass = case emqx_auth_mnesia_cli:query(ClientInfo) of
        {ok, undefined} ->
            {error, not_found};
        {ok, User} ->
            {check_pass({User#emqx_user.password, Password}, HashType), User};
        {error, Reason} ->
            ?LOG(error, "[Mnesia] query '~p' failed: ~p", [ClientInfo, Reason]),
            {error, not_found}
    end,
    case CheckPass of
        {ok, User1} ->
            emqx_metrics:inc(?AUTH_METRICS(success)),
            {stop, AuthResult#{is_superuser => is_superuser(User1),
                                anonymous => false,
                                auth_result => success}};
        {error, not_found} ->
            emqx_metrics:inc(?AUTH_METRICS(ignore)), ok;
        {error, ResultCode} ->
            ?LOG(error, "[Mnesia] Auth from mnesia failed: ~p", [ResultCode]),
            emqx_metrics:inc(?AUTH_METRICS(failure)),
            {stop, AuthResult#{auth_result => ResultCode, anonymous => false}}
    end.

%%--------------------------------------------------------------------
%% Is Superuser?
%%--------------------------------------------------------------------
is_superuser(#emqx_user{is_superuser = true}) ->
    true;
is_superuser(_) ->
    false.

check_pass(Password, HashType) ->
    case emqx_passwd:check_pass(Password, HashType) of
        ok -> ok;
        {error, _Reason} -> {error, not_authorized}
    end.

description() -> "Authentication with Mnesia".