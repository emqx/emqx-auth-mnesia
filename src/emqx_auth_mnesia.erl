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

%% Auth callbacks
-export([ init/1
        , register_metrics/0
        , check/3
        , description/0
        ]).

%% APIs
-export([ add_user/3
        , update_user/3
        , remove_user/1
        , lookup_user/1
        , all_users/0
        ]).

-define(EMPTY(Username), (Username =:= undefined orelse Username =:= <<>>)).

-define(TAB, ?MODULE).

init(DefaultUsers) ->
    ok = ekka_mnesia:create_table(emqx_user, [
            {disc_copies, [node()]},
            {attributes, record_info(fields, emqx_user)}]),
    ok = lists:foreach(fun add_default_user/1, DefaultUsers),
    ok = ekka_mnesia:copy_table(emqx_user, disc_copies).

%% @private
add_default_user({Username, Password, IsSuperuser}) ->
    add_user(iolist_to_binary(Username), iolist_to_binary(Password), IsSuperuser).

-spec(register_metrics() -> ok).
register_metrics() ->
    lists:foreach(fun emqx_metrics:new/1, ?AUTH_METRICS).

check(ClientInfo = #{password := Password}, AuthResult, #{hash_type := HashType}) ->
    case emqx_auth_mnesia_cli:query(ClientInfo) of
        [] -> 
            emqx_metrics:inc(?AUTH_METRICS(ignore)),
            ok;
        [User] ->
            case emqx_passwd:check_pass({User#emqx_user.password, Password}, HashType) of
                ok -> 
                    emqx_metrics:inc(?AUTH_METRICS(success)),
                    {stop, AuthResult#{is_superuser => is_superuser(User),
                                        anonymous => false,
                                        auth_result => success}};
                {error, Reason} -> 
                    ?LOG(error, "[Mnesia] Auth from mnesia failed: ~p", [Reason]),
                    emqx_metrics:inc(?AUTH_METRICS(failure)),
                    {stop, AuthResult#{auth_result => password_error, anonymous => false}}
            end
    end.

description() -> "Authentication with Mnesia".

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

%% @doc Add User
-spec(add_user(binary(), binary(), atom()) -> ok | {error, any()}).
add_user(Username, Password, IsSuperuser) ->
    User = #emqx_user{username = Username, password = encrypted_data(Password), is_superuser = IsSuperuser},
    ret(mnesia:transaction(fun insert_user/1, [User])).

insert_user(User = #emqx_user{username = Username}) ->
    case mnesia:read(emqx_user, Username) of
        []    -> mnesia:write(User);
        [_|_] -> mnesia:abort(existed)
    end.

%% @doc Update User
-spec(update_user(binary(), binary(), atom()) -> ok | {error, any()}).
update_user(Username, NewPassword, IsSuperuser) ->
    User = #emqx_user{username = Username, password = encrypted_data(NewPassword), is_superuser = IsSuperuser},
    ret(mnesia:transaction(fun do_update_user/1, [User])).

do_update_user(User = #emqx_user{username = Username}) ->
    case mnesia:read(emqx_user, Username) of
        [_|_] -> mnesia:write(User);
        [] -> mnesia:abort(noexisted)
    end.

%% @doc Lookup user by username
-spec(lookup_user(binary()) -> list()).
lookup_user(Username) ->
    mnesia:dirty_read(emqx_user, Username).

%% @doc Remove user
-spec(remove_user(binary()) -> ok | {error, any()}).
remove_user(Username) ->
    ret(mnesia:transaction(fun mnesia:delete/1, [{emqx_user, Username}])).

ret({atomic, ok})     -> ok;
ret({aborted, Error}) -> {error, Error}.

%% @doc All usernames
-spec(all_users() -> list()).
all_users() -> mnesia:dirty_all_keys(emqx_user).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
is_superuser(#emqx_user{is_superuser = true}) ->
    true;
is_superuser(_) ->
    false.

encrypted_data(Password) ->
    HashType = application:get_env(emqx_auth_mnesia, hash_type, sha256),
    emqx_passwd:hash(HashType, Password).
