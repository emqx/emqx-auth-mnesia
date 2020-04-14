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

-module(emqx_auth_mnesia_cli).

-include("emqx_auth_mnesia.hrl").

%% APIs
-export([ add_user/3
        , update_user/3
        , remove_user/1
        , lookup_user/1
        , all_users/0
        ]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

%% @doc Add User
-spec(add_user(binary(), binary(), atom()) -> ok | {error, any()}).
add_user(Login, Password, IsSuperuser) ->
    User = #emqx_user{login = Login, password = encrypted_data(Password), is_superuser = IsSuperuser},
    ret(mnesia:transaction(fun insert_user/1, [User])).

insert_user(User = #emqx_user{login = Login}) ->
    case mnesia:read(emqx_user, Login) of
        []    -> mnesia:write(User);
        [_|_] -> mnesia:abort(existed)
    end.

%% @doc Update User
-spec(update_user(binary(), binary(), atom()) -> ok | {error, any()}).
update_user(Login, NewPassword, IsSuperuser) ->
    User = #emqx_user{login = Login, password = encrypted_data(NewPassword), is_superuser = IsSuperuser},
    ret(mnesia:transaction(fun do_update_user/1, [User])).

do_update_user(User = #emqx_user{login = Login}) ->
    case mnesia:read(emqx_user, Login) of
        [_|_] -> mnesia:write(User);
        [] -> mnesia:abort(noexisted)
    end.

%% @doc Lookup user by login
-spec(lookup_user(binary()) -> list()).
lookup_user(Login) ->
    mnesia:dirty_read(emqx_user, Login).

%% @doc Remove user
-spec(remove_user(binary()) -> ok | {error, any()}).
remove_user(Login) ->
    ret(mnesia:transaction(fun mnesia:delete/1, [{emqx_user, Login}])).

ret({atomic, ok})     -> ok;
ret({aborted, Error}) -> {error, Error}.

%% @doc All logins
-spec(all_users() -> list()).
all_users() -> mnesia:dirty_all_keys(emqx_user).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

encrypted_data(Password) ->
    HashType = application:get_env(emqx_auth_mnesia, hash_type, sha256),
    emqx_passwd:hash(HashType, Password).
