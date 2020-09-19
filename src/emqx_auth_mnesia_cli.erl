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
-include_lib("emqx/include/logger.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-define(TABLE, emqx_user).
%% Auth APIs
-export([ add_user/2
        , update_user/2
        , remove_user/1
        , lookup_user/1
        , all_users/0
        , all_users/1
        ]).
%% Cli
-export([ auth_clientid_cli/1
        , auth_username_cli/1
        ]).
%%--------------------------------------------------------------------
%% Auth APIs
%%--------------------------------------------------------------------

%% @doc Add User
-spec(add_user(tuple(), binary()) -> ok | {error, any()}).
add_user(Login, Password) ->
    User = #emqx_user{login = Login, password = encrypted_data(Password)},
    ret(mnesia:transaction(fun insert_user/1, [User])).

insert_user(User = #emqx_user{login = Login}) ->
    case mnesia:read(?TABLE, Login) of
        []    -> mnesia:write(User);
        [_|_] -> mnesia:abort(existed)
    end.

%% @doc Update User
-spec(update_user(tuple(), binary()) -> ok | {error, any()}).
update_user(Login, NewPassword) ->
    User = #emqx_user{login = Login, password = encrypted_data(NewPassword)},
    ret(mnesia:transaction(fun do_update_user/1, [User])).

do_update_user(User = #emqx_user{login = Login}) ->
    case mnesia:read(?TABLE, Login) of
        [_|_] -> mnesia:write(User);
        [] -> mnesia:abort(noexisted)
    end.

%% @doc Lookup user by login
-spec(lookup_user(tuple()) -> list()).
lookup_user(undefined) -> [];
lookup_user(Login) ->
    case mnesia:dirty_read(?TABLE, Login) of
        {error, Reason} ->
            ?LOG(error, "[Mnesia] do_check_user error: ~p~n", [Reason]),
            [];
        Re -> Re
    end.

%% @doc Remove user
-spec(remove_user(tuple()) -> ok | {error, any()}).
remove_user(Login) ->
    ret(mnesia:transaction(fun mnesia:delete/1, [{?TABLE, Login}])).

%% @doc All logins
-spec(all_users() -> list()).
all_users() -> mnesia:dirty_all_keys(?TABLE).

all_users(clientid) ->
    MatchSpec = ets:fun2ms(fun({?TABLE, {clientid, Clientid}, _Password}) -> Clientid end),
    ets:select(?TABLE, MatchSpec);

all_users(username) ->
    MatchSpec = ets:fun2ms(fun({?TABLE, {username, Username}, _Password}) -> Username end),
    ets:select(?TABLE, MatchSpec).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

ret({atomic, ok})     -> ok;
ret({aborted, Error}) -> {error, Error}.

encrypted_data(Password) ->
    HashType = application:get_env(emqx_auth_clientid, password_hash, sha256),
    SaltBin = salt(),
    <<SaltBin/binary, (hash(Password, SaltBin, HashType))/binary>>.

hash(undefined, SaltBin, HashType) ->
    hash(<<>>, SaltBin, HashType);
hash(Password, SaltBin, HashType) ->
    emqx_passwd:hash(HashType, <<SaltBin/binary, Password/binary>>).

salt() ->
    rand:seed(exsplus, erlang:timestamp()),
    Salt = rand:uniform(16#ffffffff), <<Salt:32>>.

%%--------------------------------------------------------------------
%% Auth Clientid Cli
%%--------------------------------------------------------------------

auth_clientid_cli(["list"]) ->
    [emqx_ctl:print("~s~n", [ClientId]) || ClientId <- all_users(clientid)];

auth_clientid_cli(["add", ClientId, Password]) ->
    case add_user({clientid, iolist_to_binary(ClientId)}, iolist_to_binary(Password)) of
        ok -> emqx_ctl:print("ok~n");
        {error, Reason} -> emqx_ctl:print("Error: ~p~n", [Reason])
    end;

auth_clientid_cli(["update", ClientId, NewPassword]) ->
    case update_user({clientid, iolist_to_binary(ClientId)}, iolist_to_binary(NewPassword)) of
        ok -> emqx_ctl:print("ok~n");
        {error, Reason} -> emqx_ctl:print("Error: ~p~n", [Reason])
    end;

auth_clientid_cli(["del", ClientId]) ->
    case  remove_user({clientid, iolist_to_binary(ClientId)}) of
        ok -> emqx_ctl:print("ok~n");
        {error, Reason} -> emqx_ctl:print("Error: ~p~n", [Reason])
    end;

auth_clientid_cli(_) ->
    emqx_ctl:usage([{"clientid list", "List ClientId"},
                    {"clientid add <ClientId> <Password>", "Add ClientId"},
                    {"clientid update <Clientid> <NewPassword>", "Update Clientid"},
                    {"clientid del <ClientId>", "Delete ClientId"}]).

%%--------------------------------------------------------------------
%% Auth Username Cli
%%--------------------------------------------------------------------

auth_username_cli(["list"]) ->
    [emqx_ctl:print("~s~n", [Username]) || Username <- all_users(username)];

auth_username_cli(["add", Username, Password]) ->
    case add_user({username, iolist_to_binary(Username)}, iolist_to_binary(Password)) of
        ok -> emqx_ctl:print("ok~n");
        {error, Reason} -> emqx_ctl:print("Error: ~p~n", [Reason])
    end;

auth_username_cli(["update", Username, NewPassword]) ->
    case update_user({username, iolist_to_binary(Username)}, iolist_to_binary(NewPassword)) of
        ok -> emqx_ctl:print("ok~n");
        {error, Reason} -> emqx_ctl:print("Error: ~p~n", [Reason])
    end;

auth_username_cli(["del", Username]) ->
    case  remove_user({username, iolist_to_binary(Username)}) of
        ok -> emqx_ctl:print("ok~n");
        {error, Reason} -> emqx_ctl:print("Error: ~p~n", [Reason])
    end;

auth_username_cli(_) ->
    emqx_ctl:usage([{"users list", "List users"},
                    {"users add <Username> <Password>", "Add User"},
                    {"users update <Username> <NewPassword>", "Update User"},
                    {"users del <Username>", "Delete User"}]).
