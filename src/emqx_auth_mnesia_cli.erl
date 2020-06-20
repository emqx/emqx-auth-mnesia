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
-define(TABLE, emqx_user).
%% Auth APIs
-export([ add_user/3
        , update_user/3
        , remove_user/1
        , lookup_user/1
        , all_users/0
        ]).
%% Acl APIs
-export([ add_acl/4
        , remove_acl/2
        , lookup_acl/1
        , all_acls/0
        ]).
-export([cli/1]).
%%--------------------------------------------------------------------
%% Auth APIs
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
lookup_user(undefined) -> [];
lookup_user(Login) ->
    case mnesia:dirty_read(emqx_user, Login) of
        {error, Reason} ->
            ?LOG(error, "[Mnesia] do_check_user error: ~p~n", [Reason]),
            [];
        Re -> Re
    end.

%% @doc Remove user
-spec(remove_user(binary()) -> ok | {error, any()}).
remove_user(Login) ->
    ret(mnesia:transaction(fun mnesia:delete/1, [{emqx_user, Login}])).

%% @doc All logins
-spec(all_users() -> list()).
all_users() -> mnesia:dirty_all_keys(emqx_user).

%%--------------------------------------------------------------------
%% Acl API
%%--------------------------------------------------------------------

%% @doc Add Acls
-spec(add_acl(binary(), binary(), binary(), atom()) -> ok | {error, any()}).
add_acl(Login, Topic, Action, Allow) ->
    Acls = #emqx_acl{login = Login, topic = Topic, action = Action, allow = Allow},
    ret(mnesia:transaction(fun mnesia:write/1, [Acls])).

%% @doc Lookup acl by login
-spec(lookup_acl(binary()) -> list()).
lookup_acl(undefined) -> [];
lookup_acl(Login) ->
    case mnesia:dirty_read(emqx_acl, Login) of
        {error, Reason} ->
            ?LOG(error, "[Mnesia] do_check_acl error: ~p~n", [Reason]),
            [];
        Re -> Re
    end.

%% @doc Remove acl
-spec(remove_acl(binary(), binary()) -> ok | {error, any()}).
remove_acl(Login, Topic) ->
    [ ok = mnesia:dirty_delete_object(emqx_acl, #emqx_acl{login = Login, topic = Topic, action = Action, allow = Allow})  || [Action, Allow] <- ets:select(emqx_acl, [{{emqx_acl, Login, Topic,'$1','$2'}, [], ['$$']}])],
    ok.


%% @doc All logins
-spec(all_acls() -> list()).
all_acls() -> mnesia:dirty_all_keys(emqx_acl).


%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

ret({atomic, ok})     -> ok;
ret({aborted, Error}) -> {error, Error}.

encrypted_data(Password) ->
    HashType = application:get_env(emqx_auth_mnesia, hash_type, sha256),
    emqx_passwd:hash(HashType, Password).


%%--------------------------------------------------------------------
%% Auth APIs
%%--------------------------------------------------------------------
if_enabled(Fun) ->
    case is_enabled() of 
        true -> 
            Fun(); 
        false -> 
            hint() 
    end.
is_enabled() ->
    lists:member(?TABLE, mnesia:system_info(tables)).

hint() ->
    emqx_ctl:print("Please run './bin/emqx_ctl plugins load emqx_auth_mnesia' first.~n").

%% User
cli(["adduser", Login, Password, IsSuper]) ->
    if_enabled(fun() -> Ok = add_user(Login, Password, IsSuper), emqx_ctl:print("~p~n", [Ok]) end);

cli(["updateuser", Login, NewPassword, IsSuperuser]) ->
    if_enabled(fun() -> Ok = update_user(Login, NewPassword, IsSuperuser), emqx_ctl:print("~p~n", [Ok]) end);

cli(["deluser", Login]) ->
    if_enabled(fun() -> Ok = remove_user(Login), emqx_ctl:print("~p~n", [Ok]) end);

cli(["lookupuser",P]) ->
    if_enabled(fun() -> Ok = lookup_user(P), emqx_ctl:print("~p~n", [Ok]) end);

cli(["allusers"]) ->
    if_enabled(fun() -> Ok = all_users(), emqx_ctl:print("~p~n", [Ok]) end);

%% Acl
cli(["addacl", Login, Topic, Action, Allow]) ->
    if_enabled(fun() ->Ok = add_acl(Login, Topic, Action, Allow), emqx_ctl:print("~p~n", [Ok]) end);

cli(["delacl", Login, Topic])->
    if_enabled(fun() -> Ok = remove_acl(Login, Topic), emqx_ctl:print("~p~n", [Ok]) end);

cli(["lookupacl",P]) ->
    if_enabled(fun() -> Ok = lookup_acl(P), emqx_ctl:print("~p~n", [Ok]) end);

cli(["allacls"]) ->
    if_enabled(fun() -> Ok = all_acls(), emqx_ctl:print("~p~n", [Ok]) end);

cli(_) ->
    emqx_ctl:usage([{"authmnesia adduser <Login> <Password> <IsSuper>", "Add User"},
                    {"authmnesia updateuser <Login> <NewPassword> <IsSuper>", "Update User"},
                    {"authmnesia deluser <Login>", "Delete User"},
                    {"authmnesia lookupuser <Login>", "Lookup User"},
                    {"authmnesia allusers", "All User"},
                    {"authmnesia addacl <Login> <Topic> <Action> <Allow>", "Add Acl"},
                    {"authmnesia delacl <Login> <Topic>", "Delete Acl"},
                    {"authmnesia lookupacl <Login>", "Lookup Acl"},
                    {"authmnesia allacls ","All acls"}]).