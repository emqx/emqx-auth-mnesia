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

-module(emqx_acl_mnesia_cli).

-include("emqx_auth_mnesia.hrl").

%% APIs
-export([ add_acl/3
        , remove_acl/1
        , lookup_acl/1
        , all_acls/0
        ]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

%% @doc Add Acls
-spec(add_acl(binary(), binary(), binary()) -> ok | {error, any()}).
add_acl(Key, Topic, Action) ->
    Acls = #emqx_acl{key = Key, topic = Topic, action = Action},
    ret(mnesia:transaction(fun mnesia:write/1, [Acls])).

%% @doc Lookup acl by key
-spec(lookup_acl(binary()) -> list()).
lookup_acl(Key) ->
    mnesia:dirty_read(emqx_acl, Key).

%% @doc Remove acl
-spec(remove_acl(binary()) -> ok | {error, any()}).
remove_acl(Key) ->
    ret(mnesia:transaction(fun mnesia:delete/1, [{emqx_acl, Key}])).

ret({atomic, ok})     -> ok;
ret({aborted, Error}) -> {error, Error}.

%% @doc All logins
-spec(all_acls() -> list()).
all_acls() -> mnesia:dirty_all_keys(emqx_acl).
