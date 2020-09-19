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

-module(emqx_acl_mnesia).

-include("emqx_auth_mnesia.hrl").

-include_lib("stdlib/include/ms_transform.hrl").

-define(TABLE, emqx_acl).

%% ACL Callbacks
-export([ init/0
        , register_metrics/0
        , check_acl/5
        , description/0
        ]).

init() ->
    ok = ekka_mnesia:create_table(emqx_acl, [
            {type, ordered_set},
            {disc_copies, [node()]},
            {attributes, record_info(fields, emqx_acl)},
            {storage_properties, [{ets, [{read_concurrency, true}]}]}]),
    ok = ekka_mnesia:copy_table(emqx_user, disc_copies).

-spec(register_metrics() -> ok).
register_metrics() ->
    lists:foreach(fun emqx_metrics:ensure/1, ?ACL_METRICS).

check_acl(ClientInfo = #{ clientid := Clientid }, PubSub, Topic, _NoMatchAction, _Params) ->
    Username = maps:get(username, ClientInfo, undefined),

    Acls = case Username of
               undefined ->
                   emqx_acl_mnesia_cli:lookup_acl({clientid, Clientid}) ++
                   emqx_acl_mnesia_cli:lookup_acl(all);
               _ ->
                   emqx_acl_mnesia_cli:lookup_acl({clientid, Clientid}) ++
                   emqx_acl_mnesia_cli:lookup_acl({username, Username}) ++
                   emqx_acl_mnesia_cli:lookup_acl(all)
           end,

    case match(PubSub, Topic, Acls) of
        allow ->
            emqx_metrics:inc(?ACL_METRICS(allow)),
            {stop, allow};
        deny ->
            emqx_metrics:inc(?ACL_METRICS(deny)),
            {stop, deny};
        _ ->
            emqx_metrics:inc(?ACL_METRICS(ignore)),
            ok
    end.

description() -> "Acl with Mnesia".

%%--------------------------------------------------------------------
%% Internal functions
%%-------------------------------------------------------------------

match(_PubSub, _Topic, []) ->
    nomatch;
match(PubSub, Topic, [ {_, ACLTopic, Action, Access} | Acls]) ->
    case match_actions(PubSub, Action) andalso match_topic(Topic, ACLTopic) of
        true -> Access;
        false -> match(PubSub, Topic, Acls)
    end.

match_topic(Topic, ACLTopic) when is_binary(Topic) ->
    emqx_topic:match(Topic, ACLTopic).

match_actions(_, pubsub) -> true;
match_actions(subscribe, sub) -> true;
match_actions(publish, pub) -> true;
match_actions(_, _) -> false.
