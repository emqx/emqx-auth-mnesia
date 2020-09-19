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

-module(emqx_acl_mnesia_api).

-include("emqx_auth_mnesia.hrl").

-include_lib("stdlib/include/ms_transform.hrl").

-import(proplists, [get_value/2]).

-import(minirest,  [return/1]).

-rest_api(#{name   => list_clientid,
            method => 'GET',
            path   => "/acl/clientid",
            func   => list_clientid,
            descr  => "List available mnesia in the cluster"
           }).

-rest_api(#{name   => list_username,
            method => 'GET',
            path   => "/acl/username",
            func   => list_username,
            descr  => "List available mnesia in the cluster"
           }).

-rest_api(#{name   => list_all,
            method => 'GET',
            path   => "/acl",
            func   => list_all,
            descr  => "List available mnesia in the cluster"
           }).

-rest_api(#{name   => lookup_clientid,
            method => 'GET',
            path   => "/acl/clientid/:bin:clientid",
            func   => lookup_clientid,
            descr  => "Lookup mnesia in the cluster"
           }).

-rest_api(#{name   => lookup_username,
            method => 'GET',
            path   => "/acl/username/:bin:username",
            func   => lookup_username,
            descr  => "Lookup mnesia in the cluster"
           }).

-rest_api(#{name   => add_clientid,
            method => 'POST',
            path   => "/acl/clientid",
            func   => add_clientid,
            descr  => "Add mnesia in the cluster"
           }).

-rest_api(#{name   => add_username,
            method => 'POST',
            path   => "/acl/username",
            func   => add_username,
            descr  => "Add mnesia in the cluster"
           }).

-rest_api(#{name   => add_all,
            method => 'POST',
            path   => "/acl",
            func   => add_all,
            descr  => "Add mnesia in the cluster"
           }).

-rest_api(#{name   => delete_clientid,
            method => 'DELETE',
            path   => "/acl/clientid/:bin:clientid/:bin:topic",
            func   => delete_clientid,
            descr  => "Delete mnesia in the cluster"
           }).

-rest_api(#{name   => delete_username,
            method => 'DELETE',
            path   => "/acl/username/:bin:username/:bin:topic",
            func   => delete_username,
            descr  => "Delete mnesia in the cluster"
           }).

-rest_api(#{name   => delete_all,
            method => 'DELETE',
            path   => "/acl/:bin:topic",
            func   => delete_all,
            descr  => "Delete mnesia in the cluster"
           }).


-export([ list_clientid/2
        , list_username/2
        , list_all/2
        , lookup_clientid/2
        , lookup_username/2
        , add_clientid/2
        , add_username/2
        , add_all/2
        , delete_clientid/2
        , delete_username/2
        , delete_all/2
        ]).

list_clientid(_Bindings, Params) ->
    MatchSpec = ets:fun2ms(
                  fun({emqx_acl, {{clientid, Clientid}, Topic}, Action, Access }) -> {{clientid,Clientid}, Topic, Action,Access} end),
    return({ok, emqx_auth_mnesia_api:paginate(emqx_acl, MatchSpec, Params, fun format/1)}).

list_username(_Bindings, Params) ->
    MatchSpec = ets:fun2ms(
                  fun({emqx_acl, {{username, Username}, Topic}, Action, Access }) -> {{username, Username}, Topic, Action,Access} end),
    return({ok, emqx_auth_mnesia_api:paginate(emqx_acl, MatchSpec, Params, fun format/1)}).

list_all(_Bindings, Params) ->
    MatchSpec = ets:fun2ms(
                  fun({emqx_acl, {all, Topic}, Action, Access }) -> {all, Topic, Action,Access}end
                 ),
    return({ok, emqx_auth_mnesia_api:paginate(emqx_acl, MatchSpec, Params, fun format/1)}).


lookup_clientid(#{clientid := Clientid}, _Params) ->
    return({ok, format(emqx_acl_mnesia_cli:lookup_acl({clientid, urldecode(Clientid)}))}).

lookup_username(#{username := Username}, _Params) ->
    return({ok, format(emqx_acl_mnesia_cli:lookup_acl({username, urldecode(Username)}))}).

add_clientid(_Bindings, Params) ->
    [ P | _] = Params,
    case is_list(P) of
        true -> return(do_add_clientid(Params, []));
        false -> return(do_add_clientid([Params], []))
    end.

do_add_clientid([ Params | ParamsN ], ReList) ->
    Clientid = urldecode(get_value(<<"clientid">>, Params)),
    Topic = urldecode(get_value(<<"topic">>, Params)),
    Action = urldecode(get_value(<<"action">>, Params)),
    Access = urldecode(get_value(<<"access">>, Params)),
    Re = case validate([clientid, topic, action, access], [Clientid, Topic, Action, Access]) of
        ok -> 
            emqx_acl_mnesia_cli:add_acl({clientid, Clientid}, Topic, erlang:binary_to_atom(Action, utf8), erlang:binary_to_atom(Access, utf8));
        Err -> Err
    end,
    do_add_clientid(ParamsN, [{Clientid, format_msg(Re)} | ReList]);
    
do_add_clientid([], ReList) ->
    {ok, ReList}.

add_username(_Bindings, Params) ->
    [ P | _] = Params,
    case is_list(P) of
        true -> return(do_add_username(Params, []));
        false -> return(do_add_username([Params], []))
    end.

do_add_username([ Params | ParamsN ], ReList) ->
    Usernmae = urldecode(get_value(<<"username">>, Params)),
    Topic = urldecode(get_value(<<"topic">>, Params)),
    Action = urldecode(get_value(<<"action">>, Params)),
    Access = urldecode(get_value(<<"access">>, Params)),
    Re = case validate([username, topic, action, access], [Usernmae, Topic, Action, Access]) of
        ok ->
            emqx_acl_mnesia_cli:add_acl({username, Usernmae}, Topic, erlang:binary_to_atom(Action, utf8), erlang:binary_to_atom(Access, utf8));
        Err -> Err
    end,
    do_add_username(ParamsN, [{Usernmae, format_msg(Re)} | ReList]);

do_add_username([], ReList) ->
    {ok, ReList}.

add_all(_Bindings, Params) ->
    [ P | _] = Params,
    case is_list(P) of
        true -> return(do_add_all(Params, []));
        false -> return(do_add_all([Params], []))
    end.

do_add_all([ Params | ParamsN ], ReList) ->
    Topic = urldecode(get_value(<<"topic">>, Params)),
    Action = urldecode(get_value(<<"action">>, Params)),
    Access = urldecode(get_value(<<"access">>, Params)),
    Re = case validate([topic, action, access], [Topic, Action, Access]) of
        ok ->
            emqx_acl_mnesia_cli:add_acl(all, Topic, erlang:binary_to_atom(Action, utf8), erlang:binary_to_atom(Access, utf8));
        Err -> Err
    end,
    do_add_all(ParamsN, [{all, format_msg(Re)} | ReList]);
 
do_add_all([], ReList) ->
    {ok, ReList}.


delete_clientid(#{clientid := Clientid, topic := Topic}, _) ->
    return(emqx_acl_mnesia_cli:remove_acl({clientid, urldecode(Clientid)}, urldecode(Topic))).
delete_username(#{username := Username, topic := Topic}, _) ->
    return(emqx_acl_mnesia_cli:remove_acl({username, urldecode(Username)}, urldecode(Topic))).
delete_all(#{topic := Topic}, _) ->
    return(emqx_acl_mnesia_cli:remove_acl(all, urldecode(Topic))).

%%------------------------------------------------------------------------------
%% Interval Funcs
%%------------------------------------------------------------------------------
format({{clientid, Clientid}, Topic, Action, Access}) ->
    #{clientid => Clientid, topic => Topic, action => Action, access => Access};
format({{username, Username}, Topic, Action, Access}) ->
    #{username => Username, topic => Topic, action => Action, access => Access};
format({all, Topic, Action, Access}) ->
    #{all => all, topic => Topic, action => Action, access => Access};
format(List) when is_list(List) ->
    format(List, []).

format([L | List], Relist) ->
    format(List, [format(L) | Relist]);
format([], ReList) -> lists:reverse(ReList).

validate([], []) ->
    ok;
validate([K|Keys], [V|Values]) ->
   case do_validation(K, V) of
       false -> {error, K};
       true  -> validate(Keys, Values)
   end.

do_validation(clientid, V) when is_binary(V)
                     andalso byte_size(V) > 0 ->
    true;
do_validation(username, V) when is_binary(V)
                     andalso byte_size(V) > 0 ->
    true;
do_validation(topic, V) when is_binary(V)
                     andalso byte_size(V) > 0 ->
    true;
do_validation(action, V) when is_binary(V) ->
    case V =:= <<"pub">> orelse V =:= <<"sub">> orelse V =:= <<"pubsub">> of
        true -> true;
        false -> false
    end;
do_validation(access, V) when V =:= <<"allow">> orelse V =:= <<"deny">> ->
    true;
do_validation(_, _) ->
    false.

format_msg(Message)
  when is_atom(Message);
       is_binary(Message) -> Message;

format_msg(Message) when is_tuple(Message) ->
    iolist_to_binary(io_lib:format("~p", [Message])).

-if(?OTP_RELEASE >= 23).
urldecode(S) ->
    [{R, _}] = uri_string:dissect_query(S), R.
-else.
urldecode(S) ->
    http_uri:decode(S).
-endif.
