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

-import(proplists, [get_value/2]).

-import(minirest,  [return/0, return/1]).

-rest_api(#{name   => list_emqx_acl,
            method => 'GET',
            path   => "/emqx_acl",
            func   => list,
            descr  => "List available mnesia in the cluster"
           }).

-rest_api(#{name   => lookup_emqx_acl,
            method => 'GET',
            path   => "/emqx_acl/:bin:key",
            func   => lookup,
            descr  => "Lookup mnesia in the cluster"
           }).

-rest_api(#{name   => add_emqx_acl,
            method => 'POST',
            path   => "/emqx_acl",
            func   => add,
            descr  => "Add mnesia in the cluster"
           }).

-rest_api(#{name   => delete_emqx_acl,
            method => 'DELETE',
            path   => "/emqx_acl/:bin:key",
            func   => delete,
            descr  => "Delete mnesia in the cluster"
           }).

-export([ list/2
        , lookup/2
        , add/2
        , delete/2
        ]).

list(_Bindings, _Params) ->
    return({ok, emqx_acl_mnesia_cli:all_acls()}).

lookup(#{key := Key}, _Params) ->
    return({ok, format(emqx_acl_mnesia_cli:lookup_acl(Key))}).

add(_Bindings, Params) ->
    [ P | _] = Params,
    case is_list(P) of
        true -> return(add_acl(Params, []));
        false -> return(add_acl([Params], []))
    end.

add_acl([ Params | ParamsN ], ReList ) ->
    Key = get_value(<<"key">>, Params),
    Topic = get_value(<<"topic">>, Params),
    Action = get_value(<<"action">>, Params),
    Re = case validate([key, topic, action], [Key, Topic, Action]) of
        ok -> 
            emqx_acl_mnesia_cli:add_acl(Key, Topic, Action);
        Err -> Err
    end,
    add_acl(ParamsN, [{Key, format_msg(Re)} | ReList]);   
    
add_acl([], ReList) ->
    {ok, ReList}.

delete(#{key := Key}, _) ->
    return(emqx_acl_mnesia_cli:remove_acl(Key)).

%%------------------------------------------------------------------------------
%% Interval Funcs
%%------------------------------------------------------------------------------

format([]) ->
    #{};

format([{emqx_acl, Key, Topic, Action}]) ->
    #{key => Key, topic => Topic, action => Action};

format([{emqx_acl, _Key, _Topic, _Action} | _] = List) ->
    format(List, []).
    
format([{emqx_acl, Key, Topic, Action} | List], ReList) ->
    format(List, [ #{key => Key, topic => Topic, action => Action} | ReList]);
format([], ReList) -> ReList.

validate([], []) ->
    ok;
validate([K|Keys], [V|Values]) ->
   case validation(K, V) of
       false -> {error, K};
       true  -> validate(Keys, Values)
   end.

validation(key, V) when is_binary(V)
                     andalso byte_size(V) > 0 ->
    true;
validation(topic, V) when is_binary(V)
                     andalso byte_size(V) > 0 ->
    true;
validation(action, V) when is_binary(V) ->
    case V =:= <<"pub">> orelse V =:= <<"sub">> orelse V =:= <<"pubsub">> of
        true -> true;
        false -> false
    end;
validation(_, _) ->
    false.

format_msg(Message)
  when is_atom(Message);
       is_binary(Message) -> Message;

format_msg(Message) when is_tuple(Message) ->
    iolist_to_binary(io_lib:format("~p", [Message])).