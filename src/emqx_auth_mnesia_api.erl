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

-module(emqx_auth_mnesia_api).

-import(proplists, [get_value/2]).

-import(minirest,  [return/0, return/1]).

-rest_api(#{name   => list_emqx_user,
            method => 'GET',
            path   => "/emqx_user",
            func   => list,
            descr  => "List available mnesia in the cluster"
           }).

-rest_api(#{name   => lookup_emqx_user,
            method => 'GET',
            path   => "/emqx_user/:bin:login",
            func   => lookup,
            descr  => "Lookup mnesia in the cluster"
           }).

-rest_api(#{name   => add_emqx_user,
            method => 'POST',
            path   => "/emqx_user",
            func   => add,
            descr  => "Add mnesia in the cluster"
           }).

-rest_api(#{name   => update_emqx_user,
            method => 'PUT',
            path   => "/emqx_user/:bin:login",
            func   => update,
            descr  => "Update mnesia in the cluster"
           }).

-rest_api(#{name   => delete_emqx_user,
            method => 'DELETE',
            path   => "/emqx_user/:bin:login",
            func   => delete,
            descr  => "Delete mnesia in the cluster"
           }).

-export([ list/2
        , lookup/2
        , add/2
        , update/2
        , delete/2
        ]).

list(_Bindings, _Params) ->
    return({ok, emqx_auth_mnesia_cli:all_users()}).

lookup(#{login := Longin}, _Params) ->
    return({ok, format(emqx_auth_mnesia_cli:lookup_user(Longin))}).

add(_Bindings, Params = [{_,_} | _]) ->
    return(add_user([Params], []));
add(_Bindings, Params = [[{_,_} | _] | _]) ->
    return(add_user(Params, [])).

add_user([ Params | ParamsN ], ReList ) ->
    Longin = get_value(<<"login">>, Params),
    Password = get_value(<<"password">>, Params),
    IsSuperuser = get_value(<<"is_superuser">>, Params),
    Re = case validate([login, password, is_superuser], [Longin, Password, IsSuperuser]) of
        ok -> 
            emqx_auth_mnesia_cli:add_user(Longin, Password, IsSuperuser);
        Err -> Err
    end,
    add_user(ParamsN, [{Longin, format_msg(Re)} | ReList]);   
    
add_user([], ReList) ->
    {ok, ReList}.

update(#{login := Longin}, Params) ->
    Password = get_value(<<"password">>, Params),
    IsSuperuser = get_value(<<"is_superuser">>, Params),
    case validate([password, is_superuser], [Password, IsSuperuser]) of
        ok ->
            case emqx_auth_mnesia_cli:update_user(Longin, Password, IsSuperuser) of
                ok  -> return();
                Err -> return(Err)
            end;
        Err -> return(Err)
    end.

delete(#{login := Longin}, _) ->
    ok = emqx_auth_mnesia_cli:remove_user(Longin),
    return().

%%------------------------------------------------------------------------------
%% Interval Funcs
%%------------------------------------------------------------------------------

format([]) ->
    #{};

format([{emqx_user, Longin, Password, IsSuperuser}]) ->
    #{login => Longin,
      password => Password,
      is_superuser => IsSuperuser}.

validate([], []) ->
    ok;
validate([K|Keys], [V|Values]) ->
   case validation(K, V) of
       false -> {error, K};
       true  -> validate(Keys, Values)
   end.

validation(login, V) when is_binary(V)
                     andalso byte_size(V) > 0 ->
    true;
validation(password, V) when is_binary(V)
                     andalso byte_size(V) > 0 ->
    true;
validation(is_superuser, V) when is_atom(V) ->
    case V =:= false orelse V =:= true of
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