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

-module(emqx_auth_mnesia_SUITE).

-compile(export_all).

-include("emqx_auth_mnesia.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-import(emqx_ct_http, [ request_api/3
                      , request_api/5
                      , get_http_data/1
                      , create_default_app/0
                      , default_auth_header/0
                      ]).

-define(HOST, "http://127.0.0.1:8081/").
-define(API_VERSION, "v4").
-define(BASE_PATH, "api").

all() ->
    emqx_ct:all(?MODULE).

groups() ->
    [].

init_per_suite(Config) ->
    emqx_ct_helpers:start_apps([emqx_management, emqx_auth_mnesia], fun set_special_configs/1),
    create_default_app(),
    Config.

end_per_suite(_Config) ->
    emqx_ct_helpers:stop_apps([emqx_auth_mnesia]).

set_special_configs(emqx) ->
    application:set_env(emqx, allow_anonymous, true),
    application:set_env(emqx, enable_acl_cache, false),
    LoadedPluginPath = filename:join(["test", "emqx_SUITE_data", "loaded_plugins"]),
    application:set_env(emqx, plugins_loaded_file,
                        emqx_ct_helpers:deps_path(emqx, LoadedPluginPath));

set_special_configs(_App) ->
    ok.

%%------------------------------------------------------------------------------
%% Testcases
%%------------------------------------------------------------------------------

t_check_as_username(_Config) ->
    clean_all_users(),
    application:set_env(emqx_auth_mnesia, as, username),

    ok = emqx_auth_mnesia_cli:add_user(<<"test_username">>, <<"password">>, true),
    {error, existed} = emqx_auth_mnesia_cli:add_user(<<"test_username">>, <<"password">>, true),

    ok = emqx_auth_mnesia_cli:update_user(<<"test_username">>, <<"new_password">>, false),
    {error,noexisted} = emqx_auth_mnesia_cli:update_user(<<"no_existed_user">>, <<"password">>, true),

    [<<"test_username">>] = emqx_auth_mnesia_cli:all_users(),
    [{emqx_user, <<"test_username">>, _HashedPass, false}] =
    emqx_auth_mnesia_cli:lookup_user(<<"test_username">>),

    User1 = #{username => <<"test_username">>,
              password => <<"new_password">>,
              zone     => external},

    {ok, #{is_superuser := false, 
           auth_result := success,
           anonymous := false}} = emqx_access_control:authenticate(User1),

    {error,password_error} = emqx_access_control:authenticate(User1#{password => <<"error_password">>}),

    ok = emqx_auth_mnesia_cli:remove_user(<<"test_username">>),
    {ok, #{auth_result := success,
           anonymous := true }} = emqx_access_control:authenticate(User1).

t_check_as_clientid(_Config) ->
    clean_all_users(),
    application:set_env(emqx_auth_mnesia, as, clientid),

    ok = emqx_auth_mnesia_cli:add_user(<<"test_clientid">>, <<"password">>, false),
    {error, existed} = emqx_auth_mnesia_cli:add_user(<<"test_clientid">>, <<"password">>, false),

    ok = emqx_auth_mnesia_cli:update_user(<<"test_clientid">>, <<"new_password">>, true),
    {error,noexisted} = emqx_auth_mnesia_cli:update_user(<<"no_existed_user">>, <<"password">>, true),

    [<<"test_clientid">>] = emqx_auth_mnesia_cli:all_users(),
    [{emqx_user, <<"test_clientid">>, _HashedPass, true}] =
    emqx_auth_mnesia_cli:lookup_user(<<"test_clientid">>),

    User1 = #{clientid => <<"test_clientid">>,
              password => <<"new_password">>,
              zone     => external},

    {ok, #{is_superuser := true, 
           auth_result := success,
           anonymous := false}} = emqx_access_control:authenticate(User1),

    {error,password_error} = emqx_access_control:authenticate(User1#{password => <<"error_password">>}),

    ok = emqx_auth_mnesia_cli:remove_user(<<"test_clientid">>),
    {ok, #{auth_result := success,
           anonymous := true }} = emqx_access_control:authenticate(User1).

t_rest_api(_) ->
    clean_all_users(),
    application:set_env(emqx_auth_mnesia, as, username),

    {ok, Result} = request_http_rest_list(),
    [] = get_http_data(Result),

    Params = #{<<"login">> => <<"test_username">>, <<"password">> => <<"password">>, <<"is_superuser">> => true},
    {ok, _} = request_http_rest_add(Params),

    Params1 = [
                #{<<"login">> => <<"test_username">>, <<"password">> => <<"password">>, <<"is_superuser">> => true},
                #{<<"login">> => <<"test_username_1">>, <<"password">> => <<"password">>, <<"is_superuser">> => error_format},
                #{<<"login">> => <<"test_username_2">>, <<"password">> => <<"password">>, <<"is_superuser">> => true}
                ],
    {ok, AddResult} = request_http_rest_add(Params1),
    #{
        <<"test_username">> := <<"{error,existed}">>,
        <<"test_username_1">> := <<"{error,is_superuser}">>,
        <<"test_username_2">> := <<"ok">>
        } = get_http_data(AddResult),

    {ok, Result1} = request_http_rest_lookup(<<"test_username">>),
    #{<<"login">> := <<"test_username">>, <<"is_superuser">> := true} = get_http_data(Result1),

    {ok, _} = request_http_rest_update(<<"test_username">>, <<"new_password">>, error_format),
    {ok, _} = request_http_rest_update(<<"error_username">>, <<"new_password">>, false),

    {ok, _} = request_http_rest_update(<<"test_username">>, <<"new_password">>, false),
    {ok, Result2} = request_http_rest_lookup(<<"test_username">>),
    #{<<"login">> := <<"test_username">>, <<"is_superuser">> := false} = get_http_data(Result2),

    User1 = #{username => <<"test_username">>,
        password => <<"new_password">>,
        zone     => external},

    {ok, #{is_superuser := false, 
        auth_result := success,
        anonymous := false}} = emqx_access_control:authenticate(User1),

    {ok, _} = request_http_rest_delete(<<"test_username">>),
    {ok, #{auth_result := success,
           anonymous := true }} = emqx_access_control:authenticate(User1).

%%------------------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------------------

clean_all_users() ->
    [ mnesia:dirty_delete({emqx_user, Login})
      || Login <- mnesia:dirty_all_keys(emqx_user)].

%%--------------------------------------------------------------------
%% HTTP Request
%%--------------------------------------------------------------------

request_http_rest_list() ->
    request_api(get, uri(), default_auth_header()).

request_http_rest_lookup(Login) ->
    request_api(get, uri([Login]), default_auth_header()).

request_http_rest_add(Params) ->
    request_api(post, uri(), [], default_auth_header(), Params).

request_http_rest_update(Login, Password, IsSuperuser) ->
    Params = #{<<"password">> => Password, <<"is_superuser">> => IsSuperuser},
    request_api(put, uri([Login]), [], default_auth_header(), Params).

request_http_rest_delete(Login) ->
    request_api(delete, uri([Login]), default_auth_header()).

uri() -> uri([]).
uri(Parts) when is_list(Parts) ->
    NParts = [b2l(E) || E <- Parts],
    ?HOST ++ filename:join([?BASE_PATH, ?API_VERSION, "emqx_user"| NParts]).

%% @private
b2l(B) when is_binary(B) ->
    binary_to_list(B);
b2l(L) when is_list(L) ->
    L.
