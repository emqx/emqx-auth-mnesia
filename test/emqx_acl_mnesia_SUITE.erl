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

-module(emqx_acl_mnesia_SUITE).

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
    emqx_ct_helpers:stop_apps([emqx_management, emqx_auth_mnesia]).

init_per_testcase(t_check_acl_as_clientid, Config) ->
    emqx:hook('client.check_acl', fun emqx_acl_mnesia:check_acl/5, [#{key_as => clientid}]),
    Config;

init_per_testcase(_, Config) ->
    emqx:hook('client.check_acl', fun emqx_acl_mnesia:check_acl/5, [#{key_as => username}]),
    Config.

end_per_testcase(_, Config) ->
    emqx:unhook('client.check_acl', fun emqx_acl_mnesia:check_acl/5),
    Config.

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

t_management(_Config) ->
    clean_all_acls(),
    ?assertEqual([], emqx_auth_mnesia_cli:all_acls()),

    ok = emqx_auth_mnesia_cli:add_acl(<<"test_username">>, <<"Topic/A">>, <<"sub">>),
    ok = emqx_auth_mnesia_cli:add_acl(<<"test_username">>, <<"Topic/B">>, <<"pub">>),
    ok = emqx_auth_mnesia_cli:add_acl(<<"test_username">>, <<"Topic/C">>, <<"pubsub">>),
    
    ?assertEqual([{emqx_acl,<<"test_username">>,<<"Topic/A">>,<<"sub">>},
                  {emqx_acl,<<"test_username">>,<<"Topic/B">>,<<"pub">>},
                  {emqx_acl,<<"test_username">>,<<"Topic/C">>,<<"pubsub">>}],emqx_auth_mnesia_cli:lookup_acl(<<"test_username">>)),
    ok = emqx_auth_mnesia_cli:remove_acl(<<"test_username">>),
    ?assertEqual([], emqx_auth_mnesia_cli:lookup_acl(<<"test_username">>)).

t_check_acl_as_username(_Config) ->
    clean_all_acls(),
    emqx_modules:load_module(emqx_mod_acl_internal, false),
    
    User1 = #{zone => external, username => <<"test_username">>},
    User2 = #{zone => external, username => <<"no_exist">>},

    ok = emqx_auth_mnesia_cli:add_acl(<<"test_username">>, <<"Topic/A">>, <<"sub">>),
    ok = emqx_auth_mnesia_cli:add_acl(<<"test_username">>, <<"Topic/B">>, <<"pub">>),
    ok = emqx_auth_mnesia_cli:add_acl(<<"test_username">>, <<"Topic/A/B">>, <<"pubsub">>),
   
    allow = emqx_access_control:check_acl(User1, subscribe, <<"Topic/A">>),
    deny  = emqx_access_control:check_acl(User1, subscribe, <<"Topic/B">>),
    allow = emqx_access_control:check_acl(User1, subscribe, <<"Topic/A/B">>),
    deny  = emqx_access_control:check_acl(User1, publish,   <<"Topic/A">>),
    allow = emqx_access_control:check_acl(User1, publish,   <<"Topic/B">>),
    allow = emqx_access_control:check_acl(User1, publish,   <<"Topic/A/B">>),

    allow = emqx_access_control:check_acl(User2, subscribe, <<"Topic/C">>),
    allow = emqx_access_control:check_acl(User2, publish,   <<"Topic/D">>).

t_check_acl_as_clientid(_) ->
    clean_all_acls(),
    emqx_modules:load_module(emqx_mod_acl_internal, false),

    User1 = #{zone => external, clientid => <<"test_clientid">>},
    User2 = #{zone => external, clientid => <<"no_exist">>},

    ok = emqx_auth_mnesia_cli:add_acl(<<"test_clientid">>, <<"#">>, <<"sub">>),
    ok = emqx_auth_mnesia_cli:add_acl(<<"test_clientid">>, <<"+/A">>, <<"pub">>),
    ok = emqx_auth_mnesia_cli:add_acl(<<"test_clientid">>, <<"Topic/A/B">>, <<"pubsub">>),

    allow = emqx_access_control:check_acl(User1, subscribe, <<"Any">>),
    allow = emqx_access_control:check_acl(User1, publish, <<"Any/A">>),
    deny  = emqx_access_control:check_acl(User1, publish, <<"Any/C">>),
    allow = emqx_access_control:check_acl(User1, publish, <<"Topic/A/B">>),

    allow = emqx_access_control:check_acl(User2, subscribe, <<"Topic/C">>),
    allow = emqx_access_control:check_acl(User2, publish,   <<"Topic/D">>).

t_rest_api(_Config) ->
    clean_all_acls(),

    {ok, Result} = request_http_rest_list(),
    [] = get_http_data(Result),

    Params = #{<<"key">> => <<"test_username">>, <<"topic">> => <<"Topic/A">>, <<"action">> => <<"pubsub">>},
    {ok, _} = request_http_rest_add(Params),
    {ok, Result1} = request_http_rest_lookup(<<"test_username">>),
    #{<<"key">> := <<"test_username">>, <<"topic">> := <<"Topic/A">>, <<"action">> := <<"pubsub">>} = get_http_data(Result1),

    Params1 = [
                #{<<"key">> => <<"test_username">>, <<"topic">> => <<"+/A">>, <<"action">> => <<"pub">>},
                #{<<"key">> => <<"test_username_1">>, <<"topic">> => <<"#">>, <<"action">> => <<"sub">>},
                #{<<"key">> => <<"test_username_2">>, <<"topic">> => <<"+/A">>, <<"action">> => <<"error_format">>}
                ],
    {ok, Result2} = request_http_rest_add(Params1),
    #{
        <<"test_username">> := <<"ok">>,
        <<"test_username_1">> := <<"ok">>,
        <<"test_username_2">> := <<"{error,action}">>
        } = get_http_data(Result2),

    {ok, Result3} = request_http_rest_lookup(<<"test_username">>),
    [#{<<"key">> := <<"test_username">>, <<"topic">> := <<"+/A">>, <<"action">> := <<"pub">>},
     #{<<"key">> := <<"test_username">>, <<"topic">> := <<"Topic/A">>, <<"action">> := <<"pubsub">>}]
     = get_http_data(Result3),

    dbg:tracer(),
    dbg:p(all,c),
    dbg:tpl(emqx_auth_mnesia_cli, remove_acl, cx),
    dbg:tpl(emqx_auth_mnesia_cli, lookup_acl, cx),
    dbg:tpl(emqx_auth_mnesia_cli, all_acls, cx),

    {ok, _} = request_http_rest_delete(<<"test_username">>),
    {ok, _} = request_http_rest_delete(<<"test_username_1">>),
    {ok, Result4} = request_http_rest_list(),
    [] = get_http_data(Result4).

%%------------------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------------------

clean_all_acls() ->
    [ mnesia:dirty_delete({emqx_acl, Key})
      || Key <- mnesia:dirty_all_keys(emqx_acl)].

%%--------------------------------------------------------------------
%% HTTP Request
%%--------------------------------------------------------------------

request_http_rest_list() ->
    request_api(get, uri(), default_auth_header()).

request_http_rest_lookup(Login) ->
    request_api(get, uri([Login]), default_auth_header()).

request_http_rest_add(Params) ->
    request_api(post, uri(), [], default_auth_header(), Params).

request_http_rest_delete(Login) ->
    request_api(delete, uri([Login]), default_auth_header()).

uri() -> uri([]).
uri(Parts) when is_list(Parts) ->
    NParts = [b2l(E) || E <- Parts],
    ?HOST ++ filename:join([?BASE_PATH, ?API_VERSION, "emqx_acl"| NParts]).

%% @private
b2l(B) when is_binary(B) ->
    binary_to_list(B);
b2l(L) when is_list(L) ->
    L.