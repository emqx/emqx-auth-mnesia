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
all() ->
    emqx_ct:all(?MODULE).

groups() ->
    [].

init_per_suite(Config) ->
    emqx_ct_helpers:start_apps([emqx_auth_mnesia], fun set_special_configs/1),
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

    ok = emqx_auth_mnesia:add_user(<<"test_username">>, <<"password">>, true),
    {error, existed} = emqx_auth_mnesia:add_user(<<"test_username">>, <<"password">>, true),

    ok = emqx_auth_mnesia:update_user(<<"test_username">>, <<"new_password">>, false),
    {error,noexisted} = emqx_auth_mnesia:update_user(<<"no_existed_user">>, <<"password">>, true),

    [<<"test_username">>] = emqx_auth_mnesia:all_users(),
    [{emqx_user, <<"test_username">>, _HashedPass, false}] =
        emqx_auth_mnesia:lookup_user(<<"test_username">>),

    User1 = #{username => <<"test_username">>,
              password => <<"new_password">>,
              zone     => external},

    {ok, #{is_superuser := false, 
           auth_result := success,
           anonymous := false}} = emqx_access_control:authenticate(User1),

    {error,password_error} = emqx_access_control:authenticate(User1#{password => <<"error_password">>}),

    ok = emqx_auth_mnesia:remove_user(<<"test_username">>),
    {ok, #{auth_result := success,
           anonymous := true }} = emqx_access_control:authenticate(User1).

t_check_as_clientid(_Config) ->
    clean_all_users(),
    application:set_env(emqx_auth_mnesia, as, clientid),

    ok = emqx_auth_mnesia:add_user(<<"test_clientid">>, <<"password">>, false),
    {error, existed} = emqx_auth_mnesia:add_user(<<"test_clientid">>, <<"password">>, false),

    ok = emqx_auth_mnesia:update_user(<<"test_clientid">>, <<"new_password">>, true),
    {error,noexisted} = emqx_auth_mnesia:update_user(<<"no_existed_user">>, <<"password">>, true),

    [<<"test_clientid">>] = emqx_auth_mnesia:all_users(),
    [{emqx_user, <<"test_clientid">>, _HashedPass, true}] =
        emqx_auth_mnesia:lookup_user(<<"test_clientid">>),

    User1 = #{clientid => <<"test_clientid">>,
              password => <<"new_password">>,
              zone     => external},

    {ok, #{is_superuser := true, 
           auth_result := success,
           anonymous := false}} = emqx_access_control:authenticate(User1),

    {error,password_error} = emqx_access_control:authenticate(User1#{password => <<"error_password">>}),

    ok = emqx_auth_mnesia:remove_user(<<"test_clientid">>),
    {ok, #{auth_result := success,
           anonymous := true }} = emqx_access_control:authenticate(User1).

%%------------------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------------------

clean_all_users() ->
    [ mnesia:dirty_delete({emqx_user, Username})
      || Username <- mnesia:dirty_all_keys(emqx_user)].