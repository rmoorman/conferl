% This file is licensed to you under the Apache License,
% Version 2.0 (the "License"); you may not use this file
% except in compliance with the License.  You may obtain
% a copy of the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing,
% software distributed under the License is distributed on an
% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
% KIND, either express or implied.  See the License for the
% specific language governing permissions and limitations
% under the License.

-module(cnf_content_id_handler_SUITE).

-author('David Cao <david.cao@inakanetworks.com>').

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).
-export([test_get_ok/1]).
-export([test_handle_delete_ok/1]).

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Suite tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%
-spec ignored_funs() -> [atom()].
ignored_funs() ->
  [ module_info
  , init_per_suite
  , end_per_testcase
  , end_per_suite
  ].

-spec all() -> [atom()].
all() ->
  [Fun || {Fun, 1} <- module_info(exports),
          not lists:member(Fun, ignored_funs())].

%% @doc definion of init_per_testcases

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  application:ensure_all_started(conferl),
  application:ensure_all_started(shotgun),
  sumo:create_schema(),
  cnf_user_repo:register_user("user", "password", "email@inaka.net"),
  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  sumo:delete_all(cnf_content),
  sumo:delete_all(cnf_user),
  Config.

%% @doc definion of init_per_testcases

init_per_testcase(_Function, Config) ->
  Config.

%% @doc definion of end_per_testcases

end_per_testcase(_Function, Config) ->
  Config.

-spec test_get_ok(config()) -> config().
test_get_ok(Config) ->
  User = cnf_user_repo:register_user("get_ok", "password", "mail@email.net"),
  Header = #{ <<"Content-Type">> => <<"application/json">>
            , basic_auth => {"get_ok", "password"}},
  Content =
    cnf_content_repo:register("http://inaka.net/get_ok", cnf_user:id(User)),
  Url = "/content/" ++  integer_to_list(cnf_content:id(Content)),
  {ok, Response} = cnf_test_utils:api_call(get, Url, Header),
  #{status_code := 200} = Response,
  Config.

-spec test_handle_delete_ok(config()) ->  config().
test_handle_delete_ok(Config) ->
  User = cnf_user_repo:register_user("delete_ok", "password", "mail@email.net"),
  Header = #{ <<"Content-Type">> => <<"application/json">>
            , basic_auth => {"delete_ok", "password"}},
  Content =
    cnf_content_repo:register("http://inaka.net/delete_ok", cnf_user:id(User)),
  Url = "/content/" ++  integer_to_list(cnf_content:id(Content)),
  {ok, Response} = cnf_test_utils:api_call(delete, Url, Header),
  #{status_code := 204} = Response,
  Config.
