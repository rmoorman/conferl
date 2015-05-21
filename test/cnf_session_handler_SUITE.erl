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

-module(cnf_session_handler_SUITE).

-author('David Cao <david.cao@inakanetworks.com>').

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).
-export([post_session/1]).
-export([delete_session/1]).
-export([post_session_bad/1]).


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
  [F || {F, 1} <- module_info(exports), not lists:member(F, ignored_funs())].

%% @doc definion of init_per_testcases

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  application:ensure_all_started(sumo_db),
  application:ensure_all_started(uuid),
  application:ensure_all_started(lager),
  sumo:create_schema(),
  lager:start(),
  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  sumo:delete_all(cnf_session),
  sumo:delete_all(cnf_user),
  Config.

%% @doc definion of init_per_testcases
init_per_testcase(_Function, Config) ->
  Config.

%% @doc definion of end_per_testcases
end_per_testcase(_Function, Config) ->
  Config.

-spec post_session(config()) -> config().
post_session(Config) ->
  Name = "Doge post_session",
  Passsword = "passsword",
  Email = "email@email.net",
  cnf_user_repo:register_user(Name, Passsword, Email),
  Header = #{ <<"Content-Type">> => <<"application/json">>
            , basic_auth => {Name, Passsword}},
  Body = #{},
  JsonBody = jiffy:encode(Body),
  {ok, Response} =
    cnf_test_utils:api_call(post, "/sessions", Header, JsonBody),
  #{status_code := 200} = Response,
  #{body := JsonResponseBody} = Response,
  BodyResp = jiffy:decode(JsonResponseBody, [return_maps]),
  #{<<"token">> := _Token} = BodyResp,
  Config.

-spec post_session_bad(config()) -> config().
post_session_bad(Config) ->
  Name = "No registered Doge",
  Passsword = "passsword",
  Header = #{ <<"Content-Type">> => <<"application/json">>
            , basic_auth => {Name, Passsword}},
  Body = #{},
  JsonBody = cnf_session:to_json(Body),
  {ok, Response} =
    cnf_test_utils:api_call(post, "/sessions", Header, JsonBody),
  #{status_code := 401} = Response,
  Config.

-spec delete_session(config()) -> config().
delete_session(Config) ->
  Name = "Doge delete_session",
  Passsword = "passsword",
  Email = "email@email.net",
  RegistedUser = cnf_user_repo:register_user(Name, Passsword, Email),
  Session = cnf_session_repo:register(cnf_user:id(RegistedUser)),
  Token = cnf_session:token(Session),
  Header = #{ <<"Content-Type">> => <<"application/json">>
            , basic_auth => {Name, Passsword}},
  Body = #{},
  JsonBody = cnf_session:to_json(Body),
  {ok, Response} =
    cnf_test_utils:api_call(delete, "/sessions/" ++ Token, Header, JsonBody),
  #{status_code := 204} = Response,
  [] = cnf_session_repo:find_by_user(cnf_session:user_id(Session)),
  Config.
