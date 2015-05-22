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

-module(cnf_user_handler_SUITE).

-author('David Cao <david.cao@inakanetworks.com>').

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).
-export([delete_user/1]).
-export([get_user/1]).
-export([post_session/1]).

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
init_per_suite(_Config) ->
  application:ensure_all_started(sumo_db),
  application:ensure_all_started(shotgun),
  sumo:create_schema(),
  lager:start(),
  [].

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  sumo:delete_all(cnf_user),
  Config.

%% @doc definion of init_per_testcases
init_per_testcase(_Function, Config) ->
  Config.

%% @doc definion of end_per_testcases
end_per_testcase(_Function, Config) ->
  Config.

-spec delete_user(config()) -> config().
delete_user(Config) ->
  Name = "Doge delete_user",
  Passsword = "Such Password, Many mod_security",
  Email = "email@email.net",
  RegistedUser = cnf_user_repo:register_user(Name, Passsword, Email),
   lager:error("delete_user  RegistedUser ~p", [RegistedUser]),
  Header = #{ <<"Content-Type">> => <<"application/json">>
            , basic_auth => {Name, Passsword}},
  Body = #{},
  JsonBody = jiffy:encode(Body),
  {ok, Response} =
    cnf_test_utils:api_call(delete, "/me", Header, JsonBody),
  #{status_code := 204} = Response,
  notfound = cnf_user_repo:find(cnf_user:id(RegistedUser)),
  Config.

-spec get_user(config()) -> config().
get_user(Config) ->
  Name = "Doge get_user",
  Passsword = "Such Password, Many security",
  Email = "email@email.net",
  RegistedUser = cnf_user_repo:register_user(Name, Passsword, Email),
  lager:error("get_user  RegistedUser ~p", [RegistedUser]),
  Header = #{ <<"Content-Type">> => <<"application/json">>
            , basic_auth => {Name, Passsword}},
  Url = "/users/" ++  integer_to_list(cnf_user:id(RegistedUser)),
  {ok, Response} = cnf_test_utils:api_call(get, Url, Header),
  #{status_code := 200} = Response,
  Config.


-spec post_session(config()) -> config().
post_session(Config) ->
  Name = "Doge post_session",
  Passsword = "Such Password, Many security!",
  Email = "email@email.net",
  Header = #{ <<"Content-Type">> => <<"application/json">>
            , basic_auth => {Name, Passsword}},
  Body = #{},
  JsonBody = jiffy:encode(Body),
  %%%{ok, Response} =
  {ok, Response}  =
    cnf_test_utils:api_call(post, "/users", Header, JsonBody),
    lager:error("post_session Response!!  ~p", [Response]),
    #{status_code := 200} = Response,
     %lager:error("post_session  Respuestam ~p", [Respuesta]),
  %#{status_code := 200} = Response,
  %#{body := JsonResponseBody} = Response,
  %BodyResp = jiffy:decode(JsonResponseBody, [return_maps]),
 % #{<<"token">> := _Token} = BodyResp,
  Config.
