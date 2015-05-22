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

-module(cnf_session_SUITE).

-author('David Cao <david.cao@inakanetworks.com>').

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).
-export([create_session/1]).
-export([delete_session/1]).
-export([test_find_by_token/1]).

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
init_per_suite(_Config) ->
  application:ensure_all_started(sumo_db),
  application:ensure_all_started(uuid),
  application:ensure_all_started(lager),
  sumo:create_schema(),
  lager:start(),
  [].

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

-spec create_session(config()) ->  config().
create_session(Config) ->
  Name = <<"Doge create_session">>,
  Passsword = <<"passsword">>,
  Email = <<"email">>,
  RegistedUser = cnf_user_repo:register_user(Name, Passsword, Email),
  Session = cnf_session_repo:register(cnf_user:id(RegistedUser)),
  #{user_id := _Id
   , token := _Token
   , created_at := _Created
   , updated_at := _updated } = Session,
   Config.

-spec delete_session(config()) ->  config().
delete_session(Config) ->
  Name = <<"Doge delete_session">>,
  Passsword = <<"passsword">>,
  Email = <<"email">>,
  RegistedUser = cnf_user_repo:register_user(Name, Passsword, Email),
  Session = cnf_session_repo:register(cnf_user:id(RegistedUser)),
  1  = cnf_session_repo:unregister(cnf_session:token(Session)),
  [] = cnf_session_repo:find_by_user(cnf_session:user_id(Session)),
  Config.

-spec test_find_by_token(config()) ->  config().
test_find_by_token(Config) ->
  Name1 = <<"Doge 1">>,
  Name2 = <<"Doge 2">>,
  Name3 = <<"Doge 3">>,
  Passsword = <<"passsword">>,
  Email = <<"email">>,
  RegistedUser1 = cnf_user_repo:register_user(Name1, Passsword, Email),
  RegistedUser2 = cnf_user_repo:register_user(Name2, Passsword, Email),
  RegistedUser3 = cnf_user_repo:register_user(Name3, Passsword, Email),
  Session1 = cnf_session_repo:register(cnf_user:id(RegistedUser1)),
  Session2 = cnf_session_repo:register(cnf_user:id(RegistedUser2)),
  Session3 = cnf_session_repo:register(cnf_user:id(RegistedUser3)),
  cnf_session_repo:find_by_token(cnf_session:token(Session1)),
  cnf_session_repo:find_by_token(cnf_session:token(Session2)),
  cnf_session_repo:find_by_token(cnf_session:token(Session3)),
  Config.
