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

-module(cnf_user_SUITE).

-author('David Cao <david.cao@inakanetworks.com>').

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        , create_user/1
        , fetch_user/1
        , duplicate_user/1
        , unregistrate_user/1
        ]).

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
  sumo:create_schema(),
  lager:start(),
  [].

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  cnf_user_repo:delete_all(),
  Config.

%% @doc definion of init_per_testcases
init_per_testcase(_Function, Config) -> 
  Config.

%% @doc definion of end_per_testcases
end_per_testcase(_Function, Config) -> 
  cnf_user_repo:delete_all(),
  Config.

-spec create_user(config()) -> ok.
create_user(_Config) -> 
  Name = <<"Doge create_user">>,
  Passsword = <<"passsword">>,
  Email = <<"email">>,
  RegistedUser = cnf_user_repo:register_user(Name, Passsword, Email),
  Name = cnf_user:user_name(RegistedUser),
  Passsword = cnf_user:password(RegistedUser),
  Email = cnf_user:email(RegistedUser),
  ok.

-spec fetch_user(config()) -> ok.
fetch_user(_Config) -> 
  Name = <<"Doge fetch_user">>,
  Passsword = <<"passsword">>,
  Email = <<"email">>,
  RegistedUser = cnf_user_repo:register_user(Name, Passsword, Email),
  Id = cnf_user:id(RegistedUser),
  lager:warning("Id:  ~p", [Id]),
  PersistedUser = cnf_user_repo:fetch_user(Id),
  lager:warning("PersistedUser:  ~p", [PersistedUser]),
  Name = cnf_user:user_name(PersistedUser),
  Passsword = cnf_user:password(PersistedUser),
  Email = cnf_user:email(PersistedUser),
  ok.

-spec duplicate_user(config()) -> ok.
duplicate_user(_Config) -> 
  Name = <<"Doge duplicate_user">>,
  Passsword = <<"passsword">>,
  Email = <<"email">>,
  RegistedUser = cnf_user_repo:register_user(Name, Passsword, Email),
  try cnf_user_repo:register_user(Name, Passsword, Email) of
    _ -> ct:fail("Unexpected result (!)")
  catch
    throw:duplicate_user -> ok
  end.

-spec unregistrate_user(config()) -> ok.
unregistrate_user(_Config) -> 
  Name = <<"Doge unregistrate_user">>,
  Passsword = <<"passsword">>,
  Email = <<"email">>,
  RegistedUser = cnf_user_repo:register_user(Name, Passsword, Email),
  cnf_user_repo:unregister_user(Name),
  try cnf_user_repo:register_user(Name, Passsword, Email) of
    _ -> ok
  catch
    throw:duplicate_user -> ct:fail("Unexpected result (!)")
  end.















