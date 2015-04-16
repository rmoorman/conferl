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

-module(conferl_content_SUITE).

-author('David Cao <david.cao@inakanetworks.com>').

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , test_create_user/1
        , test_create_user_bad/1 
        , double_registration_bad/1
        , fetch_notfound_content/1
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
  , end_per_suite
  ].

-spec all() -> [atom()].
all() -> 
  [Fun || {Fun, 1} <- module_info(exports), 
          not lists:member(Fun, ignored_funs())].

%% @doc definion of init_per_testcases
%init_per_testcase(well_formed_Url, _Config) -> 
%  [{url, "http://inaka.net/"} ];
%init_per_testcase(mal_formed_Url, _Config)  -> 
%  [{url, "qweqwettyuiuy"} ].


%% @doc definion of end_per_testcase
%end_per_testcase(_ , Config) ->  Config.

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  application:ensure_all_started(sumo_db),
  sumo:create_schema(),
  conferl_content_repo:delete_all(),
  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  Config.

%% @doc definion of init_per_testcases
init_per_testcase(test_create_user, _Config) -> 
  [{url, "http://inaka.net/"}
  ,{user, 10}
  ];

init_per_testcase(test_create_user_bad, _Config) -> 
  [{url, "bad_url!!!!!"}
  ,{user, 10}
  ];

init_per_testcase(double_registration_bad, _Config) -> 
  [{url, "http://inaka.net/"}
  ,{user, 10}
  ];
init_per_testcase(fetch_notfound_content, _Config)  -> 
  [{id, 999999}];
init_per_testcase(_, Config)  -> 
  Config.


%% @doc tests for register_content
-spec test_create_user(config()) -> ok.
test_create_user(Config) ->
  Url     = proplists:get_value(url, Config),
  User    = proplists:get_value(user, Config),
  conferl_content:new(Url, User).

-spec test_create_user_bad(config()) -> ok.
test_create_user_bad(Config) ->
  Url     = proplists:get_value(url, Config),
  User    = proplists:get_value(user, Config),
  try conferl_content:new(Url, User) of
    _Content -> ct:fail("Resultado NO esperado")
  catch 
    throw:invalid_url -> ok
  end.  

-spec double_registration_bad(config()) -> ok. 
double_registration_bad(Config) ->
  Url     = proplists:get_value(url, Config),
  User    = proplists:get_value(user, Config),
  conferl_content_repo:register_content(Url, User),
  try conferl_content_repo:register_content(Url, User) of
    _Content -> ct:fail("Resultado NO esperado") 
  catch
    Error:Reason -> ct:pal("~p ~p", [Error, Reason]);
    _when_others            -> ct:fail("Resultado NO esperado") 
  end.

%% @doc tests for fetch_content
-spec fetch_notfound_content(config()) -> ok.
fetch_notfound_content(Config) -> 
  ContentId   = proplists:get_value(id, Config),
  try conferl_content_repo:fetch_content(ContentId) of
    _Content -> ct:fail("")
  catch 
    throw:notfound  -> ok;
    _               -> error
  end.
 

%% @doc tests for list_contents
%% list_contents(Domain) -> [ #{} ].
%% todo





