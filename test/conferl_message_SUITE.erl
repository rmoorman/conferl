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

-module(conferl_message_SUITE).

-author('David Cao <david.cao@inakanetworks.com>').

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , top_message_create/1
        , test_delete_by_content/1
        , test_list_top_message/1
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

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  application:ensure_all_started(sumo_db),
  sumo:create_schema(),
  conferl_message_repo:delete_all(),
  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  Config.

%% @doc definion of init_per_testcases
init_per_testcase(top_message_create, _Config) -> 
  TopMesages = 
    lists:map(fun(N) -> conferl_message:new(1, undefined, "Top m", N) end
              , lists:seq(1, 10)),
  [ {top_messages, TopMesages}
  , {top_messages_content_id, 1} 
  ];
  init_per_testcase(test_delete_by_id_content, _Config) -> 
  TopMesages = 
    lists:map(fun(N) -> conferl_message:new(1, undefined, "Top m", N) end
              , lists:seq(1, 10)),
  [ {top_messages, TopMesages}
  , {top_messages_content_id, 1} 
  ];
  init_per_testcase(test_list_top_message, _Config) -> 
  TopMesages = 
    lists:map(fun(N) -> conferl_message:new(1, undefined, "Top m", N) end
              , lists:seq(1, 10)),
  [ {top_messages, TopMesages}
  , {top_messages_content_id, 1} 
  ];

  init_per_testcase(_Function, _Config) -> 
  TopMesages = 
    lists:map(fun(N) -> conferl_message:new(1, undefined, "Top m", N) end
             , lists:seq(1, 10)),
  [ {top_messages, TopMesages}
  , {top_messages_content_id, 1} 
  ].

-spec top_message_create(config()) -> ok.
top_message_create(Config) ->
  TopMesages = proplists:get_value(top_messages, Config),
  [conferl_message_repo:write_message(Message) || Message <- TopMesages],
  ContentId = proplists:get_value(top_messages_content_id, Config),
  PersistedTopMessage = conferl_message_repo:list_messages(ContentId),
  true = lists:all(fun conferl_message:is_top_message/1 , PersistedTopMessage),
  ok.

-spec test_delete_by_content(config()) -> ok.
test_delete_by_content(Config) ->
  conferl_message_repo:delete_all(),
  top_message_create(Config),
  TopMesages = proplists:get_value(top_messages, Config),
  Lenght     = length(TopMesages),
  ContentId  = proplists:get_value(top_messages_content_id, Config),
  Lenght     = conferl_message_repo:delete_by_content_id(ContentId),
  ok.

-spec test_list_top_message(config()) -> ok.
test_list_top_message(Config) ->
  conferl_message_repo:delete_all(),
  top_message_create(Config),
  TopMesages = proplists:get_value(top_messages, Config),
  [ conferl_message_repo:write_message(Message) || Message <- TopMesages],
  ContentId = proplists:get_value(top_messages_content_id, Config),
  PersistedTopM = conferl_message_repo:list_top_level_messages(ContentId),
  true = lists:all(fun conferl_message:is_top_message/1 , PersistedTopM),
  ok.





















