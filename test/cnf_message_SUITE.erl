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

-module(cnf_message_SUITE).

-author('David Cao <david.cao@inakanetworks.com>').

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).
-export([top_message_create/1]).
-export([test_delete_by_content/1]).
-export([message_replys/1]).
-export([ test_list_top_message/1]).

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
  application:ensure_all_started(sumo_db),
  sumo:create_schema(),
  get_top_messages_conf().

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  sumo:delete_all(cnf_message),
  Config.

%% @doc definion of init_per_testcases

init_per_testcase(_Function, Config) ->
  Config.

%% @doc definion of end_per_testcases

end_per_testcase(_Function, Config) ->
  cnf_message_repo:delete_all(),
  Config.

-spec top_message_create(config()) -> ok.
top_message_create(Config) ->
  TopMessages = proplists:get_value(top_messages, Config),
  [cnf_message_repo:write_top(C, M, U)
    || {C, M, U} <- TopMessages],
  ContentId = proplists:get_value(top_messages_content_id, Config),
  PersistedTopMessage = cnf_message_repo:find_by_content(ContentId),
  true = all_are_top(PersistedTopMessage),
  ok.

-spec test_delete_by_content(config()) -> ok.
test_delete_by_content(Config) ->
  TopMessages = proplists:get_value(top_messages, Config),
  Lenght = length(TopMessages),
  ContentId = proplists:get_value(top_messages_content_id, Config),
  [cnf_message_repo:write_top(C, M, U)
    || {C, M, U} <- TopMessages],
  Lenght = cnf_message_repo:delete_by_content_id(ContentId),
  ok.

-spec test_list_top_message(config()) -> ok.
test_list_top_message(Config) ->
  TopMessages = proplists:get_value(top_messages, Config),
  [cnf_message_repo:write_top(C, M, U)
    || {C, M, U} <- TopMessages],
  ContentId = proplists:get_value(top_messages_content_id, Config),
  PersistedTopM = cnf_message_repo:list_top_level(ContentId),
  true = all_are_top(PersistedTopM),
  ok.

-spec message_replys(config()) -> ok.
message_replys(Config) ->
  TopM = proplists:get_value(top_messages, Config),
  ReplyM = proplists:get_value(reply_message, Config),
  PersistedTopMessage = [cnf_message_repo:write_top(C, M, U)
                          || {C, M, U} <- TopM],
  [cnf_message_repo:write_reply(C, cnf_message:id(P), M, U)
    || P <- PersistedTopMessage , {C, _R, M, U} <- ReplyM],
  MessageListId = [cnf_message:id(P) || P <- PersistedTopMessage],
  PersistedReplyM = lists:flatten([cnf_message_repo:list_replies(Id)
                                    || Id <- MessageListId]),
  true = all_are_reply(PersistedReplyM),
  Length = length(PersistedReplyM),
  Length = length(TopM) * length(ReplyM),
  ok.

-spec all_are_top([cnf_messages:message()]) -> boolean().
all_are_top(List) ->
  lists:all(fun cnf_message:is_top_message/1, List).

-spec all_are_reply([cnf_messages:message()])-> boolean().
all_are_reply(List) ->
  not lists:all(fun cnf_message:is_top_message/1, List).

-spec get_top_messages_conf() -> config().
get_top_messages_conf() ->
  GenerateTopM = fun(N) -> { 1
                            , "Wow! Top message."
                            , N
                           }
                  end,
  TopMessages  = lists:map(GenerateTopM, lists:seq(1, 10)),
  GenerateRplM = fun(N) -> { 1
                            , undefined
                            , "Such message, very reply."
                            , N
                          }
                  end,
  ReplyMessage = lists:map(GenerateRplM, lists:seq(1, 10)),
  [ {top_messages, TopMessages}
  , {reply_message, ReplyMessage}
  , {doge_top_text, "Wow! Top message."}
  , {doge_reply_text, "Such message, very reply."}
  , {top_messages_content_id, 1}
  ].
