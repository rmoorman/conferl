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

-module(cnf_vote_SUITE).

-author('David Cao <david.cao@inakanetworks.com>').

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        , test_upvote/1
        , test_downvote/1
        , test_fetch_vote/1
        , test_counts_votes_message/1
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
  sumo:delete_all(cnf_vote),
  Config.

%% @doc definion of init_per_testcases
init_per_testcase(_Function, Config) ->
  Config.

%% @doc definion of end_per_testcases
end_per_testcase(_Function, Config) ->
  Config.

-spec test_upvote(config()) -> config().
test_upvote(Config) ->
  UserId = 1,
  MessageId = 1,
  VotedMessage = cnf_vote_repo:upvote(UserId, MessageId),
  UserId = cnf_vote:user_id(VotedMessage),
  MessageId = cnf_vote:user_id(VotedMessage),
  up = cnf_vote:thumb(VotedMessage),
  Config.

-spec test_downvote(config()) -> config().
test_downvote(Config) ->
  UserId = 2,
  MessageId = 2,
  VotedMessage = cnf_vote_repo:downvote(UserId, MessageId),
  UserId = cnf_vote:user_id(VotedMessage),
  MessageId = cnf_vote:user_id(VotedMessage),
  down = cnf_vote:thumb(VotedMessage),
  Config.

-spec test_fetch_vote(config()) -> config().
test_fetch_vote(Config) ->
  UserId = 3,
  MessageId = 3,
  VotedMessage = cnf_vote_repo:upvote(UserId, MessageId),
  VotedMesId = cnf_vote:id(VotedMessage),
  VotedFetched1 = cnf_vote_repo:fetch_vote(VotedMesId),
  VotedFetched2 = cnf_vote_repo:fetch_vote(UserId, MessageId),
  VotedFetched1 = VotedFetched2,
  Config.

-spec test_counts_votes_message(config()) -> config().
test_counts_votes_message(Config)  ->
  MessageId = 4,
  CountVotesUp = 15,
  CountVotesDown = 30,
  [cnf_vote_repo:upvote(UserId, MessageId)
    || UserId <- lists:seq(1, CountVotesUp)],
  [cnf_vote_repo:downvote(UserId, MessageId)
    || UserId <- lists:seq(1, CountVotesDown)],
  #{up := CountVotesUp, down := CountVotesDown} =
     cnf_vote_repo:counts_votes_message(MessageId),
  Config.
