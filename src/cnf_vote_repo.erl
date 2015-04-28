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
-module(cnf_vote_repo).

-author('David Cao <david.cao@inakanetworks.com>').
-export([ vote_message/2
        , unvote_message/2
        , remove_vote/1
        , fetch_vote/1
        , fetch_vote/2
        , list_votes/1
        , counts_votes_message/1
        , delete_all/0
        ]).

-type thumb_count() ::
        #{  up    => integer()
          , down  => integer()
         }.

-spec vote_message(integer(), integer()) -> cnf_vote:vote().
vote_message(UserId, MessageId) -> 
  Vote = cnf_vote:new(UserId, MessageId, up),
  sumo:persist(cnf_vote, Vote).

-spec unvote_message(integer(), integer()) -> cnf_vote:vote().
unvote_message(UserId, MessageId) -> 
  Vote = cnf_vote:new(UserId, MessageId, down),
  sumo:persist(cnf_vote, Vote).

-spec remove_vote(integer()) -> boolean().
remove_vote(VoteId) -> 
  sumo:delete(cnf_vote, VoteId).

-spec fetch_vote(integer(), integer()) -> notfound | cnf_vote:vote().
fetch_vote(MessageId, UserId) -> 
  Result = 
    sumo:find_by(cnf_vote, [{message_id, MessageId}, {user_id, UserId}]),
  case Result of
    []     -> notfound;
    [Vote] -> Vote
  end.

-spec fetch_vote(integer()) -> notfound | cnf_vote:vote().
fetch_vote(VoteId) -> 
  sumo:find(cnf_vote, VoteId).

-spec list_votes(integer()) -> [cnf_vote:vote()].
list_votes(MessageId) -> 
  sumo:find_by(cnf_vote, [{message_id, MessageId}]).

-spec inc_thumbs(cnf_vote:vote(), thumb_count()) -> 
  #{up | down => integer()}.
inc_thumbs(Vote, ThumbAccumulator) ->
  Thumb = cnf_vote:thumb(Vote),
  maps:update(Thumb, maps:get(Thumb, ThumbAccumulator) + 1, ThumbAccumulator).

-spec counts_votes_message(MessageId :: integer()) -> thumb_count().
counts_votes_message(MessageId) ->
  Votes = list_votes(MessageId),
  lists:foldl(fun inc_thumbs/2, #{up =>0, down => 0}, Votes).

-spec delete_all() -> non_neg_integer().
delete_all() -> 
  sumo:delete_all(cnf_vote).
