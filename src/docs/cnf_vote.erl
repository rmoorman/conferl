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

-module(cnf_vote).
-author('david.cao@inakanetworks.com').

-type thumb() :: up | down.

-opaque vote() ::
        #{  id         => integer()
          , user_id    => integer()
          , message_id => integer()
          , thumb      => thumb()
          , created_at => tuple()
          , updated_at => tuple()
         }.

-export_type([vote/0]).

-export([new/3]).
-export([id/1]).
-export([user_id/1]).
-export([user_id/2]).
-export([message_id/1]).
-export([message_id/2]).
-export([thumb/1]).
-export([thumb/2]).
-export([created_at/1]).
-export([created_at/2]).
-export([updated_at/1]).
-export([updated_at/2]).
-export([to_json/1]).

%%% sumo_db callbacks
-export([sumo_schema/0]).
-export([sumo_wakeup/1]).
-export([sumo_sleep/1]).

-behavior(sumo_doc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BEHAVIOUR CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Part of the sumo_doc behavior.
%%

-spec sumo_wakeup(sumo:doc()) -> vote().
sumo_wakeup(Data) -> thumb_wakeup(Data).

%% @doc Part of the sumo_doc behavior.
-spec sumo_sleep(vote()) -> sumo:doc().
sumo_sleep(Vote) -> thumb_sleep(Vote).

%% @doc Part of the sumo_doc behavior.
-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(?MODULE, [
    sumo:new_field(id        , integer , [id, auto_increment, not_null]),
    sumo:new_field(user_id   , integer , [not_null]),
    sumo:new_field(message_id, integer , [not_null]),
    sumo:new_field(thumb     , integer , [not_null]),
    sumo:new_field(created_at, datetime, [not_null]),
    sumo:new_field(updated_at, datetime, [not_null])
  ]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public Api
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc functions definitions for message
-spec new( integer()
         , integer()
         , thumb()) -> vote().
new(UserId, MessageId, Thumb) ->
  Now = calendar:universal_time(),
  #{ id         => undefined
   , message_id => MessageId
   , user_id    => UserId
   , thumb      => Thumb
   , created_at => Now
   , updated_at => Now
   }.

-spec id(vote()) -> integer().
id(Vote) ->
  maps:get(id, Vote).

-spec user_id(vote()) -> integer().
user_id(Vote) ->
  maps:get(user_id, Vote).

-spec user_id(vote(), integer()) -> vote().
user_id(Vote, UserId) ->
  Vote#{user_id => UserId}.

-spec message_id(vote()) -> integer().
message_id(Vote) ->
  maps:get(message_id, Vote).

-spec message_id(vote(), integer()) -> vote().
message_id(Vote, UserId) ->
  Vote#{message_id => UserId}.

-spec thumb(vote()) -> thumb().
thumb(Vote) ->
  maps:get(thumb, Vote).

-spec thumb(vote(), thumb()) -> vote().
thumb(Vote, Thumb) ->
  Vote#{thumb => Thumb}.

-spec created_at(vote()) -> tuple().
created_at(Vote) ->
  maps:get(created_at, Vote).

-spec created_at(vote(), tuple()) -> vote().
created_at(Vote, CreatedAt) ->
  Vote#{ reated_at => CreatedAt}.

-spec updated_at(vote()) -> tuple().
updated_at(Vote) ->
  maps:get(updated_at, Vote).

-spec updated_at(vote(), tuple()) -> vote().
updated_at(Vote, UpdatedAt) ->
  Vote#{updated_at => UpdatedAt}.

-spec thumb_wakeup(sumo:doc()) -> vote().
thumb_wakeup(Vote = #{thumb := 1}) ->
  Vote#{thumb => up};
thumb_wakeup(Vote = #{thumb := 0}) ->
  Vote#{thumb => down}.

-spec thumb_sleep(vote()) -> sumo:doc().
thumb_sleep(Vote = #{thumb := up}) ->
  Vote#{thumb => 1};
thumb_sleep(Vote = #{thumb := down}) ->
  Vote#{thumb => 0}.

-spec to_json(vote() | [vote()]) -> vote() | [vote()].
to_json(ListVotes) when is_map(ListVotes) ->
  jiffy:encode(doc_to_binary_date(ListVotes));
to_json(Vote) when is_list(Vote) ->
  JsonListVotes = lists:map(fun doc_to_binary_date/1, Vote),
  jiffy:encode(JsonListVotes).

-spec doc_to_binary_date(map()) -> map().
doc_to_binary_date(Vote) ->
  CreatedAtBinary = cnf_utils:datetime_to_binary(created_at(Vote)),
  UpdatedAtBinary = cnf_utils:datetime_to_binary(updated_at(Vote)),
  Vote#{created_at => CreatedAtBinary, updated_at => UpdatedAtBinary}.
