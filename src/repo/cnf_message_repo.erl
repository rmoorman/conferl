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
-module(cnf_message_repo).

-author('David Cao <david.cao@inakanetworks.com>').

-export([write/1]).
-export([write_top/3]).
-export([write_reply/4]).
-export([delete/1]).
-export([delete_by_content_id/1]).
-export([list/1]).
-export([list_replies/1]).
-export([list_top_level/1]).
-export([list_by_user/1]).
-export([delete_all/0]).


-spec write(cnf_messages:message()) -> cnf_messages:message().
write(Message) -> sumo:persist(cnf_message, Message).

-spec write_top(integer()
                , string()
                , integer()) ->
  cnf_messages:message().
write_top(ContentId, MessageText, User) ->
  TopLevelResponseId = undefined,
  Message =
    cnf_message:new(ContentId
                   , TopLevelResponseId
                   , MessageText
                   , User),
  sumo:persist(cnf_message, Message).

-spec write_reply(integer()
                  , integer()
                  , string()
                  , integer()) ->
  cnf_messages:message().
write_reply(ContentId, ResponseId, MessageText, User) ->
  Message =
    cnf_message:new(ContentId
                   , ResponseId
                   , MessageText
                   , User),
  sumo:persist(cnf_message, Message).

-spec delete(pos_integer()) -> boolean().
delete(Id) ->
  sumo:delete(cnf_message, Id).

 -spec delete_by_content_id(pos_integer()) -> pos_integer().
delete_by_content_id(ContentId) ->
  sumo:delete_by(cnf_message, [{content_id, ContentId}]).

-spec list(integer()) -> [cnf_messages:message()].
 list(ContentId) ->
  sumo:find_by(cnf_message, [{content_id, ContentId}]).

-spec list_replies(pos_integer()) -> [cnf_messages:message()].
list_replies(MessageResponseId) ->
  sumo:find_by(cnf_message, [{response_id, MessageResponseId}]).
  % Is equal to use   --------> [{response_id,'==', MessageResponseId}]).

-spec list_top_level(pos_integer()) -> [cnf_messages:message()].
list_top_level(ContentId) ->
  sumo:find_by(cnf_message, [ {content_id,  ContentId}
                            , {response_id, null}
                            ]).

-spec list_by_user(pos_integer()) -> [cnf_messages:message()].
list_by_user(UserId) -> sumo:find_by(cnf_message, [{user, UserId}]).

-spec delete_all() -> integer().
  delete_all() -> sumo:delete_all(cnf_message).

