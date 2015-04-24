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

-export([ write/1
        , write_top/4
        , write_reply/5
        , delete/1
        , delete_by_content_id/1
        , list/1
        , list_replies/1
        , list_top_level/1
        , list_by_user/1
        , delete_all/0
        ]).

-spec write(cnf_messages:message()) -> cnf_messages:message().
write(Message) -> sumo:persist(cnf_message, Message).

-spec write_top(integer()
                , string()
                , integer()
                , conferl_utils:datetime()) -> 
  cnf_messages:message().
write_top(ContentId, MessageText, User, CreatedAt) ->
  TopLevelResponseId = undefined,
  Message = cnf_message:new(ContentId
                                , TopLevelResponseId
                                , MessageText
                                , User
                                , CreatedAt),
  sumo:persist(cnf_message, Message).

-spec write_reply(integer()
                  , integer()
                  , string()
                  , integer()
                  , conferl_utils:datetime()) -> 
  cnf_messages:message().
write_reply(ContentId, ResponseId, MessageText, User, CreatedAt) ->
  Message = cnf_message:new(ContentId
                                , ResponseId
                                , MessageText
                                , User
                                , CreatedAt),
  sumo:persist(cnf_message, Message).

-spec delete(cnf_messages:message()) -> non_neg_integer().
delete(Message) -> 
  Id = cnf_message:id(Message),
  sumo:delete_by(cnf_message, [{id, Id}]).

 -spec delete_by_content_id(integer()) -> non_neg_integer().
delete_by_content_id(ContentId) -> 
  sumo:delete_by(cnf_message, [{content_id, ContentId}]).

-spec list(integer()) -> [cnf_messages:message()].
 list(ContentId) -> 
  sumo:find_by(cnf_message, [{content_id, ContentId}]).

-spec list_replies(integer()) -> [cnf_messages:message()].
list_replies(MessageResponseId) ->
  sumo:find_by(cnf_message, [{response_id, MessageResponseId}]).
  % Is equal to use   --------> [{response_id,'==', MessageResponseId}]).

-spec list_top_level(integer()) -> [cnf_messages:message()].
list_top_level(ContentId) -> 
  sumo:find_by(cnf_message, [ {content_id,  ContentId}
                                , {response_id, null}
                                ]).

-spec list_by_user(integer()) -> [cnf_messages:message()].
list_by_user(UserId) -> sumo:find_by(cnf_message, [{user, UserId}]).

-spec delete_all() -> integer().
  delete_all() -> sumo:delete_all(cnf_message).

