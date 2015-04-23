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
-module(conferl_message_repo).

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

-spec write(conferl_messages:message()) -> conferl_messages:message().
write(Message) -> sumo:persist(conferl_message, Message).

-spec write_top(integer()
                , string()
                , integer()
                , conferl_utils:datetime()) -> 
  conferl_messages:message().
write_top(ContentId, MessageText, User, CreateAt) ->
  TopLevelResponseId = undefined,
  Message = conferl_message:new(ContentId
                                , TopLevelResponseId
                                , MessageText
                                , User
                                , CreateAt),
  sumo:persist(conferl_message, Message).

-spec write_reply(integer()
                  , integer()
                  , string()
                  , integer()
                  , conferl_utils:datetime()) -> 
  conferl_messages:message().
write_reply(ContentId, ResponseId, MessageText, User, CreateAt) ->
  Message = conferl_message:new(ContentId
                                , ResponseId
                                , MessageText
                                , User
                                , CreateAt),
  sumo:persist(conferl_message, Message).

-spec delete(conferl_messages:message()) -> non_neg_integer().
delete(Message) -> 
  Id = conferl_message:id(Message),
  sumo:delete_by(conferl_message, [{id, Id}]).

 -spec delete_by_content_id(integer()) -> non_neg_integer().
delete_by_content_id(ContentId) -> 
  sumo:delete_by(conferl_message, [{content_id, ContentId}]).

-spec list(integer()) -> [conferl_messages:message()].
 list(ContentId) -> 
  sumo:find_by(conferl_message, [{content_id, ContentId}]).

-spec list_replies(integer()) -> [conferl_messages:message()].
list_replies(MessageResponseId) ->
  sumo:find_by(conferl_message, [{response_id, MessageResponseId}]).
  % Is equal to use   --------> [{response_id,'==', MessageResponseId}]).

-spec list_top_level(integer()) -> [conferl_messages:message()].
list_top_level(ContentId) -> 
  sumo:find_by(conferl_message, [ {content_id,  ContentId}
                                , {response_id, 'null'}
                                ]).

-spec list_by_user(integer()) -> [conferl_messages:message()].
list_by_user(UserId) -> sumo:find_by(conferl_message, [{user, UserId}]).

-spec delete_all() -> integer().
  delete_all() -> sumo:delete_all(conferl_message).

