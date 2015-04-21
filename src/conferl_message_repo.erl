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

-export([ write_message/1
				, delete_message/1
				, delete_by_content_id/1
				, list_messages/1
				, list_replies/1
				, list_top_level_messages/1
				, list_user_messages/1
				, delete_all/0
				]).


-spec write_message(conferl_messages:message()) -> conferl_messages:message().
write_message(Message) -> sumo:persist(conferl_message, Message).

-spec delete_message(conferl_messages:message()) -> non_neg_integer().
delete_message(Message) -> 
  Id = conferl_message:id(Message),
  sumo:delete_by(conferl_message, [{id, Id}]).

 -spec delete_by_content_id(integer()) -> non_neg_integer().
delete_by_content_id(ContentId) -> 
  sumo:delete_by(conferl_message, [{content_id, ContentId}]).

-spec list_messages(integer()) -> [conferl_messages:message()].
 list_messages(ContentId) -> 
 	sumo:find_by(conferl_message, [{content_id,ContentId}]).

-spec list_replies(integer()) -> [conferl_messages:message()].
list_replies(MessageResponseId) ->
 	sumo:find_by(conferl_message, [{response_id, MessageResponseId}]).
 	% sumo:find_by(conferl_message, [{id_response,'==', MessageResponseId}]).

-spec list_top_level_messages(integer()) -> [conferl_messages:message()].
list_top_level_messages(ContentId) -> 
	sumo:find_by(conferl_message, [ {content_id,  ContentId}
                                , {response_id, 'null'}
                                ]).

-spec list_user_messages(integer()) -> [conferl_messages:message()].
list_user_messages(UserId) -> sumo:find_by(conferl_message, [{user, UserId}]).

-spec delete_all() -> integer().
  delete_all()  -> sumo:delete_all(conferl_message).

