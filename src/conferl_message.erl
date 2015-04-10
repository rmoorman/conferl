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

-module(conferl_message).

-author('david.cao@inakanetworks.com').

-type message() ::
        #{
           id      => integer(),
           message => string(),
           user    => string()
           %date
         }.


-export_type( [message/0]).

-export(sumo_doc).


-spec write_message(ContentId :: integer()
                    , UserId :: integer()
                    , Text :: iodata()) -> conferl_messages:message().

-spec write_message(ContentId :: integer()
                    , UserId :: integer()
                    , ReplyToMsgId :: integer()
                    , Text :: iodata()) -> conferl_messages:message().

-spec fetch_message(MessageId :: integer()) ->
  notfound | conferl_messages:message().

-spec  list_messages(ContentId :: integer()) -> [conferl_messages:message()].

-spec list_replies(MessageId :: integer()) -> [conferl_messages:message()].

-spec list_top_level_messages(ContentId :: integer()) -> 
  [conferl_messages:message()].
  
-spec list_user_messages(UserId :: integer()) -> [conferl_messages:message()].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BEHAVIOUR CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Part of the sumo_doc behavior.
%%

-spec sumo_wakeup(sumo:doc()) -> message().
sumo_wakeup(Data) -> Data.

%% @doc Part of the sumo_doc behavior.
-spec sumo_sleep(message()) -> sumo:doc().
sumo_sleep(Message) -> Message.

%% @doc Part of the sumo_doc behavior.
-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(?MODULE, [
    sumo:new_field(id, integer,     [not_null, auto_increment, id]),
    sumo:new_field(message, string, [{length, 256}, not_null]),
    sumo:new_field(user, string,    [{length, 128}, not_null])
  ]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%