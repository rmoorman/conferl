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

-author('David Cao <david.cao@inakanetworks.com>').

-type message() ::
  #{  id           => integer()
    , content_id   => integer()
    , response_id  => integer()
    , message_text => string()
    , user         => integer()
     %date
    }.

-export_type( [message/0]).

%%% sumo_db callbacks
-export([ sumo_schema/0
        , sumo_wakeup/1
        , sumo_sleep/1
        ]).

-export([ new/4
        , id/1
        , content_id/1
        , response_id/1
        , message_text/1
        , message_text/2
        , is_top_message/1
        ]).

-behavior(sumo_doc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BEHAVIOUR CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Part of the sumo_doc behavior.
%%

%%% sumo_db callbacks
-spec replace_null(sumo:doc()) -> message().
replace_null(Message = #{response_id := null}) -> 
  Message#{response_id => undefined};  
replace_null(Message) ->  
  Message.

-spec sumo_wakeup(sumo:doc()) -> message().
sumo_wakeup(Data) -> replace_null(Data).

%% @doc Part of the sumo_doc behavior.
-spec sumo_sleep(message()) -> sumo:doc().
sumo_sleep(Message) -> Message.

%% @doc Part of the sumo_doc behavior.
-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(?MODULE, [
    sumo:new_field(id             , integer,  [id, auto_increment, not_null]),
    sumo:new_field(content_id     , integer,  [not_null]),
    sumo:new_field(response_id    , integer,  []),
    sumo:new_field(message_text   , string ,  [{length, 1024}, not_null]),
    sumo:new_field(user           , integer,  [not_null])
  ]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public Api
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc functions definitions for message

-spec new(integer(), integer(), string(), integer()) -> message().
new( ContentId, ResponseId, Message, User) -> 
  #{  id           => undefined
    , content_id   => ContentId
    , response_id  => ResponseId
    , message_text => Message
    , user         => User
       %date
    }.

%% Getters/Setters %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec id(message()) -> integer().
id(Message) -> maps:get(id, Message).  

-spec content_id(message()) -> integer().
content_id(Message) ->  maps:get(content_id, Message). 

-spec response_id(message()) -> integer() | undefined.
response_id(Message) ->  maps:get(response_id, Message). 

-spec message_text(message()) -> string().
message_text(Message) -> maps:get(message_text, Message). 

-spec message_text(message(), string()) -> message().
message_text(Message, MessageText) -> Message#{ message_text => MessageText}. 

-spec is_top_message(message()) -> true | false.
is_top_message(Message) -> response_id(Message) == undefined.
