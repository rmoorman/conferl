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
-module(conferl_content).
-author('david.cao@inakanetworks.com').


-type message() ::
        #{
           id      => integer(),
           message => string(),
           user    => integer()
           %date
         }.


-export_type( [message/0]).

-type content() ::
        #{
           id      => integer(),
           messages => [message()],
           user    => integer()
           %date
         }.

-export_type( [content/0]).
%%% sumo_db callbacks
-export([sumo_schema/0, sumo_wakeup/1, sumo_sleep/1]).

-export([   new/3
          , register_content/1
          , unregister_content/1
          , fetch_content/1
          , list_contents/1 ]).

-behavior(sumo_doc).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BEHAVIOUR CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Part of the sumo_doc behavior.
%%

-spec sumo_wakeup(sumo:doc()) -> content().
sumo_wakeup(Data) ->  Data.

%% @doc Part of the sumo_doc behavior.
-spec sumo_sleep(content()) -> sumo:doc().
sumo_sleep(Content) ->  Content.

%% @doc Part of the sumo_doc behavior.
-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
    sumo:new_schema(?MODULE, [
    sumo:new_field(id  , integer,       [not_null, auto_increment, id]),
    sumo:new_field(user, integer,       [not_null]),
    sumo:new_field(message_id, integer, [index])
  ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public Api
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc functions definitions for content


-spec new( integer(),[message()], integer()) -> content().
new( Id , Messages, User ) -> 
        #{
           id       => Id,
           messages => Messages,
           user     => User
           %date
         }.


%% Getters/Setters
-spec id(content()) -> integer().
id(Content) ->
  maps:get(id, Content).

-spec id(content(), integer()) -> content().
id(Content, Id) ->
  Content#{id => Id} .

-spec messages(content()) -> [message()].
messages(Content) ->
 maps:get(messages, Content).

 -spec messages(content(), [message()]) -> content().
messages(Content, Messages) ->
 Content#{messages => Messages}.

-spec user(content()) -> integer().
user(Content) -> 
  maps:get(user, Content).

-spec user(content(), integer()) -> content().
user(Content, User) -> 
  Content#{user => User}.  

-spec register_content(Url :: iodata()) -> 
  conferl_contents:content() | error.


register_content(Url) -> #{}.
%% todo

-spec unregister_content(ContentId :: integer()) -> ok | error .

unregister_content(ContentId ) ->  error .
%% todo

-spec fetch_content(ContentId :: integer()) -> 
   notfound | conferl_contents:content().

fetch_content(ContentId) ->   notfound.   
%% todo

-spec list_contents(Domain :: iodata()) -> [conferl_contents:content()].
list_contents(Domain) -> [ #{} ].