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

-module(cnf_session).
-author('david.cao@inakanetworks.com').

-opaque session() ::
  #{  id         => integer()
    , user_id    => integer()
    , token      => binary()
    , created_at => tuple()
    , updated_at => tuple()
   }.

-export_type([session/0]).

-export([new/2]).
-export([id/1]).
-export([user_id/1]).
-export([token/1]).
-export([token/2]).
-export([created_at/1]).
-export([created_at/2]).
-export([updated_at/1]).
-export([updated_at/2]).
-export([is_valid/1]).

-define(MAX_SESSION_DAYS, 42).

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

-spec sumo_wakeup(sumo:doc()) -> session().
sumo_wakeup(Data) -> Data.

%% @doc Part of the sumo_doc behavior.
-spec sumo_sleep(session()) -> sumo:doc().
sumo_sleep(Session) -> Session.

%% @doc Part of the sumo_doc behavior.
-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(?MODULE, [
    sumo:new_field(id        , integer , [id, auto_increment, not_null]),
    sumo:new_field(user_id   , integer , [not_null]),
    sumo:new_field(token     , binary  , [not_null]),
    sumo:new_field(created_at, datetime, [not_null]),
    sumo:new_field(updated_at, datetime, [not_null])
  ]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public Api
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc functions definitions for message
-spec new( integer(), binary()) -> session().
new(User_id, Token) ->
  Now = calendar:universal_time(),
  #{ id         => undefined
   , user_id    => User_id
   , token      => Token
   , created_at => Now
   , updated_at => Now
   }.

-spec id(session()) -> integer().
id(Session) ->
  maps:get(id, Session).

-spec user_id(session()) -> integer().
user_id(Session) ->
  maps:get(user_id, Session).

-spec token(session()) -> integer().
token(Session) ->
  maps:get(token, Session).

-spec token(session(), integer()) -> session().
token(Session, UserId) ->
  Session#{token => UserId}.

-spec created_at(session()) -> tuple().
created_at(Session) ->
  maps:get(created_at, Session).

-spec created_at(session(), tuple()) -> session().
created_at(Session, CreatedAt) ->
  Session#{created_at => CreatedAt}.

-spec updated_at(session()) -> tuple().
updated_at(Session) ->
  maps:get(updated_at, Session).

-spec updated_at(session(), tuple()) -> session().
updated_at(Session, UpdatedAt) ->
  Session#{updated_at => UpdatedAt}.

-spec is_valid(cnf_session:session()) -> boolean().
is_valid(Session) ->
  UpdatedTime = cnf_session:updated_at(Session),
  Now = calendar:universal_time(),
  Diff = calendar:time_difference(UpdatedTime, Now),
  case Diff of
    {DiffDays, _Time} when DiffDays < ?MAX_SESSION_DAYS ->
      true;
    _WhenOthers ->
      false
  end.
