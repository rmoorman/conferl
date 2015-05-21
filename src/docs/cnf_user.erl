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
-module(cnf_user).

-author('David Cao <david.cao@inakanetworks.com>').

-opaque user() ::
  #{  id         => integer()
    , user_name  => string()
    , password   => string()
    , email      => string()
    , created_at => tuple()
    , updated_at => tuple()
    }.

-export_type([user/0]).

%%% sumo_db callbacks
-export([sumo_schema/0]).
-export([sumo_wakeup/1]).
-export([sumo_sleep/1]).

-export([new/3]).
-export([id/1]).
-export([user_name/1]).
-export([user_name/2]).
-export([password/1]).
-export([password/2]).
-export([email/1]).
-export([email/2]).
-export([created_at/1]).
-export([created_at/2]).
-export([updated_at/1]).
-export([updated_at/2]).
-export([map_to_json/1]).

-behavior(sumo_doc).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BEHAVIOUR CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Part of the sumo_doc behavior.
%%

%%% sumo_db callbacks

-spec sumo_wakeup(sumo:doc()) -> user().
sumo_wakeup(Data) -> Data.

%% @doc Part of the sumo_doc behavior.
-spec sumo_sleep(user()) -> sumo:doc().
sumo_sleep(User) -> User.

%% @doc Part of the sumo_doc behavior.
-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(?MODULE, [
    sumo:new_field(id        , integer , [id, auto_increment, not_null]),
    sumo:new_field(user_name , string  , [not_null]),
    sumo:new_field(password  , string  , [not_null]),
    sumo:new_field(email     , string  , [not_null]),
    sumo:new_field(created_at, datetime, [not_null]),
    sumo:new_field(updated_at, datetime, [not_null])
  ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public Api
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc functions definitions for user

-spec new( string()
         , string()
         , string()) -> user().
new(UserName, Password, Email) ->
  Now = calendar:universal_time(),
  #{ id         => undefined
   , user_name  => UserName
   , password   => Password
   , email      => Email
   , created_at => Now
   , updated_at => Now
   }.

-spec id(user()) -> integer().
id(User) -> maps:get(id, User).

-spec user_name(user()) -> string().
user_name(User) ->
  maps:get(user_name,User).

-spec user_name(user(), string()) -> user().
user_name(User, UserName) ->
  User#{user_name => UserName}.

-spec password(user()) -> string().
password(User) ->
  maps:get(password, User).

-spec password(user(), string()) -> user().
password(User, Password) ->
  User#{password => Password}.

-spec email(user()) -> string().
email(User) ->
  maps:get(email, User).

-spec email(user(), string()) -> user().
email(User, Email) ->
  User#{email => Email}.

-spec created_at(user()) -> tuple().
created_at(User) ->
  maps:get(created_at, User).

-spec created_at(user(), tuple()) -> user().
created_at(User, CreatedAt) ->
  User#{created_at => CreatedAt}.

-spec updated_at(user()) -> tuple().
updated_at(User) ->
  maps:get(updated_at, User).

-spec updated_at(user(), tuple()) -> user().
updated_at(User, UpdatedAt) ->
  User#{updated_at => UpdatedAt}.

-spec map_to_json(user() | [user()]) -> user() | [user()].
map_to_json(User) when is_map(User) ->
  jiffy:encode(doc_to_binary_date(User));
map_to_json(ListUsers) when is_list(ListUsers) ->
  JsonListUser = lists:map(fun doc_to_binary_date/1, ListUsers),
  jiffy:encode(JsonListUser).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private Api
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%
-spec doc_to_binary_date(map()) -> map().
doc_to_binary_date(User) ->
  CreatedAtBinary = cnf_utils:datetime_to_binary(created_at(User)),
  UpdatedAtBinary = cnf_utils:datetime_to_binary(updated_at(User)),
  User#{created_at => CreatedAtBinary, updated_at => UpdatedAtBinary}.
