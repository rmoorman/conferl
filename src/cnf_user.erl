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

-type user() ::
  #{  id        => integer()
    , user_name => string()
    , password  => string()
    , email     => string()
    }.

-export_type([user/0]).

%%% sumo_db callbacks
-export([ sumo_schema/0
        , sumo_wakeup/1
        , sumo_sleep/1
        ]).

-export([ new/3
        , id/1
        , user_name/1
        , user_name/2
        , password/1
        , password/2
        , email/1
        , email/2
        ]).

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
    sumo:new_field(id       , integer, [id, auto_increment, not_null]),
    sumo:new_field(user_name, string,  [not_null]),
    sumo:new_field(password , string,  [not_null]),
    sumo:new_field(email    , string,  [not_null])
  ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public Api
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc functions definitions for user

-spec new(string(), string(), string()) -> user().
new(UserName, Password, Email) ->
    #{  id        => undefined
      , user_name => UserName
      , password  => Password
      , email     => Email
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
