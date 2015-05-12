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
-module(cnf_content).
-author('David Cao <david.cao@inakanetworks.com>').

-opaque content() ::
        #{  id      => integer()
          , url     => string()
          , domain  => string()
          , user    => integer()
          , created_at => conferl_utils:datetime()
          , updated_at => conferl_utils:datetime()
          }.

-export_type([content/0]).

%%% sumo_db callbacks
-export([sumo_schema/0]).
-export([sumo_wakeup/1]).
-export([sumo_sleep/1]).
%%
-export([new/2]).
-export([id/1]).
-export([url/1]).
-export([url/2]).
-export([user/1]).
-export([user/2]).
-export([domain/1]).
-export([domain/2]).
-export([created_at/1]).
-export([created_at/2]).
-export([updated_at/1]).
-export([updated_at/2]).

-behavior(sumo_doc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private Api
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc functions definitions for content

-spec get_domain(string()) -> string() | invalid_url.
get_domain(Url) ->
  case http_uri:parse(Url) of
    {error, _} -> throw(invalid_url);
    {ok, {_ , _, Domain, _, _, _}} -> Domain
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BEHAVIOUR CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Part of the sumo_doc behavior.
%%

-spec sumo_wakeup(sumo:doc()) -> content().
sumo_wakeup(Data) ->
NewData = cnf_utils:date_wakeup(Data),
{datetime, CreatedAt} = created_at(NewData),
{datetime, UpdatedAt} = updated_at(NewData),
CreatedAtBinary = cnf_utils:datetime_to_json(CreatedAt),
UpdatedAtBinary = cnf_utils:datetime_to_json(UpdatedAt),
NewData#{created_at => CreatedAtBinary, updated_at => UpdatedAtBinary}.

%% @doc Part of the sumo_doc behavior.
-spec sumo_sleep(content()) -> sumo:doc().
sumo_sleep(Content) -> cnf_utils:date_sleep(Content).

%% @doc Part of the sumo_doc behavior.
-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
    sumo:new_schema(?MODULE, [
      sumo:new_field(id        , integer , [id, auto_increment, not_null]),
      sumo:new_field(user      , integer , [not_null]),
      sumo:new_field(url       , string  , [not_null]),
      sumo:new_field(domain    , string  , [not_null]),
      sumo:new_field(created_at, datetime, [not_null]),
      sumo:new_field(updated_at, datetime, [not_null])
  ]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public Api
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc functions definitions for content

-spec new( string()
         , integer()) -> content() | invalid_url.
new(Url, User) ->
  #{  id         => undefined
    , url        => Url
    , domain     => get_domain(Url)
    , user       => User
    , created_at => cnf_utils:now_datetime()
    , updated_at => cnf_utils:now_datetime()
    }.

%% Getters/Setters %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec id(content()) -> integer().
id(Content) ->
  maps:get(id, Content).

-spec url(content()) -> string().
url(Content) ->
  maps:get(url, Content).

-spec url(content(), string()) -> content().
url(Content, Url) ->
  Content#{ url => Url}.

-spec user(content()) -> integer().
user(Content) ->
  maps:get(user, Content).

-spec user(content(), integer()) -> content().
user(Content, User) ->
  Content#{user => User}.

-spec domain(content()) -> string().
domain(Content) ->
  maps:get(domain, Content).

-spec domain(content(), string()) -> content().
domain(Content, Domain) ->
  Content#{domain => Domain}.

-spec created_at(content()) -> conferl_utils:datetime().
created_at(Content) -> maps:get(created_at, Content).

-spec created_at(content(), conferl_utils:datetime()) -> content().
created_at(Content, CreatedAt) -> Content#{created_at => CreatedAt}.

-spec updated_at(content()) -> conferl_utils:datetime().
updated_at(Content) -> maps:get(updated_at, Content).

-spec updated_at(content(), conferl_utils:datetime()) -> content().
updated_at(Content, UpdatedAt) -> Content#{updated_at => UpdatedAt}.
