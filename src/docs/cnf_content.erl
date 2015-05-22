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
        #{  id         => integer()
          , url        => string()
          , domain     => string()
          , user_id    => integer()
          , created_at => tuple()
          , updated_at => tuple()
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
-export([user_id/1]).
-export([user_id/2]).
-export([domain/1]).
-export([domain/2]).
-export([created_at/1]).
-export([created_at/2]).
-export([updated_at/1]).
-export([updated_at/2]).
-export([to_json/1]).

-behavior(sumo_doc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private Api
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc functions definitions for content

-spec get_domain(string()) -> string().
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
sumo_wakeup(Data) -> Data.

%% @doc Part of the sumo_doc behavior.
-spec sumo_sleep(content()) -> sumo:doc().
sumo_sleep(Content) -> Content.

%% @doc Part of the sumo_doc behavior.
-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(?MODULE, [
    sumo:new_field(id        , integer , [id, auto_increment, not_null]),
    sumo:new_field(user_id   , integer , [not_null]),
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
  Now = calendar:universal_time(),
  #{ id         => undefined
   , url        => Url
   , domain     => get_domain(Url)
   , user_id    => User
   , created_at => Now
   , updated_at => Now
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

-spec user_id(content()) -> integer().
user_id(Content) ->
  maps:get(user_id, Content).

-spec user_id(content(), integer()) -> content().
user_id(Content, User) ->
  Content#{user_id => User}.

-spec domain(content()) -> string().
domain(Content) ->
  maps:get(domain, Content).

-spec domain(content(), string()) -> content().
domain(Content, Domain) ->
  Content#{domain => Domain}.

-spec created_at(content()) -> tuple().
created_at(Content) ->
  maps:get(created_at, Content).

-spec created_at(content(), tuple()) -> content().
created_at(Content, CreatedAt) ->
  Content#{created_at => CreatedAt}.

-spec updated_at(content()) -> tuple().
updated_at(Content) ->
  maps:get(updated_at, Content).

-spec updated_at(content(), tuple()) -> content().
updated_at(Content, UpdatedAt) ->
  Content#{updated_at => UpdatedAt}.

-spec to_json(content() | [content()]) -> content() | [content()].
to_json(Content) when is_map(Content) ->
  jiffy:encode(doc_to_binary_date(Content));
to_json(ListContents) when is_list(ListContents) ->
  JsonListContents = lists:map(fun doc_to_binary_date/1, ListContents),
  jiffy:encode(JsonListContents).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private Api
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%
-spec doc_to_binary_date(map()) -> map().
doc_to_binary_date(Content) ->
  CreatedAtBinary = cnf_utils:datetime_to_binary(created_at(Content)),
  UpdatedAtBinary = cnf_utils:datetime_to_binary(updated_at(Content)),
  Content#{created_at => CreatedAtBinary, updated_at => UpdatedAtBinary}.
