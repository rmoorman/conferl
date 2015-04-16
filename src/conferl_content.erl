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
-author('David Cao <david.cao@inakanetworks.com>').

%% Codigo de referencia %%%%%
-type message() ::
        #{
           id              => integer(),
           id_content      => integer(),
           message         => string(),
           user            => integer()
           %date
         }.


-export_type( [message/0]).

%% Fin de Codigo de referencia %%%%%

-type content() ::
        #{
           id        => integer(),
           url       => string(),
           domain    => string(),
           user      => integer() 
           %date
         }.

-export_type( [content/0]).

%%% sumo_db callbacks
-export([sumo_schema/0, sumo_wakeup/1, sumo_sleep/1]).

-export([   new/2
          , id/1
          , url/1
          , url/2
          %, messages/1
          %, messages/2
          , user/1
          , user/2
          ]).

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
    sumo:new_field(id       , integer,        [id, auto_increment, not_null]),
    sumo:new_field(user     , integer,        [not_null]),
    sumo:new_field(url      , string ,        [not_null]),
    sumo:new_field(domain   , string ,        [not_null])
  ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public Api
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc functions definitions for content

-spec get_domain(string()) -> string() | invalid_url. 
get_domain(Url) -> 
  case http_uri:parse(Url) of
    {error,no_scheme}       -> throw(invalid_url); 
    {ok,{_,_,Domain,_,_,_}} -> Domain
  end.  

-spec new( string(), integer()) -> content() | invalid_url.
new(  Url, User ) -> 
        #{
           id       => undefined,
           url      => Url,
           domain   => get_domain(Url),
           %messages => Messages,
           user     => User
           %date
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



%%list_contents(Domain) -> [ #{} ].
%% todo