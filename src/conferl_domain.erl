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

-module(conferl_domain).
-author('david.cao@inakanetworks.com').

%% Codigo de referencia %%%%%
-type domain() ::
        #{
           id           => integer(),
           url          => string()
           %date
         }.


-export_type( [domain/0]).

%%% sumo_db callbacks
-export([sumo_schema/0, sumo_wakeup/1, sumo_sleep/1]).

-export([ get_domain/1]).

-behavior(sumo_doc).

%% getters/Setter

-spec id(domain()) -> integer().
id(Domain) -> 
	maps:get(Domain, id).

-spec id(domain(), integer()) -> domain().
id(Domain, Id) -> 
	Domain#{ id => Id}.	

-spec url(domain()) -> string().
id(Domain) -> 
	maps:get(Domain, url).

-spec url(domain(), string()) -> domain().
id(Domain, Url) -> 
	Domain#{ url => Url}.		

-spec get_domain(string()) -> {ok,string()} | {error,no_scheme}.
get_domain(Url) -> 
  case http_uri:parse(Url) of
    {error,no_scheme}       -> {error,no_scheme};
    {ok,{_,_,Domain,_,_,_}} -> {ok, Domain}
  end.  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BEHAVIOUR CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Part of the sumo_doc behavior.
%%

-spec sumo_wakeup(sumo:doc()) -> domain().
sumo_wakeup(Domain) ->  Domain.

%% @doc Part of the sumo_doc behavior.
-spec sumo_sleep(domain()) -> sumo:doc().
sumo_sleep(Domain) ->  Domain.

%% @doc Part of the sumo_doc behavior.
-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
    sumo:new_schema(?MODULE, [
    sumo:new_field(id  , integer,       [not_null, auto_increment, id]),
    sumo:new_field(url, string,         [not_null])
    %sumo:new_field(message_id, integer, [index])
  ]).	