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

-module(conferl_domain_repo).
-author('David Cao <david.cao@inakanetworks.com>').

%%% General repo functions.
-export(
  [ create/1
  , create/2
  , update/1
  , delete/1
  , find_by_url/1
  , find_by_id/1
  ]).


-spec create(conferl_domain:domain()) -> conferl_domain:domain().
create(Domain) ->
	sumo:persist(conferl_domain, Domain).

-spec create(integer(), string()) -> conferl_domain:domain() | invalid_url.
create(Id, Url) ->
  sumo:persist(conferl_domain, conferl_domain:new(Id, Url) ).  

-spec update(conferl_domain:domain()) -> conferl_domain:domain().
update(Domain) ->
	sumo:persist(conferl_domain, Domain).

-spec delete(conferl_domain:domain()) -> integer().
delete(Domain) ->
	sumo:delete_by(conferl_domain,[{id, conferl_domain:id(Domain)}]).	

-spec find_by_url(string()) -> conferl_domain:domain().
find_by_url(Url) -> 
	sumo:find_by(conferl_domain, [{url, Url}]).	

-spec find_by_id(integer()) -> conferl_domain:domain().
find_by_id(Id) -> 
	sumo:find(conferl_domain, Id).	
