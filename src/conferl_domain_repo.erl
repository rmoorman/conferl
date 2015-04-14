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
-author('david.cao@inakanetworks.com').

%%% General repo functions.
-export(
  [ create/2
  , update/1
  , delete/1
  , delete_all/2
  , delete/3
  , find/1
  , find_by_url/1
  , find_by_id/1
  ]).


-spec create(conferl_domain:domain()) -> conferl_domain:domain().
create(Domain) ->
	sumo:persist(conferl_domain, Domain).

-spec update(conferl_domain:domain()) -> conferl_domain:domain().
update(Domain) ->
	sumo:persist(conferl_domain, Domain).

-spec delete(conferl_domain:domain()) -> integer().
delete(Domain) ->
	sumo:delete_by(conferl_domain,[{id, conferl_domain:id(Domain)}])	

-spec delete_all(conferl_domain:conferl()) -> integer().
-spec delete_all(Domain)
	sumo:delete(conferl_domain, Domain).

-spec find_by_url(string()) -> conferl_domain:domain().
find_by_url(Url) -> 
	sumo:find_by(conferl_domain, [{url, Url}]).	

-spec find_by_id(integer()) -> conferl_domain:domain().
find_by_url(Id) -> 
	sumo:find_by(conferl_domain, [{id, Id}]).	