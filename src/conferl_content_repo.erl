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
-module(conferl_content_repo).
-author('David Cao <david.cao@inakanetworks.com>').

%%% General repo functions.
-export(
  [ create/3
  , update/1
  , delete/1
  , delete_all/0
  , find_by_url/1
  , find_by_user/1
  , register_content/1
  , unregister_content/1
  , fetch_content/1
  , list_contents/1
  ]).




-spec create( Id       :: integer()
            , User     :: integer()
            , Url      :: iodata()) -> conferl_content:content().
create(  Id  ,Messages ,User ) ->
  Content = conferl_content:new( Id ,Messages , User),
  sumo:persist(conferl_content, Content).

-spec create(conferl_content:content()) -> conferl_content:content().
create( Content ) ->
  sumo:persist(conferl_content, Content).


-spec update(conferl_content:content()) -> conferl_content:content().
update( Content ) ->
  sumo:persist(conferl_content, Content).

-spec delete( conferl_content:content() ) -> integer().
delete( Content ) -> 
  Id = conferl_content:id(Content),
  sumo:delete_by(conferl_content, [{id, Id}]).

-spec delete_all() -> integer(). 
delete_all() -> sumo:delete_all(conferl_content). 
  

-spec find_by_url(iodata()) -> not_found | conferl_content:content().
find_by_url( Url) ->
  sumo:find(conferl_content, [{url,Url}]).  

-spec find_by_user(integer()) -> not_found | conferl_content:content().
find_by_user(UserIdUserId)  ->
  sumo:find_by(conferl_content,[{user, UserIdUserId}]).

-spec find_by_id_domain(integer()) -> not_found | conferl_content:content().
find_by_id_domain(Id_Domain) ->
  sumo:find_by(conferl_content,[{id_domain, Id_Domain}]).  

-spec register_content(conferl_contents:content()) -> conferl_contents:content() | error.
register_content(Content) -> 
  create(Content).

-spec unregister_content(conferl_contents:content()) -> ok | error .
unregister_content(Content) ->  
  delete(Content).
%% todo

-spec fetch_content(ContentId :: integer()) -> 
   notfound | conferl_contents:content().
fetch_content(ContentId) -> 
  sumo:find(conferl_content, ContentId).
%% todo

-spec list_contents(Domain :: iodata())
  -> [conferl_contents:content()] | notfound.
list_contents(Domain) ->
  DomainId = find_by_url(Domain),
  find_by_id_domain(DomainId).  

  
