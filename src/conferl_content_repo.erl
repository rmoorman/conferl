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
-author('david.cao@inakanetworks.com').

%%% General repo functions.
-export(
  [ create/3
  , update/1
  , delete/1
  , delete_all/2
  , delete/3
  , find/1
  , find_by_url/1
  , find_by_user/1
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
delete_all() -> sumo:delete(conferl_content, Content). 

-spec find(integer()) -> not_found | conferl_content:content().
find(Id) when is_integer(Id) ->
  sumo:find(conferl_content, Id).

-spec find_by_url(iodata()) -> not_found | conferl_content:content().
find_by_url( Url) ->
  sumo:find(conferl_content, [{url,Url}]).  

-spec find_by_user(integer()) -> not_found | conferl_content:content().
find(UserIdUserId) when is_integer(UserId) ->
  sumo:find_by(conferl_content,[{user, UserId}]).
