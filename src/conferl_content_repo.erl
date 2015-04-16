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
  [ create/1
  , update/1
  , delete/1
  , delete_all/0
  , find_by_url/1
  , find_by_user/1
  , register_content/2
  , unregister_content/1
  , fetch_content/1
  , list_contents/1
  ]).


-spec create(conferl_content:content()) -> 
  conferl_content:content() | duplicate_content.
create( Content ) ->
  case find_by_url( conferl_content:url(Content) ) of
    notfound  -> sumo:persist(conferl_content, Content);
    _         -> throw(duplicate_content)
  end.


-spec update(conferl_content:content()) -> conferl_content:content().
update( Content ) ->
  sumo:persist(conferl_content, Content).

-spec delete( conferl_content:content() ) -> integer().
delete( Content ) -> 
  Id = conferl_content:id(Content),
  sumo:delete_by(conferl_content, [{id, Id}]).

-spec delete_all() -> integer(). 
delete_all() -> sumo:delete_all(conferl_content). 
  

-spec find_by_url(string()) -> not_found | conferl_content:content().
find_by_url(Url) ->
  case sumo:find_by(conferl_content, [{url,Url}]) of
    []        -> notfound ;
    [Content] -> Content
  end. 

-spec find_by_user(integer()) -> not_found | conferl_content:content().
find_by_user(UserIdUserId)  ->
  case sumo:find_by(conferl_content,[{user, UserIdUserId}]) of
    []        -> notfound ;
    [Content] -> Content
  end. 

-spec find_by_domain(string()) -> not_found | conferl_content:content().
find_by_domain(Domain) ->
  case sumo:find_by(conferl_content, [{domain,Domain}]) of
    []        -> notfound ;
    [Content] -> Content
  end.   

-spec register_content(string(), integer()) -> 
conferl_contents:content() | invalid_url | duplicate_content.
register_content(Url, User) -> 
  Content = conferl_content:new(Url, User),
  create(Content).

-spec unregister_content(string()) -> ok | error .
unregister_content(Content) ->  
  delete(Content).
%% todo

-spec fetch_content(integer()) -> notfound | conferl_content:content().
fetch_content(ContentId) -> 
    case sumo:find(conferl_content,ContentId) of
      notfound  -> throw(notfound);
      Content   -> Content
    end.  
%% todo

-spec list_contents(Domain :: string())
  -> [conferl_contents:content()] | notfound.
list_contents(Domain) ->
  find_by_domain(Domain).  

  
