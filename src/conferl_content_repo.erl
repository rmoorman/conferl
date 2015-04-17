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
  [ update/1
  , delete_all/0
  , find_by_url/1
  , find_by_user/1
  , register/2
  , unregister/1
  , fetch/1
  , list/1
  ]).

-spec update(conferl_content:content()) -> conferl_content:content().
update(Content) ->
  sumo:persist(conferl_content, Content).

-spec delete_all() -> integer(). 
delete_all() -> sumo:delete_all(conferl_content). 
  
-spec find_by_url(string()) -> [conferl_content:content()].
find_by_url(Url) ->
  sumo:find_by(conferl_content, [{url,Url}]).

-spec find_by_user(integer()) -> [conferl_content:content()].
find_by_user(UserIdUserId)  ->
  sumo:find_by(conferl_content,[{user, UserIdUserId}]). 

-spec register(string(), integer()) -> conferl_contents:content().
register(Url, User) -> 
  Content = conferl_content:new(Url, User),
  case find_by_url( conferl_content:url(Content) ) of
    []  -> sumo:persist(conferl_content, Content);
    _   -> throw(duplicate_content)
end.

-spec unregister(string()) -> non_neg_integer.
unregister(Content) ->  
  Id = conferl_content:id(Content),
  sumo:delete_by(conferl_content, [{id, Id}]).

-spec fetch(integer()) -> notfound | conferl_content:content().
fetch(ContentId) -> 
  case sumo:find(conferl_content,ContentId) of
    notfound  -> throw(notfound);
    Content   -> Content
  end.  

-spec list(string()) -> [conferl_contents:content()].
list(Domain) ->   
  sumo:find_by(conferl_content, [{domain,Domain}]). 
