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
-module(cnf_content_repo).
-author('David Cao <david.cao@inakanetworks.com>').

%%% General repo functions.
-export(
  [ update/1
  , find_by_url/1
  , find_by_user/1
  , register/2
  , unregister/1
  , fetch/1
  , list/1
  ]).

-spec update(cnf_content:content()) -> cnf_content:content().
update(Content) ->
  sumo:persist(cnf_content, Content).
  
-spec find_by_url(string()) -> [cnf_content:content()].
find_by_url(Url) ->
  sumo:find_by(cnf_content, [{url, Url}]).

-spec find_by_user(integer()) -> [cnf_content:content()].
find_by_user(UserIdUserId)  ->
  sumo:find_by(cnf_content,[{user, UserIdUserId}]). 

-spec register(string(), integer()) -> cnf_content:content().
register(Url, User) -> 
  Content = cnf_content:new(Url, User),
  case find_by_url( cnf_content:url(Content) ) of
    []  -> sumo:persist(cnf_content, Content);
    _   -> throw(duplicate_content)
  end.

-spec unregister(non_neg_integer()) -> non_neg_integer().
unregister(Id) ->  
  sumo:delete(cnf_content, Id).

-spec fetch(integer()) -> notfound | cnf_content:content().
fetch(ContentId) -> 
  case sumo:find(cnf_content,ContentId) of
    notfound  -> throw(notfound);
    Content   -> Content
  end.  

-spec list(string()) -> [cnf_content:content()].
list(Domain) ->   
  sumo:find_by(cnf_content, [{domain, Domain}]). 
