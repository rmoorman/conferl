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
-module(cnf_user_repo).

-author('David Cao <david.cao@inakanetworks.com>').
-export([ register_user/3
        , unregister_user/1
        , fetch_user/1
        , fetch_by_name/1
        ]).

-spec register_user(string(), string(), string()) -> conferl_users:user().
register_user(UserName, Password, Email) -> 
  try fetch_by_name(UserName) of
    _   -> throw(duplicate_user)
  catch 
    throw:notfound -> 
      NewUser = cnf_user:new(UserName, Password, Email),
      sumo:persist(cnf_user, NewUser)
  end.  

-spec unregister_user(string()) -> non_neg_integer().
unregister_user(UserName) -> 
  Result = sumo:delete_by(cnf_user, [{user_name, UserName}]),
  case Result of
    0               -> throw(notfound);
    NumberRows      -> NumberRows
  end.

-spec fetch_user(integer()) -> conferl_users:user().
fetch_user(UserId) -> 
  sumo:find(cnf_user, UserId).

-spec fetch_by_name(string()) -> conferl_users:user().
fetch_by_name(UserName) -> 
  Result = sumo:find_by(cnf_user, [{user_name, UserName}]),
  case Result of
    []        -> throw(notfound);
    [User]    -> User
  end.
