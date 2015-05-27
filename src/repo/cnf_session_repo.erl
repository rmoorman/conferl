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
-module(cnf_session_repo).

-author('David Cao <david.cao@inakanetworks.com>').

-export([register/1]).
-export([unregister/1]).
-export([find_by_user/1]).
-export([find_by_token/1]).
-export([is_valid/1]).

-spec register(non_neg_integer()) -> cnf_session:session().
register(UserId) ->
  NewToken = generate_token(),
  NewSession = cnf_session:new(UserId, NewToken),
  sumo:persist(cnf_session, NewSession).

-spec unregister(string()) -> non_neg_integer().
unregister(Token) ->
  sumo:delete_by(cnf_session, [{token, Token}]).

-spec find_by_user(non_neg_integer()) -> cnf_session:session().
find_by_user(UserId) ->
  Result = sumo:find_by(cnf_session, [{user_id, UserId}]),
  case Result of
    [] -> throw(notfound);
    [User] -> User
  end.

-spec find_by_token(binary()) -> cnf_session:session().
find_by_token(Token) ->
  Result =  sumo:find_by(cnf_session, [{token, Token}]),
  case Result of
    [] -> throw(notfound);
    [User] -> User
  end.

-spec generate_token() -> binary().
generate_token() -> erlang:list_to_binary(uuid:uuid_to_string(uuid:get_v4())).

-spec is_valid(binary()) -> boolean().
is_valid(Token) ->
  try
    Session = find_by_token(Token),
    {ok, MaxSessionDays} = application:get_env(conferl, max_session_days),
    UpdatedTime = cnf_session:updated_at(Session),
    Now = calendar:universal_time(),
    Diff = calendar:time_difference(UpdatedTime, Now),
    case Diff of
      {DiffDays, _Time} when DiffDays < MaxSessionDays ->
        true;
      _WhenOthers ->
        false
    end
  catch
    throw:notfound -> false
  end.
