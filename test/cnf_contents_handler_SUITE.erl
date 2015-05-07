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

-module(cnf_contents_handler_SUITE).

-author('David Cao <david.cao@inakanetworks.com>').

-type config() :: [{atom(), term()}].

-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , test_handle_post_ok/1
        , test_handle_delete_ok/1
        , test_get_ok/1
        , test_handle_post_duplicated/1
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Suite tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%

-spec ignored_funs() -> [atom()].
ignored_funs() ->
  [ module_info
  , init_per_suite
  , end_per_testcase
  , end_per_suite
  ].

-spec all() -> [atom()].
all() -> 
  [Fun || {Fun, 1} <- module_info(exports), 
          not lists:member(Fun, ignored_funs())].

%% @doc definion of init_per_testcases

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  application:ensure_all_started(conferl),
  application:ensure_all_started(shotgun),
  sumo:create_schema(),
  sumo:delete_all(cnf_content),
  [].

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  sumo:delete_all(cnf_content),
  Config.

%% @doc definion of init_per_testcases

init_per_testcase(_Function, Config) -> 
  Config.

%% @doc definion of end_per_testcases

end_per_testcase(_Function, Config) -> 
  Config.


test_handle_post_ok(Config) ->
  Header = #{<<"Content-Type">> => <<"application/json">>},
  Body = #{url => <<"http://inaka.net/post_ok">>, user_id => 2345},
  JsonBody = jiffy:encode(Body),
  {ok, Response} = api_call(post, "/contents", Header,  JsonBody),
  #{status_code := 204} = Response,
  #{headers := ResponseHeaders} = Response,
  ct:pal("ResponseHeaders ~p", [ResponseHeaders]),
  Location = proplists:get_value(<<"location">>,ResponseHeaders),
  <<"http://localhost/contents/", _Id/binary>> = Location.

test_handle_post_duplicated(Config) ->
  Header = #{<<"Content-Type">> => <<"application/json">>},
  Body = #{url => <<"http://inaka.net/post_duplicated">>, user_id => 2345},
  JsonBody = jiffy:encode(Body),
  cnf_content_repo:register("http://inaka.net/post_duplicated", 2345),
  {ok, Response} = api_call(post, "/contents", Header,  JsonBody),
  ct:pal("Response test_handle_post_duplicated ~p", [Response]),
  #{status_code := 400} = Response.

test_handle_delete_ok(Config) ->
  Header = #{<<"Content-Type">> => <<"application/json">>},
  Body = #{url => <<"http://inaka.net/">>, user_id => 2345},
  Content = cnf_content_repo:register("http://inaka.net/delete_ok", 2345),
  ct:pal("Content ~p", [Content]),
  Url = "/contents/" ++  integer_to_list(cnf_content:id(Content)),
  {ok, Response} = api_call(delete, Url, Header), 
  ct:pal("Response test_handle_delete_ok ~p", [Response]),
  #{status_code := 204} = Response.

test_get_ok(Config) ->
  Header = #{<<"Content-Type">> => <<"application/json">>},
  Body = #{url => <<"http://inaka.net/">>, user_id => 2345},
  Content = cnf_content_repo:register("http://inaka.net/get_ok", 2345),
  ct:pal("Content ~p", [Content]),
  Url = "/contents/" ++  integer_to_list(cnf_content:id(Content)),
  {ok, Response} = api_call(get, Url, Header), 
  ct:pal("Response test_get_ok ~p", [Response]),
  #{status_code := 204} = Response.


-spec api_call(atom(), string(), map()) -> {atom(), map()}.
api_call(Method, Url, Headers) ->
  api_call(Method, Url, Headers, "").

-spec api_call(atom(), string(), map(), map() | string()) -> {atom(), map()}.
api_call(Method, Url, Headers, Body) ->
  {ok, Pid} = shotgun:open("localhost", 8080),
  Response = shotgun:request(Pid, Method, Url, Headers, Body, #{}),
  shotgun:close(Pid),
  Response.

