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

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).
-export([test_handle_post_ok/1]).
-export([test_handle_delete_ok/1]).
-export([test_get_ok/1]).
-export([test_get_qs_ok/1]).
-export([test_handle_post_duplicated/1]).

-type config() :: [{atom(), term()}].

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
  {ok, Response} = cnf_utils:api_call(post, "/contents", Header,  JsonBody),
  #{status_code := 204} = Response,
  #{headers := ResponseHeaders} = Response,
  Location = proplists:get_value(<<"location">>, ResponseHeaders),
  <<"http://localhost/contents/", _Id/binary>> = Location.

test_handle_post_duplicated(Config) ->
  Header = #{<<"Content-Type">> => <<"application/json">>},
  Body = #{url => <<"http://inaka.net/post_duplicated">>, user_id => 2345},
  JsonBody = jiffy:encode(Body),
  cnf_content_repo:register("http://inaka.net/post_duplicated", 2345),
  {ok, Response} = cnf_utils:api_call(post, "/contents", Header,  JsonBody),
  #{status_code := 400} = Response.

test_handle_delete_ok(Config) ->
  Header = #{<<"Content-Type">> => <<"application/json">>},
  Body = #{url => <<"http://inaka.net/">>, user_id => 2345},
  Content = cnf_content_repo:register("http://inaka.net/delete_ok", 2345),
  Url = "/contents/" ++  integer_to_list(cnf_content:id(Content)),
  {ok, Response} = cnf_utils:api_call(delete, Url, Header),
  #{status_code := 204} = Response.

test_get_ok(Config) ->
  Header = #{<<"Content-Type">> => <<"application/json">>},
  Content = cnf_content_repo:register("http://inaka.net/get_ok", 2345),
  Url = "/contents/" ++  integer_to_list(cnf_content:id(Content)),
  {ok, Response} = cnf_utils:api_call(get, Url, Header),
  #{status_code := 200} = Response.

test_get_qs_ok(Config) ->
  Header = #{<<"Content-Type">> => <<"text/plain; charset=utf-8">>} ,
  cnf_content_repo:register("http://inaka.net/get_qs_ok", 1),
  cnf_content_repo:register("https://twitter.com/get_qs_ok", 1),
  DomainInaka = "inaka.net",
  DomainTwitter = "twitter.com",
  UrlInaka = "/contents/?domain=" ++  DomainInaka,
  {ok, ResponseInaka} = cnf_utils:api_call(get, UrlInaka, Header),
  #{status_code := 200} = ResponseInaka,
  #{body := JsonBodyRespInaka} = ResponseInaka,
  BodyRespInaka = jiffy:decode(JsonBodyRespInaka, [return_maps]),
  ct:pal("BodyRespInaka ~p", [BodyRespInaka]),
  UrlTwitter = "/contents/?domain=" ++  DomainTwitter,
  F1 = fun(DomainMap) ->
         #{<<"domain">> := Domain1} = DomainMap,
         Domain1 == <<"inaka.net">>
       end,
  ok = lists:foreach(F1, BodyRespInaka),
  {ok, ResponseTwitter} = cnf_utils:api_call(get, UrlTwitter, Header),
  #{body := JsonBodyRespTwitter} = ResponseTwitter,
  BodyRespTwitter = jiffy:decode(JsonBodyRespTwitter, [return_maps]),
  F2 = fun(DomainMap) ->
         #{<<"domain">> := Domain1} = DomainMap,
         Domain1 == <<"twitter.com">>
       end,
  ok = lists:foreach(F2, BodyRespTwitter),
  #{status_code := 200} = ResponseTwitter.





