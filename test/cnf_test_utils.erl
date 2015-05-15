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
-module(cnf_test_utils).

-author('David Cao <david.cao@inakanetworks.com>').

-export([api_call/3]).
-export([api_call/4]).
-export([api_call/5]).

-spec api_call(atom(), string(), map()) -> {atom(), map()}.
api_call(Method, Url, Headers) ->
  api_call(Method, Url, Headers, "").

-spec api_call(atom(), string(), map(), map() | string()) -> {atom(), map()}.
api_call(Method, Url, Headers, Body) ->
 {ok, Port} = application:get_env(conferl, http_port),
  {ok, HttpHost} = application:get_env(conferl, http_host),
  {ok, Pid} = shotgun:open(HttpHost, Port),
  Response = shotgun:request(Pid, Method, Url, Headers, Body, #{} ),
  shotgun:close(Pid),
  Response.

-spec api_call(atom(), string(), map(), map() | string(), map()) ->
  {atom(), map()}.
api_call(Method, Url, Headers, Body, Option) ->
  {ok, Port} = application:get_env(conferl, http_port),
  {ok, HttpHost} = application:get_env(conferl, http_host),
  {ok, Pid} = shotgun:open(HttpHost, Port),
  Response = shotgun:request(Pid, Method, Url, Headers, Body, Option),
  shotgun:close(Pid),
  Response.
