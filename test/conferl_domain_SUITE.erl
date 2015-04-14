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

-module(conferl_domain_SUITE).

-author('David Cao <david.cao@inakanetworks.com>').

-export([ all/0
        , init_per_testcase/2
        , end_per_testcase/2
        , wellformed_Url/1
        , malformed_Url/1]).

-type config() :: [{atom(), term()}].

-spec all() -> [atom()].
all() -> [Fun || {Fun, 1} <- module_info(exports), Fun =/= module_info].


%% @doc definion of init_per_testcases
init_per_testcase(wellformed_Url, _Config) -> 
  [{url, "http://inaka.net/"} ];
init_per_testcase(malformed_Url, _Config)  -> 
  [{url, "qweqwettyuiuy.null"} ].


%% @doc definion of end_per_testcase
end_per_testcase(_ , Config) ->  Config.

%% @doc tests for register_content

-spec wellformed_Url(config()) -> ok.
wellformed_Url( Config ) ->
  Url = proplists:get_value(url, Config),
  case conferl_domain:get_domain(Url) of
  	{ok,   _} -> ok;
  	{error,_} -> error
  end.

-spec malformed_Url(config()) -> ok.
malformed_Url( Config) ->
  Url = proplists:get_value(url, Config),
  case conferl_domain:get_domain(Url) of
  	{ok,   _} -> error;
  	{error,_} -> ok
  end.