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
-module(cnf_utils).

-author('David Cao <david.cao@inakanetworks.com>').

-type datetime() ::
        {datetime,
          {
            {integer(), integer(), integer()},
            {integer(), integer(), integer()}
          }
        }.
-export_type([datetime/0]).

-export([now_datetime/0]).
-export([handle_exception/3]).
-export([api_call/3]).
-export([api_call/4]).
-export([date_wakeup/1]).
-export([date_sleep/1]).
-export([datetime_to_json/1]).

-spec now_datetime() -> datetime().
now_datetime() ->
  {datetime, calendar:universal_time()}.

-spec datetime_to_json(tuple()) -> binary().
datetime_to_json({{Yi, Mi, Di}, {Hi, Ni, Si}}) ->
  Y = integer_to_list(Yi),
  M = integer_to_list(Mi),
  D = integer_to_list(Di),
  H = integer_to_list(Hi),
  N = integer_to_list(Ni),
  S = integer_to_list(Si),
  iolist_to_binary([Y, "-", M, "-", D, "T", H, ":", N, ":", S, ".000000Z"]).

-spec date_sleep(map()) -> sumo:doc().
date_sleep(ConferlMap) ->
  ConferlMap#{created_at => term_to_binary(maps:get(created_at, ConferlMap))
             , updated_at => term_to_binary(maps:get(updated_at, ConferlMap))}.

-spec date_wakeup(sumo:doc()) -> map().
date_wakeup(SumoDoc) ->
  SumoDoc#{created_at => binary_to_term(maps:get(created_at, SumoDoc))
          , updated_at => binary_to_term(maps:get(updated_at, SumoDoc))}.

-spec handle_exception(atom(), cowboy_req:req(), term()) ->
    {halt, cowboy_req:req(), term()}.
handle_exception(duplicate_user, Req, State) ->
  %{false, Req1, State} is equivalent-> {ok, Req1} = cowboy_req:reply(400, Req),
  {false, Req, State};
handle_exception(duplicate_content, Req, State) ->
  {false, Req, State};
handle_exception(not_found, Req, State) ->
  {ok, Req1} = cowboy_req:reply(404, Req),
  {halt, Req1, State};
handle_exception(Reason, Req, State) ->
  lager:error("~p. Stack Trace: ~p", [Reason, erlang:get_stacktrace()]),
  lager:error("~p. handle_exception! Reason ~p", [Reason]),
  {ok, Req1} =
    try cowboy_req:reply(501, Req)
    catch
      _:Error ->
        lager:critical(
          "~p trying to report error through cowboy. Stack Trace: ~p",
          [Error, erlang:get_stacktrace()]),
        {ok, Req}
    end,
  {halt, Req1, State}.

-spec api_call(atom(), string(), map()) -> {atom(), map()}.
api_call(Method, Url, Headers) ->
  api_call(Method, Url, Headers, "").

-spec api_call(atom(), string(), map(), map() | string()) -> {atom(), map()}.
api_call(Method, Url, Headers, Body) ->
  {ok, Port} = application:get_env(conferl, http_port),
  {ok, ServerUrl} = application:get_env(conferl, server_url),
  {ok, Pid} = shotgun:open(ServerUrl, Port),
  Response = shotgun:request(Pid, Method, Url, Headers, Body, #{}),
  shotgun:close(Pid),
  Response.
