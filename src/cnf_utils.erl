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
-export([api_call/5]).
-export([date_wakeup/1]).
-export([date_sleep/1]).
-export([datetime_to_binary/1]).
-export([sumodoc_to_json/1]).
-export([date_to_sumodate/1]).
-export([sumodate_to_date/1]).

-spec now_datetime() -> datetime().
now_datetime() -> {datetime, calendar:universal_time()}.

-spec date_to_sumodate(tuple()) -> datetime().
date_to_sumodate(Calendar) -> Calendar.

-spec sumodate_to_date(datetime()) -> datetime().
sumodate_to_date(Date) -> Date.

-spec date_sleep(map()) -> sumo:doc().
date_sleep(ConferlMap) ->
  ConferlMap#{created_at  => maps:get(created_at, ConferlMap)
             , updated_at => maps:get(updated_at, ConferlMap)}.

-spec date_wakeup(sumo:doc()) -> map().
date_wakeup(SumoDoc) ->
  NewData =  SumoDoc#{created_at => maps:get(created_at, SumoDoc)
                      , updated_at => maps:get(updated_at, SumoDoc)},
  CreatedAtBinary = cnf_utils:sumodate_to_date(maps:get(created_at, NewData)),
  UpdatedAtBinary = cnf_utils:sumodate_to_date(maps:get(updated_at, NewData)),
  NewData#{created_at => CreatedAtBinary, updated_at => UpdatedAtBinary}.

-spec datetime_to_binary(tuple()) -> binary().
datetime_to_binary({{Yi, Mi, Di}, {Hi, Ni, Si}}) ->
  Y = integer_to_list(Yi),
  M = integer_to_list(Mi),
  D = integer_to_list(Di),
  H = integer_to_list(Hi),
  N = integer_to_list(Ni),
  S = io_lib:format("~.2f",[Si]),
  % S = float_to_list(Si), %%%S = integer_to_list(Si),
  iolist_to_binary([Y, "-", M, "-", D, "T", H, ":", N, ":", S]).

sumodoc_to_json(ListDoc) when is_list(ListDoc) ->
  JsonListDocs = lists:map(fun doc_to_binary_date/1, ListDoc ),
  jiffy:encode(JsonListDocs);
  sumodoc_to_json(ListDoc) when is_map(ListDoc) ->
  jiffy:encode(doc_to_binary_date(ListDoc)).

-spec doc_to_binary_date(map()) -> map().
doc_to_binary_date(Doc) ->
  CreatedAtBinary = cnf_utils:datetime_to_binary(maps:get(created_at, Doc)),
  UpdatedAtBinary = cnf_utils:datetime_to_binary(maps:get(updated_at, Doc)),
  Doc#{created_at => CreatedAtBinary, updated_at => UpdatedAtBinary}.

-spec handle_exception(atom(), cowboy_req:req(), term()) ->
    {halt, cowboy_req:req(), term()}.
handle_exception(duplicate_user, Req, State) ->
  %{false, Req1, State} is equivalent-> {ok, Req1} = cowboy_req:reply(400, Req),
  {false, Req, State};
handle_exception(wrong_password, Req, State) ->
  {{false, <<"Basic realm=\"conferl\"">>}, Req, State};
handle_exception(not_registered, Req, State) ->
  {{false, <<"Basic realm=\"conferl\"">>}, Req, State};
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
