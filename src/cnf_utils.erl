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
-export([ now_datetime/0
        , handle_exception/3
        ]).

-spec now_datetime() -> datetime().
now_datetime() ->
  {datetime, calendar:universal_time()}.


-spec handle_exception(atom(), cowboy_req:req(), term()) ->
    {halt, cowboy_req:req(), term()}.
handle_exception(duplicate_user, Req, State) ->
  {ok, Req1} = cowboy_req:reply(400, Req),
  {halt, Req1, State};
handle_exception(duplicate_content, Req, State) ->
  {ok, Req1} = cowboy_req:reply(400, Req),
  {halt, Req1, State};
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
