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

-module(cnf_contents_handler).

-author('David Cao <david.cao@inakanetworks.com>').

-include_lib("mixer/include/mixer.hrl").
-mixin([
        {cnf_default_handler,
         [ init/3
         , rest_init/2
         , rest_terminate/2
         , content_types_accepted/2
         , content_types_provided/2
         ]}
       ]).

-export([is_authorized/2]).
-export([handle_get/2]).
-export([handle_post/2]).
-export([terminate/3]).
-export([allowed_methods/2]).

-type state() :: #{}.

allowed_methods(Req, State) ->
  {[ <<"GET">>
   , <<"POST">>
   , <<"OPTIONS">>]
  , Req
  , State}.

-spec is_authorized(cowboy_req:req(), state()) ->
  {tuple(), cowboy_req:req(), state()}.
is_authorized(Req, State) ->
  case cowboy_req:parse_header(<<"authorization">>, Req) of
    {ok, {<<"basic">>, {Login, Password}}, _} ->
      try cnf_user_repo:is_registered(Login, Password) of
        ok -> {true, Req, Login}
      catch
        _Type:Excep -> cnf_utils:handle_exception(Excep, Req, State)
      end;
    _WhenOthers ->
      {{false, <<"Basic realm=\"conferl\"">>}, Req, State}
  end.

-spec handle_post(cowboy_req:req(), state()) ->
  {halt | true, cowboy_req:req(), state()}.
handle_post(Req, State) ->
  {ok, JsonBody, Req1} = cowboy_req:body(Req),
  Body = jiffy:decode(JsonBody, [return_maps]),
  #{<<"url">> := Url, <<"user_id">> := UserId} = Body,
  try
    Content = cnf_content_repo:register(binary_to_list(Url), UserId),
    Id = cnf_content:id(Content),
    {Host, Req2} = cowboy_req:url(Req1),
    Location = [Host, <<"/">>, list_to_binary(integer_to_list(Id))],
    Req3 = cowboy_req:set_resp_header(<<"Location">>, Location, Req2),
    {true, Req3, State}
  catch
    _Type:Exception ->
      cnf_utils:handle_exception(Exception, Req, State)
  end.

-spec handle_get(cowboy_req:req(), state()) ->
  {list(), cowboy_req:req(), state()}.
handle_get(Req, State) ->
  {QueryStringVal, Req2} =  cowboy_req:qs_val(<<"domain">>, Req),
  RequestContent =
    cnf_content_repo:find_by_domain(binary_to_list(QueryStringVal)),
  Body = cnf_content:to_json(RequestContent),
  {Body, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.
