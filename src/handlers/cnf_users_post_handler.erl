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
-module(cnf_users_post_handler).

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

-export([handle_post/2]).
-export([allowed_methods/2]).
-export([is_authorized/2]).

-type state() :: #{}.

allowed_methods(Req, State) ->
  {[ <<"POST">>
   , <<"OPTIONS">>]
  , Req
  , State}.


-spec is_authorized(cowboy_req:req(), state()) ->
  {tuple(), cowboy_req:req(), state()}.
is_authorized(Req, _State) ->
  {ok, {<<"basic">>, {Login, _Password}}, _}  =
     cowboy_req:parse_header(<<"authorization">>, Req),
     lager:error("is_authorized Login!!  ~p", [Login]),
      {true, Req, Login}.

-spec handle_post(cowboy_req:req(), state()) ->
  {halt | true, cowboy_req:req(), state()}.
handle_post(Req, State) ->
lager:info("handle_post <<authorization >> ~p", [cowboy_req:parse_header(<<"authorization">>, Req)]),
 {ok, {<<"basic">>, {UserName, Password}}, _} =
    cowboy_req:parse_header(<<"authorization">>, Req),
  try
    RegistedUser = cnf_user_repo:register_user(UserName, Password, "Email"),
    JsonBody = cnf_user:to_json(RegistedUser),
    Req1 = cowboy_req:set_resp_body(JsonBody, Req),
    {true, Req1, State}
  catch
    _Type:Exception ->
      cnf_utils:handle_exception(Exception, Req, State)
  end.
