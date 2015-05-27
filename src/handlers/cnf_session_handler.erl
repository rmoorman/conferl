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

-module(cnf_session_handler).

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
-export([delete_resource/2]).
-export([allowed_methods/2]).
-export([is_authorized/2]).

-type state() :: #{}.

allowed_methods(Req, State) ->
  {[ <<"POST">>
   , <<"DELETE">>
   , <<"OPTIONS">>]
  , Req
  , State}.

-spec is_authorized(cowboy_req:req(), state()) ->
  {boolean() | {boolean(), binary()}, cowboy_req:req(), state()}.
is_authorized(Req, State) ->
  case cowboy_req:parse_header(<<"authorization">>, Req) of
    {ok, {<<"basic">>, {Login, Password}}, _} ->
      try cnf_user_repo:is_registered(Login, Password) of
        ok -> NewState = #{login => Login},
          {true, Req, NewState}
      catch
        _Type:Exception ->
          cnf_utils:handle_exception(Exception, Req, State)
      end;
    _ -> {{false, <<"Basic realm=\"conferl\"">>}, Req, State}
  end.

-spec handle_post(cowboy_req:req(), state()) ->
  {true, cowboy_req:req(), state()}
  | {tuple(), cowboy_req:req(), state()}.
handle_post(Req, State) ->
  #{login := Login} = State,
  User = cnf_user_repo:find_by_name(Login),
  Session = cnf_session_repo:register(cnf_user:id(User)),
  JsonBody = cnf_session:to_json(Session),
  Req1 = cowboy_req:set_resp_body(JsonBody, Req),
  {true, Req1, State}.

-spec delete_resource(cowboy_req:req(), state()) ->
  {boolean(), cowboy_req:req(), state()}.
delete_resource(Req, State) ->
  {Token, Req1} = cowboy_req:binding(token, Req),
  #{login := VerifyiedLogin} = State,
  User = cnf_user_repo:find_by_name(VerifyiedLogin),
  Session = cnf_session_repo:find_by_token(Token),
  true = cnf_session:user_id(Session) == cnf_user:id(User),
  cnf_session_repo:unregister(binary_to_list(Token)),
  {true, Req1, State}.
