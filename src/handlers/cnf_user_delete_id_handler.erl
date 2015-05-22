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

-module(cnf_user_delete_id_handler).

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
-export([delete_resource/2]).
-export([allowed_methods/2]).

-type state() :: #{}.

allowed_methods(Req, State) ->
  {[ <<"DELETE">>
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

-spec delete_resource(cowboy_req:req(), state()) ->
  {boolean(), cowboy_req:req(), state()}.
delete_resource(Req, State) ->
  {ok, {<<"basic">>, {VerificatedLogin, _VerificatedPassword}}, _}  =
    cowboy_req:parse_header(<<"authorization">>, Req),
  %%UserId = cnf_user_repo:find_by_name(VerificatedLogin),
  lager:error("delete_resource  VerificatedLogin ~p", [VerificatedLogin]),
  P = cnf_user_repo:unregister_user(binary_to_list(VerificatedLogin)),
  lager:error("delete_resource  unregister_user ~p", [P]),
  {true, Req, State}.
