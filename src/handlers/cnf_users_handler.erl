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
-module(cnf_users_handler).

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

-type state() :: #{}.

allowed_methods(Req, State) ->
  {[ <<"POST">>
   , <<"OPTIONS">>]
  , Req
  , State}.


-spec handle_post(cowboy_req:req(), state()) ->
  {true, cowboy_req:req(), state()}.
handle_post(Req, State) ->
  {ok, JsonRequestBody, Req1} = cowboy_req:body(Req),
  #{<<"user_name">> := UserName
   , <<"email">> := Email
   , <<"password">> := Password} = jiffy:decode(JsonRequestBody, [return_maps]),
  try
    RegistedUser = cnf_user_repo:register(UserName, Password, Email) ,
    JsonResponseBody = cnf_user:to_json(RegistedUser),
    Req2 = cowboy_req:set_resp_body(JsonResponseBody, Req1),
    {true, Req2, State}
  catch
    _Type:Exception ->
      cnf_utils:handle_exception(Exception, Req, State)
  end.
