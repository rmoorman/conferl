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

-module(cnf_me_handler).

-author('David Cao <david.cao@inakanetworks.com>').

-include_lib("mixer/include/mixer.hrl").
-mixin([
        {cnf_default_handler,
         [ init/3
         , rest_init/2
         , rest_terminate/2
         , content_types_accepted/2
         , content_types_provided/2
         , is_authorized_by_token/2
         ]}
       ]).

-export([delete_resource/2]).
-export([allowed_methods/2]).
-export([is_authorized/2]).

-type state() :: #{}.

allowed_methods(Req, State) ->
  {[ <<"DELETE">>
   , <<"OPTIONS">>]
  , Req
  , State}.

-spec is_authorized(cowboy_req:req(), state()) ->
  {boolean() | {boolean(), binary()}, cowboy_req:req(), state()}.
is_authorized(Req, State) ->
  is_authorized_by_token(Req, State).

-spec delete_resource(cowboy_req:req(), state()) ->
  {boolean(), cowboy_req:req(), state()}.
delete_resource(Req, State) ->
  #{login := VerifyiedLogin} = State,
  cnf_user_repo:unregister(binary_to_list(VerifyiedLogin)),
  {true, Req, State}.
