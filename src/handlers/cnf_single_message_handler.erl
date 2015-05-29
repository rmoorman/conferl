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

-module(cnf_single_message_handler).

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

-export([allowed_methods/2]).
-export([handle_get/2]).
-export([delete_resource/2]).
-export([is_authorized/2]).

-type state() :: #{}.

allowed_methods(Req, State) ->
  {[ <<"GET">>
   , <<"DELETE">>
   , <<"OPTIONS">>]
  , Req
  , State}.

-spec is_authorized(cowboy_req:req(), state()) ->
  {boolean() | {boolean(), binary()}, cowboy_req:req(), state()}.
is_authorized(Req, State) ->
  is_authorized_by_token(Req, State).

-spec handle_get(cowboy_req:req(), state()) ->
  {list(), cowboy_req:req(), state()}.
handle_get(Req, State) ->
  {Id, Req1} = cowboy_req:binding(messages_id, Req),
  RequestContent = cnf_message_repo:find(list_to_integer(binary_to_list(Id))),
  Body = cnf_message:to_json(RequestContent),
  {Body, Req1, State}.

-spec delete_resource(cowboy_req:req(), state()) ->
  {boolean(), cowboy_req:req(), state()}.
delete_resource(Req, State) ->
  {Id, Req1} = cowboy_req:binding(messages_id, Req),
  cnf_message_repo:delete(list_to_integer(binary_to_list(Id))),
  {true, Req1, State}.
