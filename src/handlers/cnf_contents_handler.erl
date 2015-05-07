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

-export([ handle_get/2
        , handle_post/2
        , delete_resource/2
        , terminate/3
        , allowed_methods/2
        ]).

allowed_methods(Req, State) ->
  {[ <<"POST">>
   , <<"GET">>
   , <<"DELETE">>
   , <<"OPTIONS">>]
  , Req
  , State}.

handle_post(Req, State) ->
    lager:info("handle_get"),
    {ok, JsonBody, Req1} = cowboy_req:body(Req),
    Body = jiffy:decode(JsonBody, [return_maps]),
    lager:info("body ~p", [Body]),
    #{<<"url">> := Url, <<"user_id">> := UserId} = Body,
    try
      Content = cnf_content_repo:register(binary_to_list(Url), UserId),
      Id = cnf_content:id(Content),
      {Host, Req2} = cowboy_req:url(Req1),
      Location = [Host, <<"/">>, list_to_binary(integer_to_list(Id))],
      lager:info("Location ~p", [Location]),
      Req3 = cowboy_req:set_resp_header(<<"Location">>, Location, Req2),
      {true, Req3, State}
    catch
      _throw:Exception ->
        cnf_utils:handle_exception(Exception, Req, State)
    end.

handle_get(Req, State) ->
  lager:debug("handle_post"),
  {QsVal, Req2} =  cowboy_req:qs_val(<<"domain">>, Req),
  lager:info("QsVal ~p", [QsVal]),
  case QsVal of
    undefined      -> {Id, Req3} = cowboy_req:binding(id_content, Req2),
      lager:info("get_resource - id_content ~p", [Id]),
      RequestContent =
        cnf_content_repo:find(list_to_integer(binary_to_list(Id))),
      lager:info("RequestContent ~p", [RequestContent]),
      Body =  jiffy:encode(RequestContent),
      lager:info("Body ~p", [Body]),
      {Body, Req3, State};
    Query  -> RequestContent =
      cnf_content_repo:find_by_domain(binary_to_list(Query)),
      lager:info("RequestContent qs ~p", [RequestContent]),
      Body =  jiffy:encode(RequestContent),
      lager:info("Body ~p", [Body]),
      {Body, Req2, State}
    end.




delete_resource(Req, State) ->
  {Id, Req1} = cowboy_req:binding(id_content, Req),
  lager:info("delete_resource - id_content ~p", [Id]),
  cnf_content_repo:unregister(list_to_integer(binary_to_list(Id))),
  {true, Req1, State}.

terminate(_Reason, _Req, _State) ->
  ok.
