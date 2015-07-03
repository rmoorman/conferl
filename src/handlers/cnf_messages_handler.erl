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

-module(cnf_messages_handler).

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

-export([handle_get/2]).
-export([handle_post/2]).
-export([allowed_methods/2]).
-export([is_authorized/2]).

-type state() :: #{}.

allowed_methods(Req, State) ->
  {[ <<"GET">>
   , <<"POST">>
   , <<"OPTIONS">>]
  , Req
  , State}.

-spec is_authorized(cowboy_req:req(), state()) ->
  {boolean() | {boolean(), binary()}, cowboy_req:req(), state()}.
is_authorized(Req, State) ->
  is_authorized_by_token(Req, State).

-spec handle_post(cowboy_req:req(), state()) ->
  {halt | true, cowboy_req:req(), state()}.
handle_post(Req, State) ->
  {ok, JsonBody, Req1} = cowboy_req:body(Req),
  Body = jiffy:decode(JsonBody, [return_maps]),
  #{ <<"in_reply_to">> := ReplayMsgId
   , <<"message">> :=  Message
   , <<"content">> :=  ContentId} = Body,
  try
    #{token := Token} = State,
    UserId  =
      cnf_session:user_id(cnf_session_repo:find_by_token(Token)),
    MsgResponse =
      cnf_message_repo:write_reply(ContentId, ReplayMsgId, Message, UserId),
    JsonRespBody = cnf_message:to_json(MsgResponse),
    Req2 = cowboy_req:set_resp_body(JsonRespBody, Req1),
    {true, Req2, State}
  catch
    _Type:Exception ->
      cnf_utils:handle_exception(Exception, Req, State)
  end.

-spec handle_get(cowboy_req:req(), state()) ->
  {list(), cowboy_req:req(), state()}.
handle_get(Req, State) ->
  OptionQueryL =
    [ <<"all_msg_content">>
    , <<"all_rply_content">>
    , <<"top_msg_content">>
    , <<"all_msg_user">>
    ],
  CustomQuerysFun =
  fun(Option, {ReqFold, WhereList}) ->
    {QueryStringVal, NewReq} =
      cowboy_req:qs_val(Option, ReqFold, <<"undefined">>),
    case {Option, QueryStringVal} of
      { _Option , <<"undefined">>} ->
        { NewReq, WhereList};
      {<<"all_msg_content">>, Value}   ->
        ContentId = list_to_integer(binary_to_list(Value)),
        { NewReq, WhereList ++ {content_id, ContentId}};
      {<<"top_msg_content">>, Value} ->
        ContentId = list_to_integer(binary_to_list(Value)),
        { NewReq, WhereList ++ [{content_id, ContentId}, {response_id, null}]};
      {<<"all_rply_content">>, Value}  ->
         ContentId = list_to_integer(binary_to_list(Value)),
        { NewReq
        , WhereList ++ [{content_id, ContentId}, {response_id, not_null}]};
      {<<"all_msg_user">>, Value} ->
        UserId = list_to_integer(binary_to_list(Value)),
        { NewReq, WhereList ++ [{user_id, UserId}]};
       _WhenOthers ->
        { NewReq, WhereList}
    end
  end,
  OptionOrderL =
    [ <<"sort_created_at">>
    , <<"sort_by_score">>
    ],
  CustomOrderFun =
  fun(Option, {ReqFold, OrderList}) ->
    {QueryStringVal, NewReq} =
      cowboy_req:qs_val(Option, ReqFold, <<"undefined">>),
    case {Option, QueryStringVal} of
      { _Option , <<"undefined">>} ->
        { NewReq, OrderList};
      {<<"sort_created_at">> , <<"true">>} ->
        {NewReq, OrderList ++ {created_at, desc}};
      {<<"sort_by_score">> , <<"true">>} ->
        { NewReq, OrderList ++ {score, desc}};
       _WhenOthers ->
        { NewReq, OrderList}
    end
  end,
  {Req2, OrderString} = lists:foldr(CustomOrderFun, {Req, []}, OptionOrderL),
  {Req3, QueryString} = lists:foldr(CustomQuerysFun, {Req2, []}, OptionQueryL),
  RequestContent = cnf_message_repo:custom_query(QueryString, OrderString),
  Fun1 =
    fun (Message) ->
      ListVotes = cnf_vote_repo:list_votes(cnf_message:id(Message)),
      Message#{ votes => ListVotes}
    end,
  RequestContent2 = lists:map(Fun1, RequestContent),
  Body = cnf_content:to_json(RequestContent2),
  {Body, Req3, State}.
