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

-module(cnf_messages_handler_SUITE).

-author('David Cao <david.cao@inakanetworks.com>').

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).
-export([test_message_get_ok/1]).
-export([test_handle_delete_ok/1]).
-export([test_get_qs_all_msg_content/1]).
-export([test_get_qs_all_rply_content/1]).
-export([test_get_qs_top_msg_content/1]).
-export([test_get_qs_all_msg_user/1]).
-export([test_get_qs_sort_by_score/1]).
-export([test_get_qs_sort_created_at/1]).
-export([test_post_message_ok/1]).

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Suite tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%

-spec ignored_funs() -> [atom()].
ignored_funs() ->
  [ module_info
  , init_per_suite
  , end_per_testcase
  , end_per_suite
  ].

-spec all() -> [atom()].
all() ->
  [Fun || {Fun, 1} <- module_info(exports),
          not lists:member(Fun, ignored_funs())].

%% @doc definion of init_per_testcases

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  application:ensure_all_started(sumo_db),
  application:ensure_all_started(shotgun),
  sumo:create_schema(),
  lager:start(),
  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  sumo:delete_all(cnf_user),
  sumo:delete_all(cnf_message),
  sumo:delete_all(cnf_session),
  Config.

%% @doc definion of init_per_testcases
init_per_testcase(_Function, Config) ->
  Config.

%% @doc definion of end_per_testcases
end_per_testcase(_Function, Config) ->
  Config.

-spec test_message_get_ok(config()) -> config().
test_message_get_ok(Config) ->
  UserName = "Doge test_message_get_ok",
  Passsword = "passsword",
  Email = "email@email.net",
  Mgs = "Such message, very argument",
  User = cnf_user_repo:register(UserName, Passsword, Email),
  Session = cnf_session_repo:register(cnf_user:id(User)),
  Token = binary_to_list(cnf_session:token(Session)),
  UserId = cnf_user:id(User),
  Content = cnf_content_repo:register("http://inaka.net/get", UserId),
  ContentId = cnf_content:id(Content),
  TopMsg = cnf_message_repo:write_top(ContentId, Mgs, UserId),
  Header = #{ <<"Content-Type">> => <<"application/json">>
            , basic_auth => {UserName, Token}},
  Url = "/messages/" ++ integer_to_list(cnf_message:id(TopMsg)),
  {ok, Response} = cnf_test_utils:api_call(get, Url, Header),
  #{status_code := 200} = Response,
  Config.

-spec test_handle_delete_ok(config()) ->  config().
test_handle_delete_ok(Config) ->
  UserName = "Doge test_handle_delete_ok",
  Passsword = "passsword",
  Email = "email@email.net",
  Mgs = "Such message, very delete",
  User = cnf_user_repo:register(UserName, Passsword, Email),
  Session = cnf_session_repo:register(cnf_user:id(User)),
  Token = binary_to_list(cnf_session:token(Session)),
  UserId = cnf_user:id(User),
  Content = cnf_content_repo:register("http://inaka.net/del", UserId),
  ContentId = cnf_content:id(Content),
  TopMsg = cnf_message_repo:write_top(ContentId, Mgs, UserId),
  Header = #{ <<"Content-Type">> => <<"application/json">>
            , basic_auth => {UserName, Token}},
  Url = "/messages/" ++  integer_to_list(cnf_message:id(TopMsg)),
  {ok, Response} = cnf_test_utils:api_call(delete, Url, Header),
  #{status_code := 204} = Response,
  Config.

-spec test_get_qs_all_msg_content(config()) -> config().
test_get_qs_all_msg_content(Config) ->
  UserName = "Doge get_query_string_ok",
  Passsword = "passsword",
  Email = "email@email.net",
  Mgs = "Such message, very argument",
  User = cnf_user_repo:register(UserName, Passsword, Email),
  Session = cnf_session_repo:register(cnf_user:id(User)),
  Token = binary_to_list(cnf_session:token(Session)),
  UserId = cnf_user:id(User),
  Content = cnf_content_repo:register("http://inaka.net/qs", UserId),
  ContentId = cnf_content:id(Content),
  MsgList =
    [cnf_message_repo:write_top(ContentId, Mgs, UserId)
    || _ <- lists:seq(1, 5)],
  [cnf_message_repo:write_reply(ContentId, cnf_message:id(RplyId), Mgs, UserId)
  || RplyId <- MsgList],
  Header = #{ <<"Content-Type">> => <<"application/json">>
            , basic_auth => {UserName, Token}},
  Url = "/messages/?all_msg_content=" ++  integer_to_list(ContentId),
  {ok, Response} = cnf_test_utils:api_call(get, Url, Header),
  #{status_code := 200} = Response,
  #{body := JsonBodyResp} = Response,
  BodyRespList = jiffy:decode(JsonBodyResp, [return_maps]),
  Fun = fun(MsgMap) ->
         #{<<"message_text">> := Message} = MsgMap,
         Message = list_to_binary(Mgs)
        end,
  ok = lists:foreach(Fun, BodyRespList),
  Config.

-spec test_get_qs_all_rply_content(config()) -> config().
test_get_qs_all_rply_content(Config) ->
  UserName = "Doge all_rply_content",
  Passsword = "passsword",
  Email = "email@email.net",
  Mgs = "Such message, very argument",
  ReplayMgs = "Such message, very Replay",
  User = cnf_user_repo:register(UserName, Passsword, Email),
  Session = cnf_session_repo:register(cnf_user:id(User)),
  Token = binary_to_list(cnf_session:token(Session)),
  UserId = cnf_user:id(User),
  Content = cnf_content_repo:register("http://inaka.net/all_rply", UserId),
  ContId = cnf_content:id(Content),
  TopMsg = cnf_message_repo:write_top(ContId, Mgs, UserId),
  ReplayId = cnf_message:id(TopMsg),
  [cnf_message_repo:write_reply(ContId, ReplayId, ReplayMgs, UserId)
    || _ <- lists:seq(1, 5)],
  Header = #{ <<"Content-Type">> => <<"application/json">>
            , basic_auth => {UserName, Token}},
  Url = "/messages/?all_rply_content=" ++  integer_to_list(ContId),
  {ok, Response} = cnf_test_utils:api_call(get, Url, Header),
  #{status_code := 200} = Response,
  #{body := JsonBodyResp} = Response,
  BodyRespList = jiffy:decode(JsonBodyResp, [return_maps]),
  Fun = fun(MsgMap) ->
         #{<<"message_text">> := Message} = MsgMap,
         Message = list_to_binary(ReplayMgs)
        end,
  ok = lists:foreach(Fun, BodyRespList),
  Config.

-spec test_get_qs_top_msg_content(config()) -> config().
test_get_qs_top_msg_content(Config) ->
  UserName = "Doge top_msg_content",
  Passsword = "passsword",
  Email = "email@email.net",
  TopMgs = "Such message, very TOP",
  ReplayMgs = "Such message, very Replay",
  User = cnf_user_repo:register(UserName, Passsword, Email),
  Session = cnf_session_repo:register(cnf_user:id(User)),
  Token = binary_to_list(cnf_session:token(Session)),
  UserId = cnf_user:id(User),
  Content = cnf_content_repo:register("http://inaka.net/top_msg", UserId),
  ContId = cnf_content:id(Content),
  MsgList =
    [cnf_message_repo:write_top(ContId, TopMgs, UserId)
    || _ <- lists:seq(1, 5)],
  [cnf_message_repo:write_reply(ContId
                                , cnf_message:id(RplyId)
                                , ReplayMgs
                                , UserId)
  || RplyId <- MsgList],
  Header = #{ <<"Content-Type">> => <<"application/json">>
            , basic_auth => {UserName, Token}},
  Url = "/messages/?top_msg_content=" ++  integer_to_list(ContId),
  {ok, Response} = cnf_test_utils:api_call(get, Url, Header),
  #{status_code := 200} = Response,
  #{body := JsonBodyResp} = Response,
  BodyRespList = jiffy:decode(JsonBodyResp, [return_maps]),
  Fun = fun(MsgMap) ->
         #{<<"message_text">> := Message} = MsgMap,
         Message = list_to_binary(TopMgs)
        end,
  ok = lists:foreach(Fun, BodyRespList),
  Config.


-spec test_get_qs_all_msg_user(config()) -> config().
test_get_qs_all_msg_user(Config) ->
  UserName = "Doge all_msg_user",
  AnotherUserName = "Another Doge",
  Passsword = "passsword",
  Email = "email@email.net",
  TopMgs = "Such message, very TOP",
  ReplayMgs = "Such message, very Replay",
  User = cnf_user_repo:register(UserName, Passsword, Email),
  AnotherUser = cnf_user_repo:register(AnotherUserName, Passsword, Email),
  Session = cnf_session_repo:register(cnf_user:id(User)),
  Token = binary_to_list(cnf_session:token(Session)),
  UserId = cnf_user:id(User),
  AnotherUserId = cnf_user:id(AnotherUser),
  Content = cnf_content_repo:register("http://inaka.net/all_msg_user", UserId),
  ContId = cnf_content:id(Content),
  MsgList =
    [cnf_message_repo:write_top(ContId, TopMgs, UserId)
    || _ <- lists:seq(1, 5)],
  [cnf_message_repo:write_reply(ContId
                                , cnf_message:id(RplyId)
                                , ReplayMgs
                                , AnotherUserId)
  || RplyId <- MsgList],
  Header = #{ <<"Content-Type">> => <<"application/json">>
            , basic_auth => {UserName, Token}},
  Url = "/messages/?all_msg_user=" ++  integer_to_list(UserId),
  {ok, Response} = cnf_test_utils:api_call(get, Url, Header),
  #{status_code := 200} = Response,
  #{body := JsonBodyResp} = Response,
  BodyRespList = jiffy:decode(JsonBodyResp, [return_maps]),
  Fun = fun(MsgMap) ->
         #{<<"user_id">> := Id} = MsgMap,
         Id = UserId
        end,
  ok = lists:foreach(Fun, BodyRespList),
  Config.


-spec test_get_qs_sort_by_score(config()) -> config().
test_get_qs_sort_by_score(Config) ->
  UserName = "Doge sort_by_score",
  Passsword = "passsword",
  Email = "email@email.net",
  Mgs = "Such message, very Popular",
  User = cnf_user_repo:register(UserName, Passsword, Email),
  Session = cnf_session_repo:register(cnf_user:id(User)),
  Token = binary_to_list(cnf_session:token(Session)),
  UserId = cnf_user:id(User),
  Content = cnf_content_repo:register("http://inaka.net/sort_by_score", UserId),
  ContId = cnf_content:id(Content),
  [cnf_message_repo:write(
    cnf_message:score(
      cnf_message:new(ContId, undefined, Mgs, UserId), Votes))
  || Votes <- lists:seq(1, 50, 10)],
  Header = #{ <<"Content-Type">> => <<"application/json">>
            , basic_auth => {UserName, Token}},
  Url =
    "/messages/?all_msg_content=" ++
     integer_to_list(ContId) ++
     "&sort_by_score=" ++
     "true",
  {ok, Response} = cnf_test_utils:api_call(get, Url, Header),
  #{status_code := 200} = Response,
  #{body := JsonBodyResp} = Response,
  BodyRespList = jiffy:decode(JsonBodyResp, [return_maps]),
  FunOrder = fun(M1, M2) ->
            maps:get(<<"score">>, M1) >= maps:get(<<"score">>, M2)
         end,
  OrderedRespList = lists:sort(FunOrder, BodyRespList),
  OrderedRespList = BodyRespList,
  Config.

-spec test_get_qs_sort_created_at(config()) -> config().
test_get_qs_sort_created_at(Config) ->
  UserName = "Doge sort_created_at",
  Passsword = "passsword",
  Email = "email@email.net",
  Mgs = "Such message, very Popular",
  User = cnf_user_repo:register(UserName, Passsword, Email),
  Session = cnf_session_repo:register(cnf_user:id(User)),
  Token = binary_to_list(cnf_session:token(Session)),
  UserId = cnf_user:id(User),
  Content = cnf_content_repo:register("http://inaka.net/created_at", UserId),
  ContId = cnf_content:id(Content),
  [cnf_message_repo:write(
    cnf_message:created_at(
      cnf_message:new( ContId
                     , undefined
                     , Mgs
                     , UserId)
      , {{Year, 5, 29}, {21, 6, 20}}))
  || Year <- lists:seq(2000, 2060, 10)],
  Header = #{ <<"Content-Type">> => <<"application/json">>
            , basic_auth => {UserName, Token}},
  Url =
    "/messages/?all_msg_content=" ++
    integer_to_list(ContId) ++
    "&sort_created_at=" ++
    "true",
  {ok, Response} = cnf_test_utils:api_call(get, Url, Header),
  #{status_code := 200} = Response,
  #{body := JsonBodyResp} = Response,
  BodyRespList = jiffy:decode(JsonBodyResp, [return_maps]),
  FunOrder =
    fun(M1, M2) ->
            maps:get(<<"created_at">>, M1) >= maps:get(<<"created_at">>, M2)
    end,
  OrderedRespList = lists:sort(FunOrder, BodyRespList),
  OrderedRespList = BodyRespList,
  Config.

-spec test_post_message_ok(config()) -> config().
test_post_message_ok(Config) ->
  UserName = "Doge Top Post",
  ReplyerUserName = "Doge Rply Top Post",
  Passsword = "passsword",
  Email = "email@email.net",
  Mgs = "Such message, very Popular",
  ReplayMgs = "Such message, very Replay",
  User = cnf_user_repo:register(UserName, Passsword, Email),
  UserId = cnf_user:id(User),
  ReplyerUser = cnf_user_repo:register(ReplyerUserName, Passsword, Email),
  ReplyerUserId = cnf_user:id(ReplyerUser),

  Session = cnf_session_repo:register(ReplyerUserId),
  Token = binary_to_list(cnf_session:token(Session)),

  Content = cnf_content_repo:register("http://lol.net/post", cnf_user:id(User)),
  ContentId = cnf_content:id(Content),
  MsgTop = cnf_message_repo:write_top(ContentId, Mgs, UserId),
  MsgTopId = cnf_message:id(MsgTop),
  Header = #{ <<"Content-Type">> => <<"application/json">>
            , basic_auth => {ReplyerUserName, Token}},
  Body =
  #{ in_reply_to => MsgTopId
    , message => ReplayMgs
    , content => ContentId
  },
  JsonBody = jiffy:encode(Body),
  {ok, Response} =
  cnf_test_utils:api_call(post, "/messages", Header,  JsonBody),
  #{status_code := 200} = Response,
  #{body := JsonBodyResp} = Response,
  BodyRespList = jiffy:decode(JsonBodyResp, [return_maps]),
  BodyRespList#{ <<"content_id">> := ContentId
               , <<"message_text">> := ReplayMgs
               , <<"user_id">> := ReplyerUserId
               },
  Config.
