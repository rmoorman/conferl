-module(cnf_default_handler).

-export([init/3]).
-export([rest_init/2]).
-export([rest_terminate/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).
-export([forbidden/2]).
-export([is_authorized_by_token/2]).
-export([is_authorized_by_password/2]).

-type state() :: #{}.
%% cowboy

init(_Transport, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
  {ok, Req, #{}}.

rest_terminate(_Req, _State) ->
  ok.

content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"json">>, '*'}, handle_post}], Req, State}.

content_types_provided(Req, State) ->
  {[{{<<"application">>, <<"json">>, '*'}, handle_get}], Req, State}.

forbidden(Req, State) ->
  {false, Req, State}.

-spec is_authorized_by_token(cowboy_req:req(), state()) ->
  {boolean() | {boolean(), binary()}, cowboy_req:req(), state()}.
is_authorized_by_token(Req, State) ->
  case cowboy_req:parse_header(<<"authorization">>, Req) of
    {ok, {<<"basic">>, {Login, Token}}, _} ->
      try
        true = cnf_session_repo:is_valid(Token),
        {true, Req, #{login => Login, token => Token}}
      catch
        _Type:_Excep -> {{false, <<"Basic realm=\"conferl\"">>}, Req, State}
      end;
    _WhenOthers ->
      {{false, <<"Basic realm=\"conferl\"">>}, Req, State}
  end.

-spec is_authorized_by_password(cowboy_req:req(), state()) ->
  {boolean() | {boolean(), binary()}, cowboy_req:req(), state()}.
is_authorized_by_password(Req, State) ->
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
