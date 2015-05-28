-module(cnf_default_handler).

-export([init/3]).
-export([rest_init/2]).
-export([rest_terminate/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).
-export([forbidden/2]).
-export([is_authorized_by_token/2]).
-export([is_authorized_by_password/2]).
-export([is_authorized_generic/3]).

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
  is_authorized_generic(fun validation_by_token/2, Req, State).

-spec is_authorized_by_password(cowboy_req:req(), state()) ->
  {boolean() | {boolean(), binary()}, cowboy_req:req(), state()}.
is_authorized_by_password(Req, State) ->
  is_authorized_generic(fun validation_by_password/2, Req, State).

-spec validation_by_token(string(), binary()) -> map().
validation_by_token(Login, Token) ->
  true = cnf_session_repo:is_valid(Token),
  #{login => Login, token => Token}.

-spec validation_by_password(string(), string()) -> map().
validation_by_password(Login, Password) ->
  true = cnf_user_repo:is_registered(Login, Password),
  #{login => Login}.

-spec is_authorized_generic(fun(), cowboy_req:req(), state() ) ->
  {boolean() | {boolean(), binary()}, cowboy_req:req(), state()}.
is_authorized_generic(ValidationFun, Req, State) ->
  case cowboy_req:parse_header(<<"authorization">>, Req) of
    {ok, {<<"basic">>, {Login, Authentification}}, _} ->
      try
        NewState = ValidationFun(Login, Authentification),
        {true, Req, NewState}
      catch
        _Type:_Excep -> {{false, <<"Basic realm=\"conferl\"">>}, Req, State}
      end;
    _WhenOthers ->
      {{false, <<"Basic realm=\"conferl\"">>}, Req, State}
  end.

