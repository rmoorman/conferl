-module(cnf_default_handler).

-export([init/3]).
-export([rest_init/2]).
-export([rest_terminate/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).
-export([forbidden/2]).

%% cowboy

init(_Transport, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
  {ok, Req, #{}}.

rest_terminate(_Req, _State) ->
  ok.

content_types_accepted(Req, State) ->
  {_Method, Req1} = cowboy_req:method(Req),
  {[{{<<"application">>, <<"json">>, '*'}, handle_post}], Req1, State}.

content_types_provided(Req, State) ->
  {[{{<<"application">>, <<"json">>, '*'}, handle_get}], Req, State}.

forbidden(Req, State) ->
  {false, Req, State}.
