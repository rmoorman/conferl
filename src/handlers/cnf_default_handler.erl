-module(cnf_default_handler).

-export(
   [
    init/3,
    rest_init/2,
    rest_terminate/2,
    content_types_accepted/2,
    content_types_provided/2,
    forbidden/2
   ]
  ).

%% cowboy
init(_Transport, _Req, _Opts) ->
  lager:info("Init "),
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
  lager:info("rest_init"),
  {ok, Req, #{}}.

rest_terminate(_Req, _State) ->
  ok.

content_types_accepted(Req, State) ->
  {Method, Req1} = cowboy_req:method(Req),

  HandleMethod = case Method of
                   <<"PUT">> ->
                     handle_put;
                   <<"POST">> ->
                     handle_post;
                   <<"PATCH">> ->
                     handle_patch
                 end,

  HandleMultipartMethod = case Method of
                            <<"PUT">> ->
                              handle_multipart_put;
                            <<"POST">> ->
                              handle_multipart_post;
                            <<"PATCH">> ->
                              handle_multipart_patch
                          end,
  {[
    {{<<"application">>, <<"json">>, '*'}, HandleMethod},
    {<<"application/x-www-form-urlencoded">>, HandleMethod},
    {{<<"multipart">>, <<"form-data">>, '*'}, HandleMultipartMethod}
   ],
   Req1, State}.

content_types_provided(Req, State) ->
  {[{{<<"application">>, <<"json">>, []}, handle_get}], Req, State}.

forbidden(Req, State) ->
  {false, Req, State}.
