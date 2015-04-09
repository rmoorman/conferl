-module(conferl_content).
-author('david.cao@inakanetworks.com').

-export([]).



-type message() ::
        #{
           id => integer(),
           message => string(),
           user => string()
           %date
         }.

-type content() ::
        #{
           id => integer(),
           message => [message],
           user => string()
           %date
         }.

-export_type( [content/0
              , message/0]).

-export([ register_content/1
          , unregister_content/1
          , fetch_content/1
          , list_contents/1 ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public Api
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc functions definitions for content

-spec register_content(Url :: iodata()) -> 
  conferl_contents:content() | error.

register_content(Url) -> #{}.
%% todo

-spec unregister_content(ContentId :: integer()) -> ok | error .

unregister_content(ContentId ) ->  error .
%% todo

-spec fetch_content(ContentId :: integer()) -> 
   notfound | conferl_contents:content().

fetch_content(ContentId) ->   notfound.   
%% todo

-spec list_contents(Domain :: iodata()) -> [conferl_contents:content()].
list_contents(Domain) -> [ #{} ].
%% todo