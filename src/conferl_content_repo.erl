-module(conferl_content_repo).
-author('david.cao@inakanetworks.com').

%%% General repo functions.
-export(
  [ create/3
  , update/1
  , delete/3
  , delete_all/2
  , delete_by/3
  , find_all/2
  , find_all/5
  , find_by/3
  , find_by/5
  , persist/2
  ]).


-spec create( Id       :: integer()
            , Messages :: [message]
            , User     :: integer()) -> conferl_content:content().
create(  Id  ,Messages ,User ) ->
  Content = conferl_content:new( Id ,Messages , User),
  sumo:persist(conferl_content, Content).


-spec update(conferl_content:content()) -> conferl_content:content().
update( Content ) ->
  sumo:persist(conferl_content, Content).

-spec delete( conferl_content:content() ) -> integer().
delete( Content ) -> 
  Id = conferl_content:id(Content),
  sumo:delete_by(conferl_content, [{id, Id}]).

-spec delete_all() -> integer(). 
delete_all() -> sumo:delete(conferl_content, Content). 
