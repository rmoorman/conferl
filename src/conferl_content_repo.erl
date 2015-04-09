-module(conferl_content_repo).
-author('david.cao@inakanetworks.com').

-behaviour(sumo_repo).

%%% General repo functions.
-export(
  [ init/1
  , create_schema/2
  , delete/3
  , delete_all/2
  , delete_by/3
  , execute/2
  , execute/3
  , find_all/2
  , find_all/5
  , find_by/3
  , find_by/5
  , persist/2
  ]).
