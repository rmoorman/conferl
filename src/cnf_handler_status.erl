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

-module(cnf_handler_status).

-author('David Cao <david.cao@inakanetworks.com>').

-export([ handle/2
        , init/3
        , terminate/3
        ]).

init({tcp, http}, Req, _Opts) ->
  {ok, Req, undefined}.

handle(Req, State) -> 
  {ok, Req2} = cowboy_req:reply( 200
                                ,[{<<"content-type">>, <<"text/plain">>}]
                                ,<<"{status:ok, nodes:1}">>
                                ,Req),
  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.
