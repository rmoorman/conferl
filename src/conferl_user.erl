% 
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing,
% software distributed under the License is distributed on an
% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
% KIND, either express or implied.  See the License for the
% specific language governing permissions and limitations
% under the License.

-module(conferl_user).

-author('david.cao@inakanetworks.com').

-type user() ::
        #{
           id      => integer(),
           user    => string()
           %date
         }.


-export_type( [message/0]).

-export(sumo_doc).


 -spec register_user(Username :: iodata()
                    , Password :: iodata()
                    , Email :: iodata()) -> duplicated | conferl_users:user().

 -spec unregister_user(UserId :: integer()) -> ok.


 -spec fetch_user(UserId :: integer()) -> notfound | conferl_users:user().