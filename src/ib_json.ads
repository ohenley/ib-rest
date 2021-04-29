--  spdx-license-identifier: apache-2.0
--
--  copyright (c) 2021 ohenley <olivier.henley@gmail.com>
--
--  licensed under the apache license, version 2.0 (the "license");
--  you may not use this file except in compliance with the license.
--  you may obtain a copy of the license at
--
--      http://www.apache.org/licenses/license-2.0
--
--  unless required by applicable law or agreed to in writing, software
--  distributed under the license is distributed on an "as is" basis,
--  without warranties or conditions of any kind, either express or implied.
--  see the license for the specific language governing permissions and
--  limitations under the license.


with ada.strings.unbounded;
---------------------------
with ib_ada;

use ada.strings.unbounded;
---------------------------
use ib_ada;

package ib_json is

   function jwrt (k : string; v : integer; last : boolean) return string;
   function jwrt (k : string; v : safe_float; last : boolean) return string;
   function jwrt (k : string; v: contract_type; last : boolean) return string;
   function jwrt (k : string; v: position_type; last : boolean) return string;
   function jwrt (k : string; v: open_order_vector.vector; last : boolean) return string;
   function jwrt (k : string; v: position_map.map; last : boolean) return string;
   function jwrt (k : string; v: summary_map.map; last : boolean) return string;
   function jwrt (k : string; v: act_type; last : boolean) return string;
   function jwrt (k : string; v: account_map.map; last : boolean) return string;

end ib_json;
