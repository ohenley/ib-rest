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

with ada.containers.indefinite_hashed_maps;
with ada.characters.handling;
with ada.strings.fixed;
-------------------------------------------
with ib_ada;

use ada.containers;
use ada.characters.handling;
use ada.strings.fixed;
-------------------------------------------
use ib_ada;

package body ib_json is

   function last_item (i : string; last : boolean) return string is
   begin
      if not last then
         return i & ", ";
      end if;
      return i;
   end;


   function jwrt (k : string; v : integer; last : boolean) return string is
      j : string := """" & k & """ : " & trim(v'image, ada.strings.left);
   begin
      return last_item(j, last);
   end;

   function jwrt (k : string; v : safe_float; last : boolean) return string is
      j : string := """" & k & """ : " & trim(v'image, ada.strings.left);
   begin
      return last_item(j, last);
   end;

   function jwrt (k : string; v : boolean; last : boolean) return string is
      j : string := """" & k & """ : " & trim(v'image, ada.strings.left);
   begin
      return last_item(j, last);
   end;

   function jwrt (k : string; v : unbounded_string; last : boolean) return string is
      v_str : string := +v;
      j : string := """" & k & """ : """ & v_str & """";
   begin
      return last_item(j, last);
   end;


   function jwrt (k : string; v: contract_type; last : boolean) return string is
      j : string :=
        """" & k & """ : {" &
        jwrt ("contract_id", v.contract_id, false) &
        jwrt ("symbol", v.symbol, false) &
        jwrt ("security", +v.security'image, false) &
        jwrt ("exchange", +v.exchange'image, false) &
        jwrt ("currency", +v.currency'image, true) &
        "}";
   begin
      return last_item (j, last);
   end;

   function jwrt (k : string; v: position_type; last : boolean) return string is
      j : string :=
        """" & k & """ : {" &
        jwrt ("contract", v.contract, false) &
        jwrt ("open_value", safe_float(v.quantity) * v.average_cost, false) &
        jwrt ("quantity", v.quantity, false) &
        jwrt ("average_cost", v.average_cost, false) &
        jwrt ("unrealized_profit", v.unrealized_profit, true) &
        "}";
   begin
      return last_item (j, last);
   end;

   function jwrt (k : string; v: summary_type; last : boolean) return string is
      j : string :=
        """" & k & """ : {" &
        jwrt ("value", v.value, false) &
        jwrt ("currency", +v.currency'image, true) &
        "}";
   begin
      return last_item (j, last);
   end;

   function jwrt (k : string; v: order_type; last : boolean) return string is
      j : string :=
        """" & k & """ : {" &
        jwrt ("side", +v.side'image, false) &
        jwrt ("time_in_force", +v.time_in_force'image, false) &
        jwrt ("at_price_type", +v.at_price_type'image, false) &
        jwrt ("quantity", +v.quantity'image, true) &
        "}";
   begin
      return last_item (j, last);
   end;


   function jwrt (v: open_order_type; last : boolean) return string is
      j : string :=
        "{" &
        jwrt ("request_id", +v.request_id, false) &
        jwrt ("order", v.order, false) &
        jwrt ("contract", v.contract, true) &
        "}";
   begin
      return last_item (j, last);
   end;

   function jwrt (k : string; v: open_order_vector.vector; last : boolean) return string is
      j : unbounded_string;
      counter : count_type := 0;
   begin
      append(j, """" & k & """ : [");
      for oo of v loop
         counter := counter + 1;
         if counter = v.length then
            append(j, jwrt (oo, true));
         else
            append(j, jwrt (oo, false));
         end if;
      end loop;
      append(j, "]");
      return last_item(+j, last);
   end;

   function jwrt (k : string; v: position_map.map; last : boolean) return string is
      j : unbounded_string;
      counter : count_type := 0;
   begin
      append(j, """" & k & """ : {");
      for pos in v.iterate loop
         counter := counter + 1;
         if counter = v.length then
            append(j, jwrt (+position_map.key(pos), v(pos), true));
         else
            append(j, jwrt (+position_map.key(pos), v(pos), false));
         end if;
      end loop;
      append(j, "}");
      return last_item(+j, last);
   end;

   function jwrt (k : string; v: summary_map.map; last : boolean) return string is
      j : unbounded_string;
      counter : count_type := 0;
   begin
      append(j, """" & k & """ : {");
      for summary in v.iterate loop
         counter := counter + 1;
         if counter = v.length then
            append(j, jwrt (to_lower(summary_map.key(summary)'image), v(summary), true));
         else
            append(j, jwrt (to_lower(summary_map.key(summary)'image), v(summary), false));
         end if;
      end loop;
      append(j, "}");
      return last_item(+j, last);
   end;

   function jwrt (k : string; v: act_type; last : boolean) return string is
      j : string :=
        """" & k & """ : {" &
        jwrt ("open_orders", v.open_orders, false) &
        jwrt ("positions", v.positions, false) &
        jwrt ("summaries", v.summaries, true) &
        "}";
   begin
      return last_item (j, last);
   end;

   function jwrt (k : string; v: account_map.map; last : boolean) return string is
      j : unbounded_string;
      counter : count_type := 0;
   begin
      append(j, """" & k & """ : {");
      for id in v.iterate loop
         counter := counter + 1;
         if counter = v.length then
            append(j, jwrt (+account_map.key(id), v(id), true));
         else
            append(j, jwrt (+account_map.key(id), v(id), false));
         end if;
      end loop;
      append(j, "}");
      return last_item(+j, last);
   end;

end ib_json;

