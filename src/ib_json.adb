--with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Hashed_Maps; use Ada.Containers;


with Ada.Characters.Handling; use  Ada.Characters.Handling;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with ib_ada; use ib_ada;

package body ib_json is

   function last_item (i : string; last : boolean) return string is
   begin
      if not last then
         return i & ", ";
      end if;
      return i;
   end;


   function jwrt (k : string; v : integer; last : boolean) return string is
      j : string := """" & k & """ : " & trim(v'image, Ada.Strings.Left);
   begin
      return last_item(j, last);
   end;

   function jwrt (k : string; v : safe_float; last : boolean) return string is
      j : string := """" & k & """ : " & trim(v'image, Ada.Strings.Left);
   begin
      return last_item(j, last);
   end;

   function jwrt (k : string; v : boolean; last : boolean) return string is
      j : string := """" & k & """ : " & trim(v'image, Ada.Strings.Left);
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
        jwrt ("pnl_unrealized", v.pnl_unrealized, false) &
        jwrt ("pnl_realized", v.pnl_realized, false) &
        jwrt ("pnl_daily", v.pnl_daily, true) &
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


   function jwrt (k : string; v: open_order_type; last : boolean) return string is
      j : string :=
        """" & k & """ : {" &
        jwrt ("request_id", +v.request_id'image, false) &
        jwrt ("order", v.order, false) &
        jwrt ("contract", v.contract, true) &
        "}";
   begin
      return last_item (j, last);
   end;

   function jwrt (k : string; v: open_order_map.map; last : boolean) return string is
      j : unbounded_string;
      counter : count_type := 0;
   begin
      append(j, """" & k & """ : {");
      for oo in v.iterate loop
         counter := counter + 1;
         if counter = v.length then
            append(j, jwrt (+open_order_map.key(oo), v(oo), true));
         else
            append(j, jwrt (+open_order_map.key(oo), v(oo), false));
         end if;
      end loop;
      append(j, "}");
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

