with ib_ada; use ib_ada;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package ib_json is

   function jwrt (k : string; v: contract_type; last : boolean) return string;
   function jwrt (k : string; v: position_type; last : boolean) return string;
   function jwrt (k : string; v: position_map.map; last : boolean) return string;
   function jwrt (k : string; v: summary_map.map; last : boolean) return string;
   function jwrt (k : string; v: act_type; last : boolean) return string;
   function jwrt (k : string; v: account_map.map; last : boolean) return string;

end ib_json;
