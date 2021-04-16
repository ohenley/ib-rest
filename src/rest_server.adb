with ib_ada; use ib_ada;
with ib_ada.conn;
with ib_ada.communication;

with Ada.text_io; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.IO_Exceptions;

with GNAT.Sockets;
with GNAT.Spitbol.Patterns;  use GNAT.Spitbol.Patterns;

with black.request;
with black.response;

with ib_json;

procedure rest_server is
   use GNAT.Sockets;
   Listener : Socket_Type;

   run_me : boolean := false;
   port : integer := 0;
   gateway : ib_ada.session_type := UNDEFINED;

   function handle_command_line_arguments return boolean is
      help : string := "try: ./rest_server run port:8080 gateway:ib_paper";
      version : string := "0.1.0 : Interactive Broker rest_server";

      subject : unbounded_string;
      port_pattern : pattern := "port:" & arb;
      gateway_pattern : pattern := "gateway:" & arb;
   begin
      for i in 1 .. argument_count loop
         if i = 1 then
            if argument (i) = "run" then
               run_me := true;
            elsif argument (i) = "version" then
               put_line (version);
               exit;
            else
               exit;
            end if;
         else
            subject := +argument (i);
            if match (subject, port_pattern, "") then
               port := integer'value (+subject);
            end if;
            if match (subject, gateway_pattern, "") then
               gateway := ib_ada.session_type'value (+subject);
            end if;
         end if;
      end loop;

      if not run_me or port = 0 or gateway = UNDEFINED then
         put_line (help);
         return false;
      end if;
      return true;
   end;


   function Make_Server (Port         : in Port_Type;
                         Mode         : in Mode_Type := Socket_Stream;
                         Queue_Length : in Positive := 15)
                         return Socket_Type is
   begin
      return Server : Socket_Type do
         Create_Socket (Socket => Server,
                        Mode   => Mode);
         Set_Socket_Option (Socket => Server,
                            Level => IP_Protocol_For_IP_Level,
                            Option => (Name    => Reuse_Address,
                                       Enabled => True));
         Bind_Socket (Socket  => Server,
                      Address => (Family => Family_Inet,
                                  Addr   => Any_Inet_Addr,
                                  Port   => Port));

         if Mode = Socket_Stream then
            Listen_Socket (Socket => Server,
                           Length => Queue_Length);
         end if;
      end return;
   end Make_Server;

   result : boolean;

begin

   result := handle_command_line_arguments;

   if result then
      ib_ada.conn.client.setup(gateway);

      ib_ada.communication.handshake;
      ib_ada.communication.start_api;

      put_line ("connected to " & gateway'image);

      listener := make_server (port_type(port));

      put_line ("listening at http://127.0.0.1:" & trim(port'image, Ada.strings.left));

      loop
         declare
            Connection : Socket_Type;
            Client     : Sock_Addr_Type;
         begin
            Accept_Socket (Server  => Listener,
                           Socket  => Connection,
                           Address => Client);
            declare
               Request : constant Black.Request.Instance :=
                 Black.Request.Parse_HTTP (Stream (Connection));
               use Black.Response;
               response : unbounded_string;

            begin
               if Request.Resource = "/positions" then

                  ib_ada.communication.positions;

                  if request.has_parameter ("pnl") then
                     if boolean'value (request.Parameter ("pnl")) then
                        ib_ada.communication.pnls;
                     end if;
                  end if;

                  declare
                     json_resp : string := "{" & ib_json.jwrt ("accounts", accounts, true) & "}";
                  begin
                     Instance'Output (Stream (Connection), OK (Data => json_resp));
                  end;
               elsif request.resource = "/open_orders" then
                  ib_ada.communication.open_orders;
                  declare
                     json_resp : string := "{" & ib_json.jwrt ("accounts", accounts, true) & "}";
                  begin
                     Instance'Output (Stream (Connection), OK (Data => json_resp));
                  end;

               elsif request.resource = "/place_order" then
                  declare
                     side : order_side_type := order_side_type'value (request.parameter ("side"));
                     symbol : string := request.parameter ("symbol");
                     quantity : integer := integer'value (request.parameter ("quantity"));
                     at_price_type : order_at_price_type := order_at_price_type'value (request.parameter ("at_price_type"));
                     req_id : integer;
                  begin
                     req_id := ib_ada.communication.place_order (side, symbol, quantity, at_price_type);
                     declare
                        json_resp : string := "{" & ib_json.jwrt ("request_id", req_id, true) & "}";
                     begin
                        Instance'Output (Stream (Connection), OK (Data => json_resp));
                     end;
                  end;
               elsif request.resource = "/place_fake_order" then
                  declare
                     side : order_side_type := order_side_type'value (request.parameter ("side"));
                     symbol : string := request.parameter ("symbol");
                     quantity : integer := integer'value (request.parameter ("quantity"));
                     at_price_type : order_at_price_type := order_at_price_type'value (request.parameter ("at_price_type"));
                     req_id : integer;
                     commission : safe_float;
                  begin
                     req_id := ib_ada.communication.place_fake_order (side, symbol, quantity, at_price_type);
                     commission := ib_ada.communication.get_commission (req_id);
                     declare
                        json_resp : string := "{" &
                          ib_json.jwrt ("request_id", req_id, false) &
                          ib_json.jwrt ("commission", commission, true) &
                          "}";
                     begin
                        Instance'Output (Stream (Connection), OK (Data => json_resp));
                     end;
                  end;
               elsif request.resource = "/cancel_order" then
                  if request.has_parameter ("request_id") then
                     declare
                        request_id : integer := integer'value (request.parameter ("request_id"));
                     begin
                        ib_ada.communication.cancel_order (request_id);
                        Instance'Output (Stream (Connection), OK (Data => "{ ""success"" : ""cancel order " & trim(request_id'image, Ada.Strings.Left) & " sent.""}"));
                     end;
                  else
                     Instance'Output (Stream (Connection), OK (Data =>  "{ ""fail"" : ""request_id needed as query parameter.""}"  ));
                  end if;
               elsif Request.Resource = "/accounts_summary" then
                  if request.has_parameter ("tag") then
                     declare
                        tag : tag_type := tag_type'value (request.parameter ("tag"));
                     begin
                        ib_ada.communication.accounts_summary (tag);
                        declare
                           json_resp : string := "{" & ib_json.jwrt ("accounts", accounts, true) & "}";
                        begin
                           Instance'Output (Stream (Connection), OK (Data => json_resp));
                        end;
                     end;
                  else
                     Instance'Output (Stream (Connection), OK (Data =>  "{ ""fail"" : ""tag is invalid.""}"  ));
                  end if;
               elsif Request.Resource = "/test" then
                  ib_ada.communication.market_data ("AAPL", 265598);
                  declare
                     json_resp : string := "{ called test }";
                  begin
                     Instance'Output (Stream (Connection), OK (Data => json_resp));
                  end;
               else
                  Instance'Output (Stream (Connection), Not_Found (Resource => Request.Resource));
               end if;
            end;

            Close_Socket (Socket => Connection);
         exception
            when Ada.IO_Exceptions.End_Error =>
               ib_ada.conn.client.disconnect;
         end;
      end loop;
   end if;

end;
