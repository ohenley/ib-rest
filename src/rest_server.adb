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
      help : string := "try: rest_server run port:8080 gateway:ib_paper";
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

      Listener := Make_Server (port_type(port));

      put_line ("listening at http://127.0.0.1:" & trim(port'image, Ada.Strings.Left));

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
                  declare
                     json_resp : string := "{" & ib_json.jwrt ("accounts", accounts, true) & "}";
                  begin
                     Instance'Output (Stream (Connection), OK (Data => json_resp));
                  end;
               elsif Request.Resource = "/accounts_summaries" then
                  ib_ada.communication.account_summary (ib_ada.BUYING_POWER);
                  declare
                     json_resp : string := "{" & ib_json.jwrt ("accounts", accounts, true) & "}";
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
