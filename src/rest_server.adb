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

with ada.text_io;
with ada.strings.fixed;
with ada.strings.unbounded;
with ada.command_line;
with ada.io_exceptions;
with ada.characters.handling;
-----------------------------
with gnat.sockets;
with gnat.spitbol.patterns;
-----------------------------
with black.request;
with black.response;
-----------------------------
with ib_ada;
with ib_ada.connection;
with ib_ada.communication;
-----------------------------
with ib_json;

use ada.text_io;
use ada.strings.fixed;
use ada.strings.unbounded;
use ada.command_line;
-----------------------------
use gnat.spitbol.patterns;
-----------------------------
use ib_ada;


procedure rest_server is
   use gnat.sockets;
   listener : socket_type;

   port : integer := 0;
   gateway : ib_ada.session_type := undefined;

   function handle_command_line_arguments return boolean is
      help : string := "try: ./rest_server run port:8080 gateway:ib_paper";
      version : string := "0.1.0 : interactive broker rest_server";

      subject : unbounded_string;
      port_pattern : pattern := "port:" & arb;
      gateway_pattern : pattern := "gateway:" & arb;

      run_me : boolean := false;
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

      if not run_me or port = 0 or gateway = undefined then
         put_line (help);
         return false;
      end if;
      return true;
   end;

   function make_server (port         : in port_type;
                         mode         : in mode_type := socket_stream;
                         queue_length : in positive := 15)
                         return socket_type is
   begin
      return server : socket_type do
         create_socket (socket => server,
                        mode   => mode);
         set_socket_option (socket => server,
                            level => ip_protocol_for_ip_level,
                            option => (name    => reuse_address,
                                       enabled => true));
         bind_socket (socket  => server,
                      address => (family => family_inet,
                                  addr   => any_inet_addr,
                                  port   => port));

         if mode = socket_stream then
            listen_socket (socket => server,
                           length => queue_length);
         end if;
      end return;
   end make_server;

   result : boolean;
   resp : ib_ada.communication.resp_type;

begin
   result := handle_command_line_arguments;

   if result then
      ib_ada.connection.client.setup(gateway);

      resp := ib_ada.communication.handshake;
      resp := ib_ada.communication.start_api;

      put_line ("connected to " & gateway'image);

      listener := make_server (port_type(port));

      put_line ("listening at http://127.0.0.1:" & trim(port'image, ada.strings.left));

      loop
         declare
            connection : socket_type;
            client     : sock_addr_type;

            function format_response (status : ib_ada.communication.resp_type; data : string) return string is
               use ada.characters.handling;
               use ib_ada.communication;

               function success_error_status (status : ib_ada.communication.resp_type) return string is
               begin
                  if status.resp_id = ib_ada.communication.status then
                     return """error""";
                  end if;
                  return """success""";
               end;

               function success_status_data (status : ib_ada.communication.resp_type; data : string) return string is
               begin
                  if status.resp_id = ib_ada.communication.status then
                     return """""";
                  end if;
                  return data;
               end;

               j : string :=
                 "{" &
                 """status"" : " & success_error_status(status) & "," &
                 """data"" : " & success_status_data(status, data) & "," &
                 """request_number"" : " & trim(status.req_number'image, ada.strings.left) &
                 "}";
            begin
               return j;
            end;

            function format_fail (data : string) return string is
               j : string :=
                 "{" &
                 """status"" : ""fail""," &
                 """data"" : """ & data & """" &
                 "}";
            begin
               return j;
            end;
         begin
            accept_socket (server  => listener,
                           socket  => connection,
                           address => client);
            declare
               request : constant black.request.instance := black.request.parse_http (stream (connection));
               use black.response;
            begin
               if request.resource = "/positions" then
                  resp := ib_ada.communication.positions;
                  if request.has_parameter ("profit_and_loss") then
                     if boolean'value (request.parameter ("profit_and_loss")) then
                        resp := ib_ada.communication.profits_and_losses;
                     end if;
                  end if;

                  declare
                     json_resp : string := "{" & ib_json.jwrt ("accounts", accounts, true) & "}";
                  begin
                     instance'output (stream (connection), ok (data => format_response(resp, json_resp)));
                  end;
               elsif request.resource = "/open_orders" then
                  resp := ib_ada.communication.open_orders;
                  declare
                     json_resp : string := "{" & ib_json.jwrt ("accounts", accounts, true) & "}";
                  begin
                     instance'output (stream (connection), ok (data => format_response(resp, json_resp)));
                  end;

               elsif request.resource = "/place_order" then
                  declare
                     side : order_side_type := order_side_type'value (request.parameter ("side"));
                     symbol : string := request.parameter ("symbol");
                     quantity : integer := integer'value (request.parameter ("quantity"));
                     at_price_type : order_at_price_type := order_at_price_type'value (request.parameter ("at_price_type"));
                  begin
                     resp := ib_ada.communication.place_order (side, symbol, quantity, at_price_type);
                     instance'output (stream (connection), ok (data => format_response(resp, "{}")));
                  end;
               elsif request.resource = "/commission" then
                  declare
                     side : order_side_type := order_side_type'value (request.parameter ("side"));
                     symbol : string := request.parameter ("symbol");
                     quantity : integer := integer'value (request.parameter ("quantity"));
                     at_price_type : order_at_price_type := order_at_price_type'value (request.parameter ("at_price_type"));
                     commission : safe_float;
                  begin
                     resp := ib_ada.communication.place_fake_order (side, symbol, quantity, at_price_type);
                     commission := ib_ada.communication.get_commission (resp.req_number);
                     declare
                        json_resp : string := "{" &
                          ib_json.jwrt ("commission", commission, true) &
                          "}";
                     begin
                        instance'output (stream (connection), ok (data => format_response(resp, json_resp)));
                     end;
                  end;
               elsif request.resource = "/cancel_order" then
                  if request.has_parameter ("request_number") then
                     declare
                        request_number : integer := integer'value (request.parameter ("request_number"));
                     begin
                        resp := ib_ada.communication.cancel_order (request_number);
                        instance'output (stream (connection), ok (data => format_response(resp, "cancel order " & trim(request_number'image, ada.strings.left) & " sent.")));
                     end;
                  else
                     instance'output (stream (connection), ok (data => format_fail("request_number needed as query parameter.")));
                  end if;
               elsif request.resource = "/accounts_summary" then
                  if request.has_parameter ("tag") then
                     declare
                        tag : tag_type := tag_type'value (request.parameter ("tag"));
                     begin
                        resp := ib_ada.communication.accounts_summary (tag);
                        declare
                           json_resp : string := "{" & ib_json.jwrt ("accounts", accounts, true) & "}";
                        begin
                           instance'output (stream (connection), ok (data => format_response(resp, json_resp)));
                        end;
                     end;
                  else
                     instance'output (stream (connection), ok (data =>  format_fail("tag is invalid.") ));
                  end if;
               end if;
            end;
            close_socket (socket => connection);
         exception
            when ada.io_exceptions.end_error =>
               ib_ada.connection.client.disconnect;
         end;
      end loop;
   end if;
end;
