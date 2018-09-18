with Ada.Text_IO;
with bbs.text;
with bbs.binary;
with bbs.http;
use type bbs.http.request_type;

package body bbs.web_server is
   --
   --  This is the web server.  In initializes the network interface and enters
   --  an infinite loop processing requests.
   --
   procedure server(internals : bbs.web_common.proc_tables.Map;
                    config_name : String;
                    port : GNAT.Sockets.Port_Type) is
      local : GNAT.Sockets.Sock_Addr_Type;
      sock1 : GNAT.Sockets.Socket_Type;  -- Socket for receiving requests
      socket : GNAT.Sockets.Socket_Type; -- Socket for responding.
      handlers : array (1 .. num_handlers) of request_handler;
      handler_index : Natural := 1;
   begin
      bbs.web_common.load_directory(config_name);
      internal_map := internals;
      --
      --  Do a bunch of initialization stuff.  We want to listen on any
      --  interface to the specified port.  The socket is IPv4 TCP/IP.
      --
      local.Addr := GNAT.Sockets.Any_Inet_Addr;
      local.Port := port;
      GNAT.Sockets.Create_Socket(sock1, GNAT.Sockets.Family_Inet,
                                 GNAT.Sockets.Socket_Stream);
      GNAT.Sockets.Set_Socket_Option(sock1, GNAT.Sockets.Socket_Level,
                                     (GNAT.Sockets.Reuse_Address, True));
      GNAT.Sockets.Bind_Socket(sock1, local);
      --
      --  Once the socket is configured.  Listen on it and accept a connection.
      --  once the connection is made, read from it and write back a response.
      --  Then close the sockets and exit.
      --
      GNAT.Sockets.Listen_Socket(sock1);
      loop
         if bbs.web_common.reload_configuration then
            bbs.web_common.load_directory("config.txt");
            bbs.web_common.reload_configuration := False;
         end if;
         GNAT.Sockets.Accept_Socket(sock1, socket, local);
         bbs.web_common.counter := bbs.web_common.counter + 1;
         --
         --  Handlers contains a array of num_handlers tasks.  As requests come
         --  in, they are assigned to tasks in round-robin fashon.  This means
         --  that it is possible for a task that takes a long time to process
         --  to eventually delay serving of other tasks.  With a little more
         --  complexity, it would be possible to change this to use the next
         --  available task.  In most cases, tasks should complete quickly and
         --  this should not be a big problem.
         --
         handlers(handler_index).start(socket);
         handler_index := handler_index + 1;
         if handler_index > num_handlers then
            handler_index := 1;
         end if;
      end loop;
      --
      --  Close the sockets and exit.  This will be done when some sort of
      --  shutdown signal is received to exit the loop above.  This is not
      --  yet implemented
      --
--      GNAT.Sockets.Close_Socket(sock1);
--      Ada.Text_IO.Put_Line("Done.");
   end server;
   --
   --  Handle the details of the http request.  This is done as a task.  Once a
   --  network connection is made, the stream for that connection is handed off
   --  to a task which processes the request, runs to completion, and then
   --  exits.
   --
   task body request_handler is
      item : Ada.Strings.Unbounded.Unbounded_String;
      headers : bbs.web_common.params.Map;
      req : bbs.http.request_type;
      el : bbs.web_common.element;
      param : bbs.web_common.params.Map;
      s : GNAT.Sockets.Stream_Access;
      sock : GNAT.Sockets.Socket_Type;
      exit_flag : Boolean := False;
   begin
      loop
         select
            accept start(socket : GNAT.Sockets.Socket_Type) do
               sock := socket;
            end start;
         or
            accept end_task do
               exit_flag := true;
            end end_task;
         end select;
         exit when exit_flag;
         bbs.web_common.task_counter.increment;
         s := GNAT.Sockets.Stream(sock);
         --
         --  First read the HTTP headers.  These will need to be parsed to
         --  determine what is being requested.  Both GET and POST requests
         --  should be handled.  POST requests will need to be able to handle
         --  passed parameters so that forms can be processed.
         --
         param.Clear;
         headers.Clear;
         bbs.http.read_headers(s, req, item, headers, param);
         --
         --  Check the request type.  If the type is Other, a request type not
         --  implemented response has already been sent.
         --
         case req is
            when bbs.http.GET =>
               --
               --  Check if the requested item is in the directory.
               --
               if bbs.web_common.directory.Contains(Ada.Strings.Unbounded.To_String(item)) then
                  el := bbs.web_common.directory.Element(Ada.Strings.Unbounded.To_String(item));
                  declare
                     name : constant String := Ada.Strings.Unbounded.To_String(el.file);
                     mime : constant String := Ada.Strings.Unbounded.To_String(el.mime);
                  begin
                     --
                     --  The following mime types should be supported:
                     --  * application/javascript
                     --  * application/xml
                     --  * image/jpeg
                     --  * image/png
                     --  * image/svg+xml
                     --  * text/css
                     --  * text/html
                     --  * text/plain
                     --
                     --  Note that a pseudo type "internal" is also supported.
                     --  This indicates that the item is procedurally generated
                     --  and the procedure will be responsible for generating
                     --  the proper types.  Dispatch to the proper procedure
                     --  will be done based on the requested item.
                     --
                     if (mime = "text/html") or (mime = "text/plain") or
                       (mime = "text/css") or (mime = "application/javascript") or
                       (mime = "application/xml") or (mime = "image/svg+xml") then
                        --
                        --  Send an text type file with the proper mime type.
                        --
                        bbs.text.send_file_with_headers(s, mime, name);
                     elsif (mime = "image/jpeg") or (mime = "image/png") then
                        --
                        --  Send a binary file with the proper mime type.
                        --
                        bbs.binary.send_file_with_headers(s, mime, name);
                     elsif mime = "internal" then
                        if internal_map.Contains(name) then
                           internal_map.Element(name)(s, headers, param);
                        end if;
                     else
                        --
                        --  If the mime type is unrecognized, it is an internal
                        --  error.
                        --
                        bbs.http.internal_error(s, mime);
                     end if;
                  end;
               else
                  bbs.http.not_found(s, Ada.Strings.Unbounded.To_String(item));
               end if;
            when bbs.http.POST =>
               --
               --  Post requests will only work on internal type files.
               --
               --  Check if the requested item is in the directory.
               --
               if bbs.web_common.directory.Contains(Ada.Strings.Unbounded.To_String(item)) then
                  el := bbs.web_common.directory.Element(Ada.Strings.Unbounded.To_String(item));
                  declare
                     name : constant String := Ada.Strings.Unbounded.To_String(el.file);
                     mime : constant String := Ada.Strings.Unbounded.To_String(el.mime);
                  begin
                     if mime = "internal" then
                        if internal_map.Contains(name) then
                          internal_map.Element(name)(s, headers, param);
                        end if;
                     else
                        --
                        --  If the mime type is unrecognized, it is an internal
                        --  error.
                        --
                        bbs.http.internal_error(s, mime);
                     end if;
                  end;
               else
                  bbs.http.not_found(s, Ada.Strings.Unbounded.To_String(item));
               end if;
            when others => -- Handled in HTTP package
               null;
         end case;
         --
         --  Close the session socket
         --
         GNAT.Sockets.Close_Socket(sock);
         bbs.web_common.task_counter.decrement;
      end loop;
   end request_handler;

end bbs.web_server;
