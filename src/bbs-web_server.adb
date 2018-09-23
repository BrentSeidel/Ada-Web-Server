with Ada.Text_IO;
with bbs.files;
--with bbs.binary;
with bbs.http;
use type bbs.http.request_type;

package body bbs.web_server is
   package ASU renames Ada.Strings.Unbounded; -- not the school
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
      internal_map : constant bbs.web_common.proc_tables.Map := internals;
      directory : bbs.web_common.dictionary.Map;
   begin
      --
      --  Do a bunch of initialization stuff.  We want to listen on any
      --  interface to the specified port.  The socket is IPv4 TCP/IP.
      --
      bbs.web_common.load_directory(config_name, directory);
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
         --
         --  Check if the configuration needs to be reloaded.
         --
         if bbs.web_common.reload_configuration then
            bbs.web_common.load_directory("config.txt", directory);
            bbs.web_common.reload_configuration := False;
         end if;
         --
         --  This call blocks until a connection request comes in.  Once a
         --  request comes, increment the counter of requests handled.
         --
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
         if (debug) then
            Ada.Text_IO.Put_Line(Integer'Image(bbs.web_common.counter) & " requests serviced, " &
                                   Integer'Image(bbs.web_common.task_counter.read) &
                                   " active tasks.");
            Ada.Text_IO.Put_Line("Using server index " & Natural'Image(handler_index));
         end if;
         handlers(handler_index).start(socket, internal_map, directory);
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
   --  to a task which processes the request and then waits for the next one.
   --
   task body request_handler is
      internal_map : bbs.web_common.proc_tables.Map;
      directory : bbs.web_common.dictionary.Map;
      sock : GNAT.Sockets.Socket_Type;
      item : ASU.Unbounded_String;
      req : bbs.http.request_type;
      headers : bbs.web_common.params.Map;
      el : bbs.web_common.element;
      param : bbs.web_common.params.Map;
      s : GNAT.Sockets.Stream_Access;
      exit_flag : Boolean := False;
   begin
      loop
         select
            accept start(socket : GNAT.Sockets.Socket_Type;
                         internals : bbs.web_common.proc_tables.Map;
                         dir : bbs.web_common.dictionary.Map) do
               sock := socket;
               internal_map := internals;
               directory := dir;
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
         bbs.http.read_headers(s, req, item, headers, param, directory);
         --
         --  Check the request type.  If the type is other than GET or POST, a
         --  response has already been sent.
         --
         case req is
            when bbs.http.GET =>
               --
               --  Check if the requested item is in the directory.
               --
               if directory.Contains(ASU.To_String(item)) then
                  el := directory.Element(ASU.To_String(item));
                  declare
                     name : constant String := ASU.To_String(el.file);
                     mime : constant String := ASU.To_String(el.mime);
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
                        bbs.files.send_text_with_headers(s, mime, name);
                     elsif (mime = "image/jpeg") or (mime = "image/png") then
                        --
                        --  Send a binary file with the proper mime type.
                        --
                        bbs.files.send_binary_with_headers(s, mime, name);
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
                  bbs.http.not_found(s, ASU.To_String(item));
               end if;
            when bbs.http.POST =>
               --
               --  Post requests will only work on internal type files.
               --
               --  Check if the requested item is in the directory.
               --
               if directory.Contains(ASU.To_String(item)) then
                  el := directory.Element(ASU.To_String(item));
                  declare
                     name : constant String := ASU.To_String(el.file);
                     mime : constant String := ASU.To_String(el.mime);
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
                  bbs.http.not_found(s, ASU.To_String(item));
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

   --
   --  Set and get value of debug flag;
   --
   function get_debug return Boolean is
   begin
      return debug;
   end get_debug;
   --
   procedure set_debug(f : Boolean) is
   begin
      debug := f;
   end set_debug;

end bbs.web_server;
