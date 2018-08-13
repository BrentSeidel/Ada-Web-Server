package body web_server is

   --
   -- This is the web server.  In initializes the network interface and enters
   -- an infinite loop processing requests.
   --
   procedure server is
      local : GNAT.Sockets.Sock_Addr_Type;
      server : GNAT.Sockets.Socket_Type;
      socket : GNAT.Sockets.Socket_Type;
      handler : handler_ptr;
   begin
      web_common.load_directory("config.txt");
      --
      -- Do a bunch of initialization stuff.  We want to listen on any interface
      -- to the specified port.  The socket is IPv4 TCP/IP.
      --
      local.Addr := GNAT.Sockets.Any_Inet_Addr;
      local.port := web_common.port;
      GNAT.Sockets.Create_Socket(server, GNAT.Sockets.Family_Inet,
                                 GNAT.Sockets.Socket_Stream);
      GNAT.Sockets.Set_Socket_Option(server, GNAT.Sockets.Socket_Level,
                                     (GNAT.Sockets.Reuse_Address, True));
      GNAT.Sockets.Bind_Socket(server, local);
      --
      -- Once the socket is configured.  Listen on it and accept a connection.
      -- once the connection is made, read from it and write back a response.
      -- Then close the sockets and exit.
      --
      -- This may even be put into a task so that simultaneous requests
      -- can be processed, but this is for applications that shouldn't see much
      -- traffic.
      --
      GNAT.Sockets.Listen_Socket(server);
      loop
         GNAT.Sockets.Accept_Socket(server, socket, local);
         web_common.counter := web_common.counter + 1;
         --
         -- When a request comes in create a new handler to handle the request.
         -- If multiple requests are being processed simultaneously, this means
         -- that the pointer to the earlier tasks is no longer available.  This
         -- should not be a problem in a well behaved system.  A better solution
         -- would probably be to create a pool of tasks (say, an array) and just
         -- pick the first one that is available.  This would require a loop in
         -- the task body.
         --
         handler := new request_handler;
         handler.start(socket);
      end loop;
      --
      -- Close the sockets and exit.  This will be done when some sort of shutdown
      -- signal is received to exit the loop above.
      --
--      GNAT.Sockets.Close_Socket(server);
--      Ada.Text_IO.Put_Line("Done.");
   end;
   --
   -- Handle the details of the http request.  This is done as a task.  Once a
   -- network connection is made, the stream for that connection is handed off
   -- to a task which processes the request, runs to completion, and then exits.
   --
   task body request_handler is
      item : Ada.Strings.Unbounded.Unbounded_String;
      req : http.request_type;
      el : web_common.element;
      param : web_common.params.Map;
      s : GNAT.Sockets.Stream_Access;
      sock : GNAT.Sockets.Socket_Type;
   begin
      accept start(socket : GNAT.Sockets.Socket_Type) do
         sock := socket;
      end start;
      s := GNAT.Sockets.Stream(sock);
      --
      -- First read the HTTP headers.  These will need to be parsed to
      -- determine what is being requested.  Both GET and POST requests
      -- should be handled.  POST requests will need to be able to handle
      -- passed parameters so that forms can be processed.
      --
      param.Clear;
      http.read_headers(s, req, item, param);
      --
      -- Check the request type.  If the type is Other, a request type not
      -- implemented response has already been sent.
      --
      if (req = http.GET) then
         --
         -- Check if the requested item is in the directory.
         --
         if (web_common.directory.Contains(Ada.Strings.Unbounded.To_String(item))) then
            el := web_common.directory.Element(Ada.Strings.Unbounded.To_String(item));
            declare
               name : String := Ada.Strings.Unbounded.To_String(el.file);
               mime : String := Ada.Strings.Unbounded.To_String(el.mime);
            begin
               --
               -- The following mime types should be supported:
               -- * application/javascript
               -- * application/xml
               -- * image/jpeg
               -- * image/png
               -- * image/svg+xml
               -- * text/css
               -- * text/html
               -- * text/plain
               --
               -- Note that a pseudo type "internal" is also supported.  This
               -- indicates that the item is procedurally generated and the
               -- procedure will be responsible for generating the proper types.
               -- Dispatch to the proper procedure will be done based on the
               -- requested item.
               --
               if (mime = "text/html") or (mime = "text/plain") or
                 (mime = "text/css") or (mime = "application/javascript") or
                 (mime = "application/xml") or (mime = "image/svg+xml") then
                  --
                  -- Send an text type file with the proper mime type.
                  --
                  text.send_file_with_headers(s, mime, name);
               elsif (mime = "image/jpeg") or (mime = "image/png") then
                  --
                  -- Send a binary file with the proper mime type.
                  --
                  binary.send_file_with_headers(s, mime, name);
               elsif (mime = "internal") then
                  decode_internal(s, name, param);
               else
                  --
                  -- If the mime type is unrecognized, it is an internal error.
                  --
                  http.internal_error(s, mime);
               end if;
            end;
         else
            http.not_found(s, Ada.Strings.Unbounded.To_String(item));
         end if;
      elsif (req = http.POST) then
         --
         -- Post requests will only work on internal type files.
         --
         -- Check if the requested item is in the directory.
         --
         if (web_common.directory.Contains(Ada.Strings.Unbounded.To_String(item))) then
            el := web_common.directory.Element(Ada.Strings.Unbounded.To_String(item));
            declare
               name : String := Ada.Strings.Unbounded.To_String(el.file);
               mime : String := Ada.Strings.Unbounded.To_String(el.mime);
            begin
               if (mime = "internal") then
                  decode_internal(s, name, param);
               else
                  --
                  -- If the mime type is unrecognized, it is an internal error.
                  --
                  http.internal_error(s, mime);
               end if;
            end;
         else
            http.not_found(s, Ada.Strings.Unbounded.To_String(item));
         end if;
      end if;
      --
      -- Close the session socket
      --
      GNAT.Sockets.Close_Socket(sock);
   end;
   --
   -- Simple procedure to decode internally generated pages.  It's used by both
   -- GET and POST methods and so should be common.
   --
   procedure decode_internal(s : GNAT.Sockets.Stream_Access; name : String;
                             p : web_common.params.Map) is
   begin
      --
      -- ******************************************************
      -- Customization goes here to add any internally routines
      -- to generate responses.
      --
      if (name = "counter") then
         internal.xml_count(s);
      elsif (name = "configure") then
         internal.html_show_config(s);
      elsif (name = "target") then
         internal.target(s, p);
      elsif (name = "thermometer") then
         svg.thermometer(s, p);
      elsif (name = "dial") then
         svg.dial(s, p);
      else
         http.not_implemented_int(s, name);
      end if;
   end;


end web_server;
