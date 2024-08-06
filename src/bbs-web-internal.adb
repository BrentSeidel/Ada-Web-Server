with BBS.web.http;
with BBS.web.html;
package body BBS.web.internal is

   --
   --  Return the count of transactions as an xml message
   --
   procedure xml_count(s : GNAT.Sockets.Stream_Access;
                       h : params.Map;
                       p : params.Map) is
   begin
      BBS.web.http.ok(s, "application/xml");
      String'Write(s, "<xml><tasks>" & Integer'Image(task_counter.read) &
                     "</tasks><counter>" & Integer'Image(request_counter.read) &
                     "</counter></xml>" & CRLF);
   end xml_count;

   --
   --  Display information sent in a form
   --
   procedure target(s : GNAT.Sockets.Stream_Access;
                    h : params.Map;
                    p : params.Map) is
      index : params.Cursor;
   begin
      BBS.web.http.ok(s, "text/html");
      BBS.web.html.html_head(s, "Form Parameters", "Style");
      String'Write(s, "<p>Table showing the parameters submitted in a form</p>" & CRLF);
      --
      --  Write a table for the headers
      --
      String'Write(s, "<h2>Headers</h2>" & CRLF);
      String'Write(s, "<table>" & CRLF);
      String'Write(s, "<tr><th>Header</th><th>Value</th></tr></tr>" & CRLF);
      index := params.First(h);
      while (params.Has_Element(index)) loop
         String'Write(s, "<tr><td>" & params.Key(index) & "</td><td>" &
                        params.Element(index) & "</td></tr>" & CRLF);
         params.Next(index);
      end loop;
      String'Write(s, "</table>" & CRLF);
      --
      -- Write a table for the parameters
      --
      String'Write(s, "<h2>Form Parameters</h2>" & CRLF);
      String'Write(s, "<table>" & CRLF);
      String'Write(s, "<tr><th>Key</th><th>Value</th></tr></tr>" & CRLF);
      index := params.First(p);
      while (params.Has_Element(index)) loop
         String'Write(s, "<tr><td>" & params.Key(index) & "</td><td>" &
                        params.Element(index) & "</td></tr>" & CRLF);
         params.Next(index);
      end loop;
      String'Write(s, "</table>" & CRLF);
      String'Write(s, "<h2>Headers</h2>" & CRLF);
      BBS.web.html.html_end(s, "footer.html");
   end target;
   --
   --  Request that the configuration file be reloaded.
   --
   procedure html_reload_config(s : GNAT.Sockets.Stream_Access;
                                h : params.Map;
                                p : params.Map) is
   begin
      BBS.web.http.ok(s, "text/html");
      BBS.web.html.html_head(s, "Reload Requested", "Style");
      String'Write(s, "<h1>Reload Request</h1>");
      String'Write(s, "<p>Configuration file reload request submitted.</p>" & CRLF);
      BBS.web.html.html_end(s, "footer.html");
      bbs.web.reload_configuration.set;
   end html_reload_config;
   --
   -- Set the web exit flag.
   --
   procedure html_set_exit(s : GNAT.Sockets.Stream_Access;
                           h : params.Map;
                           p : params.Map) is
   begin
      BBS.web.http.ok(s, "text/html");
      BBS.web.html.html_head(s, "Server Exiting", "Style");
      String'Write(s, "<h1>Exit Flag Set</h1>");
      String'Write(s, "Web server should be exiting.");
      BBS.web.html.html_end(s, "footer.html");
      web.exit_flag.set;
   end html_set_exit;
   --
   -- Raise an exception to test task exception handling
   --
   procedure html_raise(s : GNAT.Sockets.Stream_Access;
                        h : params.Map;
                        p : params.Map) is
   begin
      raise Program_Error;
   end;

end BBS.web.internal;
