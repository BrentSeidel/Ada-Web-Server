with http;
with html;
package body internal is

   --
   --  Return the count of transactions as an xml message
   --
   procedure xml_count(s : GNAT.Sockets.Stream_Access;
                       h : web_common.params.Map;
                       p : web_common.params.Map) is
   begin
      http.ok(s, "application/xml");
      String'Write(s, "<xml><counter>" & Integer'Image(web_common.task_counter.read) &
                     "</counter></xml>" & CRLF);
   end xml_count;

   --
   --  Display the configuration data as a table.
   --
   procedure html_show_config(s : GNAT.Sockets.Stream_Access;
                              h : web_common.params.Map;
                              p : web_common.params.Map) is
      index : web_common.dictionary.Cursor := web_common.dictionary.First(web_common.directory);
      el : web_common.element;
   begin
      http.ok(s, "text/html");
      html.html_head(s, "Page Condfiguration List", "Style");
      String'Write(s, "<p>Table showing the page configuration list</p>" & CRLF);
      String'Write(s, "<table>" & CRLF);
      String'Write(s, "<tr><th>Key</th><th>File Name</th><th>MIME Type</th></tr></tr>" & CRLF);
      --
      --  Start table and populate it by iterating over directory
      --
      while (web_common.dictionary.Has_Element(index)) loop
         el := web_common.dictionary.Element(index);
         String'Write(s, "<tr><td>" & web_common.dictionary.Key(index) & "</td><td>" &
                        Ada.Strings.Unbounded.To_String(el.file) & "</td><td>" &
                        Ada.Strings.Unbounded.To_String(el.mime) & "</td></tr>" & CRLF);
         web_common.dictionary.Next(index);
      end loop;
      String'Write(s, "</table>" & CRLF);
      html.html_end(s, "footer.html");
   end html_show_config;
   --
   --  Display information sent in a form
   --
   procedure target(s : GNAT.Sockets.Stream_Access;
                    h : web_common.params.Map;
                    p : web_common.params.Map) is
      index : web_common.params.Cursor;
   begin
      http.ok(s, "text/html");
      html.html_head(s, "Form Parameters", "Style");
      String'Write(s, "<p>Table showing the parameters submitted in a form</p>" & CRLF);
      --
      --  Write a table for the headers
      --
      String'Write(s, "<h2>Headers</h2>" & CRLF);
      String'Write(s, "<table>" & CRLF);
      String'Write(s, "<tr><th>Header</th><th>Value</th></tr></tr>" & CRLF);
      index := web_common.params.First(h);
      while (web_common.params.Has_Element(index)) loop
         String'Write(s, "<tr><td>" & web_common.params.Key(index) & "</td><td>" &
                        web_common.params.Element(index) & "</td></tr>" & CRLF);
         web_common.params.Next(index);
      end loop;
      String'Write(s, "</table>" & CRLF);
      --
      -- Write a table for the parameters
      --
      String'Write(s, "<h2>Form Parameters</h2>" & CRLF);
      String'Write(s, "<table>" & CRLF);
      String'Write(s, "<tr><th>Key</th><th>Value</th></tr></tr>" & CRLF);
      index := web_common.params.First(p);
      while (web_common.params.Has_Element(index)) loop
         String'Write(s, "<tr><td>" & web_common.params.Key(index) & "</td><td>" &
                        web_common.params.Element(index) & "</td></tr>" & CRLF);
         web_common.params.Next(index);
      end loop;
      String'Write(s, "</table>" & CRLF);
      String'Write(s, "<h2>Headers</h2>" & CRLF);
      html.html_end(s, "footer.html");
   end target;
   --
   --  Request that the configuration file be reloaded.
   --
   procedure html_reload_config(s : GNAT.Sockets.Stream_Access;
                                h : web_common.params.Map;
                                p : web_common.params.Map) is
   begin
      http.ok(s, "text/html");
      html.html_head(s, "Reload Requested", "Style");
      String'Write(s, "<h1>Reload Request</h1>");
      String'Write(s, "<p>Configuration file reload request submitted.</p>" & CRLF);
      html.html_end(s, "footer.html");
      web_common.reload_configuration := True;
   end html_reload_config;

end;
