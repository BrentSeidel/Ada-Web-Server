package body internal is

   --
   -- Return the count of transactions as an xml message
   --
   procedure xml_count(s : GNAT.Sockets.Stream_Access) is
   begin
      http.ok(s, "application/xml");
      String'Write(s, "<xml><counter>" & Integer'Image(web_common.counter) &
                     "</counter></xml>" & CRLF);
   end;

   --
   -- Display the configuration data as a table.
   --
   procedure html_show_config(s : GNAT.Sockets.Stream_Access) is
      index : web_common.dictionary.Cursor := web_common.dictionary.First(web_common.directory);
      el : web_common.element;
   begin
      http.ok(s, "text/html");
      html.html_head(s, "Page Condfiguration List", "Style");
      String'Write(s, "<p>Table showing the page configuration list</p>" & CRLF);
      String'Write(s, "<table>" & CRLF);
      String'Write(s, "<tr><th>Key</th><th>File Name</th><th>MIME Type</th></tr></tr>" & CRLF);
      --
      -- Start table and populate it by iterating over directory
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
   end;
   --
   -- Display information sent in a form
   --
   procedure target(s : GNAT.Sockets.Stream_Access; p : web_common.params.Map) is
      index : web_common.params.Cursor := web_common.params.First(p);
   begin
      http.ok(s, "text/html");
      html.html_head(s, "Form Parameters", "Style");
      String'Write(s, "<p>Table showing the parameters submitted in a form</p>" & CRLF);
      String'Write(s, "<table>" & CRLF);
      String'Write(s, "<tr><th>Key</th><th>Value</th></tr></tr>" & CRLF);
      --
      -- Start table and populate it by iterating over directory
      --
      while (web_common.params.Has_Element(index)) loop
         String'Write(s, "<tr><td>" & web_common.params.Key(index) & "</td><td>" &
                        web_common.params.Element(index) & "</td></tr>" & CRLF);
         web_common.params.Next(index);
      end loop;
      String'Write(s, "</table>" & CRLF);
      html.html_end(s, "footer.html");
   end;

end;
