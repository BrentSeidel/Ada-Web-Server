with bbs.http;
with bbs.html;
package body bbs.internal is

   --
   --  Return the count of transactions as an xml message
   --
   procedure xml_count(s : GNAT.Sockets.Stream_Access;
                       h : bbs.web_common.params.Map;
                       p : bbs.web_common.params.Map) is
   begin
      bbs.http.ok(s, "application/xml");
      String'Write(s, "<xml><counter>" & Integer'Image(bbs.web_common.task_counter.read) &
                     "</counter></xml>" & CRLF);
   end xml_count;

   --
   --  Display information sent in a form
   --
   procedure target(s : GNAT.Sockets.Stream_Access;
                    h : bbs.web_common.params.Map;
                    p : bbs.web_common.params.Map) is
      index : bbs.web_common.params.Cursor;
   begin
      bbs.http.ok(s, "text/html");
      bbs.html.html_head(s, "Form Parameters", "Style");
      String'Write(s, "<p>Table showing the parameters submitted in a form</p>" & CRLF);
      --
      --  Write a table for the headers
      --
      String'Write(s, "<h2>Headers</h2>" & CRLF);
      String'Write(s, "<table>" & CRLF);
      String'Write(s, "<tr><th>Header</th><th>Value</th></tr></tr>" & CRLF);
      index := bbs.web_common.params.First(h);
      while (bbs.web_common.params.Has_Element(index)) loop
         String'Write(s, "<tr><td>" & bbs.web_common.params.Key(index) & "</td><td>" &
                        bbs.web_common.params.Element(index) & "</td></tr>" & CRLF);
         bbs.web_common.params.Next(index);
      end loop;
      String'Write(s, "</table>" & CRLF);
      --
      -- Write a table for the parameters
      --
      String'Write(s, "<h2>Form Parameters</h2>" & CRLF);
      String'Write(s, "<table>" & CRLF);
      String'Write(s, "<tr><th>Key</th><th>Value</th></tr></tr>" & CRLF);
      index := bbs.web_common.params.First(p);
      while (bbs.web_common.params.Has_Element(index)) loop
         String'Write(s, "<tr><td>" & bbs.web_common.params.Key(index) & "</td><td>" &
                        bbs.web_common.params.Element(index) & "</td></tr>" & CRLF);
         bbs.web_common.params.Next(index);
      end loop;
      String'Write(s, "</table>" & CRLF);
      String'Write(s, "<h2>Headers</h2>" & CRLF);
      bbs.html.html_end(s, "footer.html");
   end target;
   --
   --  Request that the configuration file be reloaded.
   --
   procedure html_reload_config(s : GNAT.Sockets.Stream_Access;
                                h : bbs.web_common.params.Map;
                                p : bbs.web_common.params.Map) is
   begin
      bbs.http.ok(s, "text/html");
      bbs.html.html_head(s, "Reload Requested", "Style");
      String'Write(s, "<h1>Reload Request</h1>");
      String'Write(s, "<p>Configuration file reload request submitted.</p>" & CRLF);
      bbs.html.html_end(s, "footer.html");
      bbs.web_common.reload_configuration := True;
   end html_reload_config;

end bbs.internal;
