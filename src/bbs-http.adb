with Ada.Characters.Latin_1;
with Ada.Text_IO;
package body bbs.http is

   --
   --  Return code 200 OK for normal cases
   --
   procedure ok(s : GNAT.Sockets.Stream_Access; txt: String) is
   begin
      String'Write(s, "HTTP/1.0 200 OK" & CRLF);
      String'Write(s, "Content-Type: " & txt & CRLF);
      String'Write(s, server_header);
      String'Write(s, "Connection: Close" & CRLF);
      String'Write(s, CRLF);
   end ok;
   --
   --  Return code 200 OK for OPTIONS reqest cases
   --
   procedure options_ok(s : GNAT.Sockets.Stream_Access; item : String) is
   begin
      String'Write(s, "HTTP/1.0 200 OK" & CRLF);
      if item = "*" then
         String'Write(s, "Allow: OPTIONS, GET, POST" & CRLF);
      elsif bbs.web_common.directory.Contains(item) then
         declare
            el : constant bbs.web_common.element := bbs.web_common.directory.Element(item);
            mime : constant String := Ada.Strings.Unbounded.To_String(el.mime);
         begin
            if mime = "internal" then
               String'Write(s, "Allow: OPTIONS, GET, POST" & CRLF);
            else
               String'Write(s, "Allow: OPTIONS, GET" & CRLF);
            end if;
         end;
      else
         String'Write(s, "Allow: OPTIONS" & CRLF);
      end if;
      String'Write(s, server_header);
      String'Write(s, "Content-Length: 0" & CRLF);
      String'Write(s, CRLF);
   end options_ok;
   --
   --  Return code 404 NOT FOUND for when the requested item is not in the
   --  directory.
   --
   procedure not_found(s : GNAT.Sockets.Stream_Access; item: String) is
   begin
      String'Write(s, "HTTP/1.0 404 NOT FOUND" & CRLF);
      String'Write(s, "Content-Type: text/html" & CRLF);
      String'Write(s, server_header);
      String'Write(s, "Connection: Close" & CRLF);
      String'Write(s, CRLF);
      String'Write(s, "<html><head><title>" & item & " not found</title></head>");
      String'Write(s, "<body>Item " & item & " cannot be found on the server.");
      String'Write(s, "</body></html>");
   end not_found;
   --
   --  Return code 500 INTERNAL SERVER ERROR generally when unable to open the
   --  file for the specified item.
   --
   procedure internal_error(s : GNAT.Sockets.Stream_Access; file: String) is
   begin
      String'Write(s, "HTTP/1.0 500 INTERNAL ERROR" & CRLF);
      String'Write(s, "Content-Type: text/html" & CRLF);
      String'Write(s, server_header);
      String'Write(s, "Connection: Close" & CRLF);
      String'Write(s, CRLF);
      String'Write(s, "<html><head><title>Internal Error</title></head>");
      String'Write(s, "<body>Internal error trying to serve " & file);
      String'Write(s, "</body></html>");
   end internal_error;
   --
   --  Return code 501 NOT IMPLEMENTED for any request other than GET or POST.
   --
   procedure not_implemented_req(s : GNAT.Sockets.Stream_Access; req: String) is
   begin
      String'Write(s, "HTTP/1.0 501 NOT IMPLEMENTED" & CRLF);
      String'Write(s, "Content-Type: text/html" & CRLF);
      String'Write(s, server_header);
      String'Write(s, "Connection: Close" & CRLF);
      String'Write(s, CRLF);
      String'Write(s, "<html><head><title>Request type not implemented</title></head>");
      String'Write(s, "<body>Request type " & req & " is not implemented");
      String'Write(s, "</body></html>");
   end not_implemented_req;
   --
   --  Return code 501 NOT IMPLEMENTED for a request for an internally generated
   --  item that is not yet implemented..
   --
   procedure not_implemented_int(s : GNAT.Sockets.Stream_Access; item: String) is
   begin
      String'Write(s, "HTTP/1.0 501 NOT IMPLEMENTED" & CRLF);
      String'Write(s, "Content-Type: text/html" & CRLF);
      String'Write(s, server_header);
      String'Write(s, "Connection: Close" & CRLF);
      String'Write(s, CRLF);
      String'Write(s, "<html><head><title>" & item & " not implemented</title></head>");
      String'Write(s, "<body>Item " & item & " is not yet implemented on the server.");
      String'Write(s, "</body></html>");
   end not_implemented_int;

   --
   --  Read a line from the input stream.  The line is terminated with a CR-LF.
   --  The CR-LF is stripped from the return string as it can just be assumed to
   --  be there.
   --
   function get_line_from_stream(s : GNAT.Sockets.Stream_Access)
                                 return Ada.Strings.Unbounded.Unbounded_String is
      c : Character;
      str : Ada.Strings.Unbounded.Unbounded_String;
   begin
      loop
         loop
            c := Character'Input(s);
            str := str & c;
            exit when c = Ada.Characters.Latin_1.CR;
         end loop;
         c := Character'Input(s);
         str := str & c;
         exit when c = Ada.Characters.Latin_1.LF;
      end loop;
      return Ada.Strings.Unbounded.Head(str, Ada.Strings.Unbounded.Length(str) - 2);
   end get_line_from_stream;
   --
   --  Read a specified number of characters from an input stream.  This is
   --  needed because a POST request is not terminated by CRLF.  Instead the
   --  content-length header needs to be parsed and that number of characters
   --  read at the end.
   --
   function get_data_from_stream(s : GNAT.Sockets.Stream_Access; len : Natural)
                                 return Ada.Strings.Unbounded.Unbounded_String is
      c : Character;
      str : Ada.Strings.Unbounded.Unbounded_String;
   begin
      for i in Natural range 1 .. len loop
         c := Character'Input(s);
         str := str & c;
      end loop;
      return str;
   end get_data_from_stream;
   --
   --  Read the headers from the request..
   --
   procedure read_headers(s : GNAT.Sockets.Stream_Access;
                          method : out request_type;
                          item : out Ada.Strings.Unbounded.Unbounded_String;
                          headers : in out bbs.web_common.params.Map;
                          params : in out bbs.web_common.params.Map) is
      param_string : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
      length : Natural;
      line  : Ada.Strings.Unbounded.Unbounded_String;
      req   : Ada.Strings.Unbounded.Unbounded_String;
      temp1 : Ada.Strings.Unbounded.Unbounded_String;
      temp2 : Ada.Strings.Unbounded.Unbounded_String;
      index : Natural;
   begin
      --
      --  The first line contains the request.  Parse it out.
      --
      line := get_line_from_stream(s);
      if debug_req then
         Ada.Text_IO.Put_Line(Ada.Strings.Unbounded.To_String(line));
      end if;
      index := Ada.Strings.Unbounded.Index(line, " ");
      req := Ada.Strings.Unbounded.Head(line, index - 1);
      line := Ada.Strings.Unbounded.Tail(line,
                                         Ada.Strings.Unbounded.Length(line) - index);
      if req = "CONNECT" then
         method := CONNECT;
      elsif req = "DELETE" then
         method := DELETE;
      elsif req = "GET" then
         method := GET;
      elsif req = "HEAD" then
         method := HEAD;
      elsif req = "OPTIONS" then
         method := OPTIONS;
      elsif req = "PATCH" then
         method := PATCH;
      elsif req = "POST" then
         method := POST;
      elsif req = "PUT" then
         method := PUT;
      elsif req = "TRACE" then
         method := TRACE;
      else
         method := Other;
      end if;
      --
      -- Parse out the requested item.  Don't care about the HTTP version
      --
      index := Ada.Strings.Unbounded.Index(line, " ");
      if index > 0 then
         item := Ada.Strings.Unbounded.Head(line, index - 1);
         line := Ada.Strings.Unbounded.Tail(line,
                                            Ada.Strings.Unbounded.Length(line) - index);
      else
         item := line;
         line := Ada.Strings.Unbounded.Null_Unbounded_String;
      end if;
      --
      -- Scan through the rest of the headers.  If needed, the content-length
      -- header is parsed.
      --
      loop
         line := get_line_from_stream(s);
         exit when Ada.Strings.Unbounded.Length(line) = 0;
         index := Ada.Strings.Unbounded.Index(line, " ");
         temp1 := Ada.Strings.Unbounded.Head(line, index - 1);
         temp2 := Ada.Strings.Unbounded.Tail(line,
                                            Ada.Strings.Unbounded.Length(line) - index);
         headers.Insert(Key      => Ada.Strings.Unbounded.To_String(temp1),
                        New_Item => Ada.Strings.Unbounded.To_String(temp2));
         --
         -- If this is a POST request, we need to find the Content-Length:
         -- header and determine the length.  To be strictly correct, the
         -- Content-Type: header should also be examined.
         --
         if method = POST then
            if (temp1 = "Content-Length:") then
               length := Natural'Value(Ada.Strings.Unbounded.To_String(temp2));
            end if;
         end if;
         if debug_head then
            Ada.Text_IO.Put_Line(Ada.Strings.Unbounded.To_String(line));
         end if;
      end loop;
      --
      -- Check the request method and send proper response if not implemented.
      --
      case method is
         when GET =>
            --
            -- If a GET request, check to see if parameters are attached
            --
            line := item;
            index := Ada.Strings.Unbounded.Index(line, "?");
            if index > 0 then
               item := Ada.Strings.Unbounded.Head(line, index - 1);
               param_string := Ada.Strings.Unbounded.Tail(line,
                                                  Ada.Strings.Unbounded.Length(line) - index);
            end if;
         when POST =>
            --
            -- If the method is post, the parameters will be read here.
            --
            param_string := get_data_from_stream(s, length);
         when OPTIONS =>
            options_ok(s, Ada.Strings.Unbounded.To_String(item));
         when others =>
            not_implemented_req(s, Ada.Strings.Unbounded.To_String(req));
      end case;
      --
      -- If there are parameters, process them and store them in a parameter
      -- dictionary.
      --
      if (param_string /= Ada.Strings.Unbounded.Null_Unbounded_String) then
         while (Ada.Strings.Unbounded.Length(param_string) > 0) loop
            --
            -- First split off a key value pair.  They are separated by '&'.
            --
            index := Ada.Strings.Unbounded.Index(param_string, "&");
            if index > 0 then
               temp1 := Ada.Strings.Unbounded.Head(param_string, index - 1);
               param_string := Ada.Strings.Unbounded.Tail(param_string,
                                                          Ada.Strings.Unbounded.Length(param_string) - index);
            else
               temp1 := param_string;
               param_string := Ada.Strings.Unbounded.Null_Unbounded_String;
            end if;
            --
            -- Split the key-value pair into separate key and values and store
            -- them in the params map.  At this point, the value could be URL
            -- decoded.
            --
            index := Ada.Strings.Unbounded.Index(temp1, "=");
            declare
               key : constant String := Ada.Strings.Unbounded.To_String(
                                                     Ada.Strings.Unbounded.Head(temp1, index - 1));
               value : constant String := bbs.web_common.url_decode(Ada.Strings.Unbounded.To_String(
                                                       Ada.Strings.Unbounded.Tail(temp1,
                                                          Ada.Strings.Unbounded.Length(temp1) - index)));
            begin
               params.Insert(Key      => key,
                             New_Item => value);
            end;
         end loop;
      end if;
   end read_headers;
   --
   --  Procedures and functions to get and set the debugging flags.
   --
   function get_debug_req return Boolean is
   begin
      return debug_req;
   end get_debug_req;
   --
   function get_debug_head return Boolean is
   begin
      return debug_head;
   end get_debug_head;
   --
   procedure set_debug_req(f : Boolean) is
   begin
      debug_req := f;
   end set_debug_req;
   --
   procedure set_debug_head(f : Boolean) is
   begin
      debug_head := f;
   end set_debug_head;
   --

end bbs.http;
