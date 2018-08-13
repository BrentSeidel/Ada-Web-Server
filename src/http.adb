package body http is

   --
   -- Return code 200 OK for normal cases
   --
   procedure ok(s : GNAT.Sockets.Stream_Access; txt: String) is
   begin
      String'Write(s, "HTTP/1.0 200 OK" & CRLF);
      String'Write(s, "Content-Type: " & txt & CRLF);
      String'Write(s, "Server: Custom Ada 2012" & CRLF);
      String'Write(s, "Connection: Close" & CRLF);
      String'Write(s, CRLF);
   end;
   --
   -- Return code 404 NOT FOUND for when the requested item is not in the
   -- directory.
   --
   procedure not_found(s : GNAT.Sockets.Stream_Access; item: String) is
   begin
      String'Write(s, "HTTP/1.0 404 NOT FOUND" & CRLF);
      String'Write(s, "Content-Type: text/html" & CRLF);
      String'Write(s, "Server: Custom Ada 2012" & CRLF);
      String'Write(s, "Connection: Close" & CRLF);
      String'Write(s, CRLF);
      String'Write(s, "<html><head><title>" & item & " not found</title></head>");
      String'Write(s, "<body>Item " & item & " cannot be found on the server.");
      String'Write(s, "</body></html>");
   end;
   --
   -- Return code 500 INTERNAL SERVER ERROR generally when unable to open the
   -- file for the specified item.
   --
   procedure internal_error(s : GNAT.Sockets.Stream_Access; file: String) is
   begin
      String'Write(s, "HTTP/1.0 500 INTERNAL ERROR" & CRLF);
      String'Write(s, "Content-Type: text/html" & CRLF);
      String'Write(s, "Server: Custom Ada 2012" & CRLF);
      String'Write(s, "Connection: Close" & CRLF);
      String'Write(s, CRLF);
      String'Write(s, "<html><head><title>Internal Error</title></head>");
      String'Write(s, "<body>Internal error trying to serve " & file);
      String'Write(s, "</body></html>");
   end;
   --
   -- Return code 501 NOT IMPLEMENTED for any request other than GET or POST.
   --
   procedure not_implemented_req(s : GNAT.Sockets.Stream_Access; req: String) is
   begin
      String'Write(s, "HTTP/1.0 501 NOT IMPLEMENTED" & CRLF);
      String'Write(s, "Content-Type: text/html" & CRLF);
      String'Write(s, "Server: Custom Ada 2012" & CRLF);
      String'Write(s, "Connection: Close" & CRLF);
      String'Write(s, CRLF);
      String'Write(s, "<html><head><title>Request type not implemented</title></head>");
      String'Write(s, "<body>Request type " & req & " is not implemented");
      String'Write(s, "</body></html>");
   end;
   --
   -- Return code 501 NOT IMPLEMENTED for a request for an internally generated
   -- item that is not yet implemented..
   --
   procedure not_implemented_int(s : GNAT.Sockets.Stream_Access; item: String) is
   begin
      String'Write(s, "HTTP/1.0 501 NOT IMPLEMENTED" & CRLF);
      String'Write(s, "Content-Type: text/html" & CRLF);
      String'Write(s, "Server: Custom Ada 2012" & CRLF);
      String'Write(s, "Connection: Close" & CRLF);
      String'Write(s, CRLF);
      String'Write(s, "<html><head><title>" & item & " not implemented</title></head>");
      String'Write(s, "<body>Item " & item & " is not yet implemented on the server.");
      String'Write(s, "</body></html>");
   end;

   --
   -- Read a line from the input stream.  The line is terminated with a CR-LF.
   -- The CR-LF is stripped from the return string as it can just be assumed to
   -- be there.
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
   end;
   --
   -- Read a specified number of characters from an input stream.  This is needed
   -- because a POST request is not terminated by CRLF.  Instead the content-length
   -- header needs to be parsed and that number of characters read at the end.
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
   end;
   --
   -- Read the headers from the request.  This will need to be updated to return
   -- the item being requested and any passed parameters.  Possibly it will also
   -- be necessary to distinguish between GET and POST methods.  Any other method
   -- will trigger a not_implemented_req() response.
   --
   -- Currently, everything is implemented, except for extracting parameters for
   -- POST requests.  Parameters on GET requests are not handled and probably
   -- won't be unless a need arises.
   --
   procedure read_headers(s : GNAT.Sockets.Stream_Access;
                          method : out request_type;
                          item : out Ada.Strings.Unbounded.Unbounded_String;
                          params : in out web_common.params.Map) is
      param_string : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
      length : Natural;
      line  : Ada.Strings.Unbounded.Unbounded_String;
      req   : Ada.Strings.Unbounded.Unbounded_String;
      temp  : Ada.Strings.Unbounded.Unbounded_String;
      index : Natural;
   begin
      --
      -- The first line contains the request.  Parse it out.
      --
      line := get_line_from_stream(s);
      Ada.Text_IO.Put_Line(Ada.Strings.Unbounded.To_String(line));
      index := Ada.Strings.Unbounded.Index(line, " ");
      req := Ada.Strings.Unbounded.Head(line, index - 1);
      line := Ada.Strings.Unbounded.Tail(line,
                                         Ada.Strings.Unbounded.Length(line) - index);
      if (req = "GET") then
         method := GET;
      elsif (req = "POST") then
         method := POST;
      else
         method := Other;
      end if;
      --
      -- Parse out the requested item.  Don't care about the HTTP version
      --
      index := Ada.Strings.Unbounded.Index(line, " ");
      if (index > 0) then
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
         --
         -- If this is a POST request, we need to find the Content-Length:
         -- header and determine the length.  To be strictly correct, the
         -- Content-Type: header should also be examined.
         --
         if (method = POST) then
            if (Ada.Strings.Unbounded.Head(line, 15) = "Content-Length:") then
               temp := Ada.Strings.Unbounded.Tail(line,
                                                  Ada.Strings.Unbounded.Length(line) - 16);
               length := Natural'Value(Ada.Strings.Unbounded.To_String(temp));
            end if;
         end if;
         exit when Ada.Strings.Unbounded.Length(line) = 0;
--         Ada.Text_IO.Put_Line(Ada.Strings.Unbounded.To_String(line));
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
            if (index > 0) then
               item := Ada.Strings.Unbounded.Head(line, index - 1);
               param_string := Ada.Strings.Unbounded.Tail(line,
                                                  Ada.Strings.Unbounded.Length(line) - index);
            end if;
         when POST =>
            --
            -- I think that if the method is post, the parameters will need to
            -- be read here.
            --
            param_string := get_data_from_stream(s, length);
         when Other =>
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
            if (index > 0) then
               temp := Ada.Strings.Unbounded.Head(param_string, index - 1);
               param_string := Ada.Strings.Unbounded.Tail(param_string,
                                                          Ada.Strings.Unbounded.Length(param_string) - index);
            else
               temp := param_string;
               param_string := Ada.Strings.Unbounded.Null_Unbounded_String;
            end if;
            --
            -- Split the key-value pair into separate key and values and store
            -- them in the params map.  At this point, the value could be URL
            -- decoded.
            --
            index := Ada.Strings.Unbounded.Index(temp, "=");
            declare
               key : String := Ada.Strings.Unbounded.To_String(
                                                     Ada.Strings.Unbounded.Head(temp, index - 1));
               value : String := web_common.url_decode(Ada.Strings.Unbounded.To_String(
                                                       Ada.Strings.Unbounded.Tail(temp,
                                                          Ada.Strings.Unbounded.Length(temp) - index)));
            begin
               params.Insert(Key      => key,
                             New_Item => value);
            end;
         end loop;
      end if;
   end;

end;
