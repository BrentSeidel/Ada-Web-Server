--
--  Author: Brent Seidel
--  Date: 6-Aug-2024
--
--  This file is part of Simple Ada Web Server.
--  Simple Ada Web Server is free software: you can redistribute it and/or
--  modify it under the terms of the GNU General Public License as published
--  by the Free Software Foundation, either version 3 of the License, or (at
--  your option) any later version.
--
--  Simple Ada Web Server is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
--  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
--  more details.
--
--  You should have received a copy of the GNU General Public License along
--  with Simple Ada Web Server. If not, see <https://www.gnu.org/licenses/>.--
--
with Ada.Characters.Latin_1;
with Ada.Text_IO;
with Ada.Streams;
use type Ada.Streams.Stream_Element_Offset;
package body BBS.web.http is
   package ASU renames Ada.Strings.Unbounded; -- not the school

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
   procedure options_ok(s : GNAT.Sockets.Stream_Access; item : String;
                        dir : dictionary.Map) is
   begin
      String'Write(s, "HTTP/1.0 200 OK" & CRLF);
      if item = "*" then
         String'Write(s, "Allow: OPTIONS, GET, POST" & CRLF);
      elsif dir.Contains(item) then
         declare
            el : constant element := dir.Element(item);
            mime : constant String := ASU.To_String(el.mime);
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
   --  *** There is a potential problem using Character'Input().  No
   --  *** notification is given should the stream be closed while waiting for
   --  *** input.  If this happens, the task will hang.  This is an easy
   --  *** opportunity for a denial of service attack.  This has been addressed
   --  *** by using GNAT.Sockets.Receive_Socket instead which can indicate a
   --  *** socket closure by the value of last.  Should this be detected, an
   --  *** Ada.Text_IO.End_Error (perhaps not the best choice) is raised and
   --  *** further input is abandoned.
   --
   function get_line_from_stream(s : GNAT.Sockets.Socket_Type)
                                 return Ada.Strings.Unbounded.Unbounded_String is
      c    : Character;
      last : Ada.Streams.Stream_Element_Offset;
      elem : Ada.Streams.Stream_Element_Array(1 .. 1);
      str  : Ada.Strings.Unbounded.Unbounded_String;
   begin
      loop
         loop
            GNAT.Sockets.Receive_Socket(s, elem, last);
            if last = 0 then
               raise closed_by_peer;
            end if;
            c := Character'Val(elem(1));
            str := str & c;
            exit when c = Ada.Characters.Latin_1.CR;
         end loop;
         GNAT.Sockets.Receive_Socket(s, elem, last);
         if last = 0 then
            raise closed_by_peer;
         end if;
         c := Character'Val(elem(1));
         str := str & c;
         exit when c = Ada.Characters.Latin_1.LF;
      end loop;
      return Ada.Strings.Unbounded.Head(str, ASU.Length(str) - 2);
   end get_line_from_stream;
   --
   --  Read a specified number of characters from an input stream.  This is
   --  needed because a POST request is not terminated by CRLF.  Instead the
   --  content-length header needs to be parsed and that number of characters
   --  read at the end.
   --
   function get_data_from_stream(s : GNAT.Sockets.Socket_Type; len : Natural)
                                 return Ada.Strings.Unbounded.Unbounded_String is
      c    : Character;
      last : Ada.Streams.Stream_Element_Offset;
      elem : Ada.Streams.Stream_Element_Array(1 .. 1);
      str  : Ada.Strings.Unbounded.Unbounded_String;
   begin
      for i in Natural range 1 .. len loop
         GNAT.Sockets.Receive_Socket(s, elem, last);
         if last = 0 then
            raise closed_by_peer;
         end if;
         c := Character'Val(elem(1));
         str := str & c;
      end loop;
      return str;
   end get_data_from_stream;
   --
   --  Read the headers from the request..
   --
   procedure read_headers(s       : GNAT.Sockets.Stream_Access;
                          sock    : GNAT.Sockets.Socket_Type;
                          method  : out request_type;
                          item    : out ASU.Unbounded_String;
                          headers : in out params.Map;
                          args    : in out params.Map;
                          dir     : dictionary.Map) is
      param_string : ASU.Unbounded_String := ASU.Null_Unbounded_String;
      length : Natural;
      line  : ASU.Unbounded_String;
      req   : ASU.Unbounded_String;
      temp1 : ASU.Unbounded_String;
      temp2 : ASU.Unbounded_String;
      index : Natural;
   begin
      args.Clear;
      headers.Clear;
      --
      --  The first line contains the request.  Parse it out.
      --
      line := get_line_from_stream(sock);
      if debug_req.get then
         Ada.Text_IO.Put_Line(ASU.To_String(line));
      end if;
      index := ASU.Index(line, " ");
      req := ASU.Head(line, index - 1);
      line := ASU.Tail(line, ASU.Length(line) - index);
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
      index := ASU.Index(line, " ");
      if index > 0 then
         item := ASU.Head(line, index - 1);
         line := ASU.Tail(line, ASU.Length(line) - index);
      else
         item := line;
         line := ASU.Null_Unbounded_String;
      end if;
      --
      -- Scan through the rest of the headers.  If needed, the content-length
      -- header is parsed.
      --
      loop
         line := get_line_from_stream(sock);
         exit when ASU.Length(line) = 0;
         index := ASU.Index(line, " ");
         temp1 := ASU.Head(line, index - 1);
         temp2 := ASU.Tail(line, ASU.Length(line) - index);
         headers.Insert(Key      => ASU.To_String(temp1),
                        New_Item => ASU.To_String(temp2));
         --
         -- If this is a POST request, we need to find the Content-Length:
         -- header and determine the length.  To be strictly correct, the
         -- Content-Type: header should also be examined.
         --
         if method = POST then
            if (temp1 = "Content-Length:") then
               length := Natural'Value(ASU.To_String(temp2));
            end if;
         end if;
         if debug_head.get then
            Ada.Text_IO.Put_Line(ASU.To_String(line));
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
            index := ASU.Index(line, "?");
            if index > 0 then
               item := ASU.Head(line, index - 1);
               param_string := ASU.Tail(line, ASU.Length(line) - index);
            end if;
         when POST =>
            --
            -- If the method is post, the parameters will be read here.
            --
            param_string := get_data_from_stream(sock, length);
         when OPTIONS =>
            options_ok(s, ASU.To_String(item), dir);
         when others =>
            not_implemented_req(s, ASU.To_String(req));
      end case;
      --
      -- If there are parameters, process them and store them in a parameter
      -- dictionary.
      --
      if (param_string /= ASU.Null_Unbounded_String) then
         while (ASU.Length(param_string) > 0) loop
            --
            -- First split off a key value pair.  They are separated by '&'.
            --
            index := ASU.Index(param_string, "&");
            if index > 0 then
               temp1 := ASU.Head(param_string, index - 1);
               param_string := ASU.Tail(param_string, ASU.Length(param_string) - index);
            else
               temp1 := param_string;
               param_string := ASU.Null_Unbounded_String;
            end if;
            --
            -- Split the key-value pair into separate key and values and store
            -- them in the params map.  At this point, the value could be URL
            -- decoded.
            --
            index := ASU.Index(temp1, "=");
            declare
               key : constant String := ASU.To_String(ASU.Head(temp1, index - 1));
               value : constant String := url_decode(ASU.To_String(
                                                     ASU.Tail(temp1,
                                                     ASU.Length(temp1) - index)));
            begin
               args.Insert(Key      => key,
                           New_Item => value);
            end;
         end loop;
      end if;
   end read_headers;

end BBS.web.http;
