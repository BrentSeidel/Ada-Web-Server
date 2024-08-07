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
with Ada.Strings.Unbounded;
use type Ada.Strings.Unbounded.Unbounded_String;
with GNAT.Sockets;
package BBS.web.http is
   --
   -- Type of request.  OPTIONS, GET, and POST are supported.  All others will
   -- will return a not_implemented_req() response.
   --
   type request_type is (CONNECT, DELETE, GET, HEAD, OPTIONS, PATCH, POST, PUT,
                         TRACE, Other);
   --
   --  Return code 200 OK for normal cases
   --
   procedure ok(s : GNAT.Sockets.Stream_Access; txt: String)
     with Global => (Input => (CRLF, server_header));
   --
   -- Return code 404 NOT FOUND for when the requested item is not in the
   -- directory.
   --
   procedure not_found(s : GNAT.Sockets.Stream_Access; item: String)
     with Global => (Input => (CRLF, server_header));
   --
   -- Return code 500 INTERNAL SERVER ERROR generally when unable to open the
   -- file for the specified item.
   --
   procedure internal_error(s : GNAT.Sockets.Stream_Access; file: String)
     with Global => (Input => (CRLF, server_header));
   --
   -- Return code 501 NOT IMPLEMENTED for a request for an internally generated
   -- item that is not yet implemented.
   --
   procedure not_implemented_int(s : GNAT.Sockets.Stream_Access; item: String)
     with Global => (Input => (CRLF, server_header));
   --
   -- The read_headers procedure will need to handle both GET and POST request
   -- as well as return the passed parameters.  Returned values will be the
   -- requested item and a dictionary containing the parameters.  If there are
   -- no parameters, the dictionary will be empty.
   --
   procedure read_headers(s       : GNAT.Sockets.Stream_Access;
                          sock    : GNAT.Sockets.Socket_Type;
                          method  : out request_type;
                          item    : out Ada.Strings.Unbounded.Unbounded_String;
                          headers : in out params.Map;
                          args    : in out params.Map;
                          dir     : dictionary.Map)
     with Global => Null;
   --
   -- Flags to control printing of requests and headers for debugging purposes.
   --
   debug_req : protected_flag;
   debug_head : protected_flag;

private
   --
   -- Return code 200 OK for OPTIONS reqest cases
   --
   procedure options_ok(s : GNAT.Sockets.Stream_Access; item : String;
                        dir : dictionary.Map)
     with Global => (Input => (CRLF, server_header));
   --
   -- Return code 501 NOT IMPLEMENTED for any unsupported request.  This is
   -- generated internal to the http package.
   --
   procedure not_implemented_req(s : GNAT.Sockets.Stream_Access; req: String)
     with Global => (Input => (CRLF, server_header));
   --
   -- Read a line from the HTTP request stream
   --
   function get_line_from_stream(s : GNAT.Sockets.Socket_Type)
                                 return Ada.Strings.Unbounded.Unbounded_String
     with Global => Null;
   --
   -- Read a specified number of characters from the HTTP request stream
   --
   function get_data_from_stream(s : GNAT.Sockets.Socket_Type; len : Natural)
                                 return Ada.Strings.Unbounded.Unbounded_String
     with Global => Null;

end BBS.web.http;
