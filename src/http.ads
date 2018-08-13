with Ada.Characters.Latin_1;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Strings;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Unbounded;
use type Ada.Strings.Unbounded.Unbounded_String;
with Ada.Text_IO;
with GNAT.Sockets;
with Ada.Containers.Indefinite_Hashed_Maps;
with web_common;

package http is

   --
   -- Type of request.  GET and POST are supported.  All others will return OTHER.
   --
   type request_type is (GET, POST, Other);
   --
   -- Return code 200 OK for normal cases
   --
   procedure ok(s : GNAT.Sockets.Stream_Access; txt: String);
   --
   -- Return code 404 NOT FOUND for when the requested item is not in the
   -- directory.
   --
   procedure not_found(s : GNAT.Sockets.Stream_Access; item: String);
   --
   -- Return code 500 INTERNAL SERVER ERROR generally when unable to open the
   -- file for the specified item.
   --
   procedure internal_error(s : GNAT.Sockets.Stream_Access; file: String);
   --
   -- Return code 501 NOT IMPLEMENTED for any request other than GET or POST.
   --
   procedure not_implemented_req(s : GNAT.Sockets.Stream_Access; req: String);
   --
   -- Return code 501 NOT IMPLEMENTED for a request for an internally generated
   -- item that is not yet implemented..
   --
   procedure not_implemented_int(s : GNAT.Sockets.Stream_Access; item: String);
   --
   -- Read a line from the HTTP request stream
   --
   function get_line_from_stream(s : GNAT.Sockets.Stream_Access)
                                 return Ada.Strings.Unbounded.Unbounded_String;
   --
   -- Read a specified number of characters from the HTTP request stream
   --
   function get_data_from_stream(s : GNAT.Sockets.Stream_Access; len : Natural)
                                 return Ada.Strings.Unbounded.Unbounded_String;
   --
   -- The read_headers procedure will need to handle both GET and POST request
   -- as well as return the passed parameters.  Returned values will be the
   -- requested item and a dictionary containing the parameters.  If there are
   -- no parameters, the dictionary will be empty.
   --
   procedure read_headers(s : GNAT.Sockets.Stream_Access;
                          method : out request_type;
                          item : out Ada.Strings.Unbounded.Unbounded_String;
                          params : in out web_common.params.Map);

private
   CRLF : String renames web_common.CRLF;

end;
