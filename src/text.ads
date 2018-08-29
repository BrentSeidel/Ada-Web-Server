with Ada.Strings.Unbounded;
use type Ada.Strings.Unbounded.Unbounded_String;
with GNAT.Sockets;
with web_common;

package text is

   --
   -- This procedure sends a text file to the client with headers.
   --
   procedure send_file_with_headers(s : GNAT.Sockets.Stream_Access;
                                    mime : String; name : String);

   --
   -- This procedure sends a text file to the client with headers.
   --
   procedure send_file_without_headers(s : GNAT.Sockets.Stream_Access;
                                    name : String);

private
   CRLF : String renames web_common.CRLF;

end;
