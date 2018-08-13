with Ada.Streams;
with Ada.Strings.Unbounded;
use type Ada.Strings.Unbounded.Unbounded_String;
with Ada.Text_IO;
--with Ada.Text_IO.Unbounded_IO;
with Ada.Sequential_IO;
with GNAT.Sockets;
with http;
with web_common;

package binary is

   --
   -- This procedure sends a binary file to the client with headers.
   --
   procedure send_file_with_headers(s : GNAT.Sockets.Stream_Access;
                                    mime : String; name : String);

   --
   -- This procedure sends a text file to the client with headers.
   --

private
   CRLF : String renames web_common.CRLF;

   package Char_IO is new Ada.Sequential_IO(Character);
end;
