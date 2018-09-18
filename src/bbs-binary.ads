with Ada.Strings.Unbounded;
use type Ada.Strings.Unbounded.Unbounded_String;
with Ada.Sequential_IO;
with GNAT.Sockets;
with bbs.web_common;

package bbs.binary is

   --
   --  This procedure sends a binary file to the client with headers.
   --
   procedure send_file_with_headers(s : GNAT.Sockets.Stream_Access;
                                    mime : String; name : String);

private
   CRLF : String renames web_common.CRLF;

   package Char_IO is new Ada.Sequential_IO(Character);
end bbs.binary;
