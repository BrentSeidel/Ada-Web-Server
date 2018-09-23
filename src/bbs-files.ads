with Ada.Sequential_IO;
with GNAT.Sockets;
with bbs.web_common;

package bbs.files is

   --
   --  This procedure sends a binary file to the client with headers.
   --
   procedure send_binary_with_headers(s : GNAT.Sockets.Stream_Access;
                                      mime : String; name : String)
     with Global => Null;
   --
   --  This procedure sends a text file to the client with headers.
   --
   procedure send_text_with_headers(s : GNAT.Sockets.Stream_Access;
                                    mime : String; name : String)
     with Global => (web_common.CRLF);

   --
   --  This procedure sends a text file to the client with headers.
   --
   procedure send_text_without_headers(s : GNAT.Sockets.Stream_Access;
                                    name : String)
     with Global => (web_common.CRLF);

private
   CRLF : String renames web_common.CRLF;

   package Char_IO is new Ada.Sequential_IO(Character);
end bbs.files;
