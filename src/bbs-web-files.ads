with Ada.Sequential_IO;
with GNAT.Sockets;

package BBS.web.files is

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
     with Global => (CRLF);

   --
   --  This procedure sends a text file to the client with headers.
   --
   procedure send_text_without_headers(s : GNAT.Sockets.Stream_Access;
                                    name : String)
     with Global => (CRLF);

private

   package Char_IO is new Ada.Sequential_IO(Character);
end BBS.web.files;
