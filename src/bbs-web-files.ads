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
