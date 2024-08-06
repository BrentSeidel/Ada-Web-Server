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
with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with BBS.web.http;
package body BBS.web.files is

   --
   --  Send the contents of the specified binary type file out to the client
   --  program.  If the file can't be opened, a HTTP 404 NOT FOUND code is
   --  returned instead of 200 OK.
   --
   --  Note that currently the file is sent byte by byte.  This is likely not
   --  the most efficient way, but it works, and for now there is no need for
   --  maximum performance.
   --
   procedure send_binary_with_headers(s : GNAT.Sockets.Stream_Access;
                                    mime : String; name : String) is
      buff : Character;
      file : Char_IO.File_Type;
   begin
      begin
         Char_IO.Open(File => file,
                      Mode => Char_IO.In_File,
                      Name => name);
      exception
         when others =>
            BBS.web.http.internal_error(s, name);
            return;
      end;
      BBS.web.http.ok(s, mime);
      while not Char_IO.End_Of_File(file) loop
         Char_IO.Read(file, buff);
         Character'Write(s, buff);
      end loop;
      Char_IO.Close(file);
   end send_binary_with_headers;

   --
   --  Send the contents of the specified text type file out to the client
   --  program.  If the file can't be opened, a HTTP 404 NOT FOUND code is
   --  returned instead of 200 OK.
   --
   procedure send_text_with_headers(s : GNAT.Sockets.Stream_Access;
                                    mime : String; name : String) is
      line : Ada.Strings.Unbounded.Unbounded_String;
      file : Ada.Text_IO.File_Type;
   begin
      begin
         Ada.Text_IO.Open(File => file,
                          Mode => Ada.Text_IO.In_File,
                          Name => name);
      exception
         when others =>
            BBS.web.http.internal_error(s, name);
            return;
      end;
      BBS.web.http.ok(s, mime);
      while not Ada.Text_IO.End_Of_File(file) loop
         line := Ada.Text_IO.Unbounded_IO.Get_Line(file);
         String'Write(s, Ada.Strings.Unbounded.To_String(line) & CRLF);
      end loop;
      Ada.Text_IO.Close(file);
   end send_text_with_headers;
   --
   --  This procedure sends a text file to the client with headers.  If the file
   --  cannot be opened, the procedure simply returns.
   --
   procedure send_text_without_headers(s : GNAT.Sockets.Stream_Access;
                                    name : String) is
      line : Ada.Strings.Unbounded.Unbounded_String;
      file : Ada.Text_IO.File_Type;
   begin
      begin
         Ada.Text_IO.Open(File => file,
                          Mode => Ada.Text_IO.In_File,
                          Name => name);
      exception
         when others =>
            return;
      end;
      while not Ada.Text_IO.End_Of_File(file) loop
         line := Ada.Text_IO.Unbounded_IO.Get_Line(file);
         String'Write(s, Ada.Strings.Unbounded.To_String(line) & CRLF);
      end loop;
      Ada.Text_IO.Close(file);
   end send_text_without_headers;

end BBS.web.files;
