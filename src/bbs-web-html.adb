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
with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
package body BBS.web.html is
   --
   --  Generate a standard HTML heading.  This just has a title attribute
   --
   procedure html_head(s : GNAT.Sockets.Stream_Access; title: String) is
   begin
      String'Write(s, "<html><head><title>" & title & "</title></head><body>" & CRLF);
   end html_head;
   --
   --  Generate a simple HTML heading with style sheet
   --
   procedure html_head(s : GNAT.Sockets.Stream_Access; title: String; style : String) is
   begin
      String'Write(s, "<html><head>" & CRLF);
      String'Write(s, "<title>" & title & "</title>" & CRLF);
      String'Write(s, "<link rel=""stylesheet"" type=""text/css"" href=""" & style & """>" & CRLF);
      String'Write(s, "</head><body>" & CRLF);
   end html_head;
   --
   --  Generate a standard HTML ending.  The ending should be in a file.  If the
   --  file can't be found, a minimal ending is substatuted.
   --
   procedure html_end(s : GNAT.Sockets.Stream_Access; name: String) is
      line : Ada.Strings.Unbounded.Unbounded_String;
      file : Ada.Text_IO.File_Type;
   begin
      begin
         Ada.Text_IO.Open(File => file,
                          Mode => Ada.Text_IO.In_File,
                          Name => name);
      exception
         when others =>
            String'Write(s, "</body></html>" & CRLF);
            return;
      end;
      while not Ada.Text_IO.End_Of_File(file) loop
         line := Ada.Text_IO.Unbounded_IO.Get_Line(file);
         String'Write(s, Ada.Strings.Unbounded.To_String(line) & CRLF);
      end loop;
      Ada.Text_IO.Close(file);
   end html_end;

end BBS.web.html;
