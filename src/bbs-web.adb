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
with Ada.Exceptions;
package body BBS.web is
   package ASU renames Ada.Strings.Unbounded; -- not the school
   --
   --  A protected type for maintianing a counter of active request_handler
   --  tasks.
   --
   protected body protected_counter is
      procedure increment is
      begin
         value := value + 1;
      end;
      --
      procedure decrement is
      begin
         value := value - 1;
      end;
      --
      function read return Integer is
      begin
         return value;
      end;
   end protected_counter;
   --
   -- A protected flag for communicating between tasks
   --
   protected body protected_flag is
      procedure set is
      begin
         value := True;
      end;
      procedure clear is
      begin
         value := False;
      end;
      function get return Boolean is
      begin
         return value;
      end;
   end protected_flag;
   --
   --  Load the dictionary from a file.  No error checking is done on the open
   --  since if this doesn't work, there really isn't much point in continuing.
   --
   --  Each line of the file must contain three entries each separated by a
   --  single space.  The entries themselves cannot contain spaces.
   --
   --  The first entry is the reqested item.  This is what the client sends to
   --  the server.  Since the client HTML encodes certain characters, this will
   --  have to match the encoded value.
   --
   --  The second entry is either the local filename or a name to identify an
   --  internal routine to generate data.
   --
   --  The last entry is the MIME type of the file or "internal" if the data is
   --  generated internally.
   --
   --  Comments can be added.  A comment is indicated by a '#' character at the
   --  start of a line.  Everything through the end of that line is ignored.
   --
   procedure load_directory(name : String; map : out dictionary.Map) is
      location : ASU.Unbounded_String;
      file  : Ada.Text_IO.File_Type;
      line  : ASU.Unbounded_String;
      item  : ASU.Unbounded_String;
      mime  : ASU.Unbounded_String;
      space : Natural;
      el    : element;
   begin
      map.Clear;
      Ada.Text_IO.Open(File => file,
                       Mode => Ada.Text_IO.In_File,
                       Name => name);
      while not Ada.Text_IO.End_Of_File(file) loop
         line := Ada.Text_IO.Unbounded_IO.Get_Line(file);
         --
         --  Check for comment.  First character is '#'.
         --
         if (ASU.Element(line, 1) /= '#') then
            --
            --  Parse out the item
            --
            space := ASU.Index(line, " ");
            item := ASU.Head(line, space - 1);
            line := ASU.Tail(line, ASU.Length(line) - space);
            --
            --  Parse out the location and MIME type
            --
            space := ASU.Index(line, " ");
            location := ASU.Head(line, space - 1);
            mime := ASU.Tail(line, ASU.Length(line) - space);
            el.file := location;
            el.mime := mime;
            map.Insert(ASU.To_String(item), el);
         end if;
      end loop;
      Ada.Text_IO.Close(file);
   exception
      when err: others =>
         Ada.Text_IO.Put_Line("Exception occured processing directory file.");
         Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Information(err));
         raise;
   end load_directory;
   --
   --  Convert a hex digit character to a number
   --
   function hex_digit(c : Character) return Integer is
      n : Integer := 0;
   begin
      case c is
         when '0' =>
            n := 0;
         when '1' =>
            n := 1;
         when '2' =>
            n := 2;
         when '3' =>
            n := 3;
         when '4' =>
            n := 4;
         when '5' =>
            n := 5;
         when '6' =>
            n := 6;
         when '7' =>
            n := 7;
         when '8' =>
            n := 8;
         when '9' =>
            n := 9;
         when 'a' | 'A' =>
            n := 10;
         when 'b' | 'B' =>
            n := 11;
         when 'c' | 'C' =>
            n := 12;
         when 'd' | 'D' =>
            n := 13;
         when 'e' | 'E' =>
            n := 14;
         when 'f' | 'F' =>
            n := 15;
         when others =>
            n := 0;
      end case;
      return n;
   end hex_digit;
   --
   --  URL Decode a string.
   --
   function url_decode(s : String) return String is
      t : ASU.Unbounded_String := ASU.Null_Unbounded_String;
      i : Natural := 1;
      c : Character;
      v : Integer;
   begin
      while (i <= s'Length) loop
         c := s(i);
         if (c = '%') then
            v := hex_digit(s(i + 1))*16 + hex_digit(s(i + 2));
            c := Character'Val(v);
            i := i + 2;
            null;
         elsif (c = '+')
         then
            c := ' ';
         end if;
         i := i + 1;
         t := t & c;
      end loop;
      return ASU.To_String(t);
   end url_decode;

end BBS.web;
