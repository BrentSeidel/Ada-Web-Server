package body web_common is

   --
   -- Load the dictionary from a file.  No error checking is done on the open
   -- since if this doesn't work, there really isn't much point in continuing.
   --
   -- Each line of the file must contain three entries each separated by a single
   -- space.  The entries themselves cannot contain spaces.
   --
   -- The first entry is the reqested item.  This is what the client sends to the
   -- server.  Since the client HTML encodes certain characters, this will have
   -- to match the encoded value.
   --
   -- The second entry is either the local filename or a name to identify an
   -- internal routine to generate data.
   --
   -- The last entry is the MIME type of the file or "internal" if the data is
   -- generated internally.
   --
   -- Comments can be added.  A comment is indicated by a '#' character at the
   -- start of a line.  Everything through the end of that line is ignored.
   --
   procedure load_directory(name : String) is
      file : Ada.Text_IO.File_Type;
      line : Ada.Strings.Unbounded.Unbounded_String;
      item : Ada.Strings.Unbounded.Unbounded_String;
      location : Ada.Strings.Unbounded.Unbounded_String;
      mime : Ada.Strings.Unbounded.Unbounded_String;
      space : Natural;
      el : element;
   begin
      Ada.Text_IO.Open(File     => file,
                       Mode     => Ada.Text_IO.In_File,
                       Name     => name);
      while not Ada.Text_IO.End_Of_File(file) loop
         line := Ada.Text_IO.Unbounded_IO.Get_Line(file);
         --
         -- Check for comment.  First character is '#'.
         --
         if (Ada.Strings.Unbounded.Element(line, 1) /= '#') then
            --
            -- Parse out the item
            --
            space := Ada.Strings.Unbounded.Index(line, " ");
            item := Ada.Strings.Unbounded.Head(line, space - 1);
            line := Ada.Strings.Unbounded.Tail(line,
                                               Ada.Strings.Unbounded.Length(line) - space);
            --
            -- Parse out the location and MIME type
            --
            space := Ada.Strings.Unbounded.Index(line, " ");
            location := Ada.Strings.Unbounded.Head(line, space - 1);
            mime := Ada.Strings.Unbounded.Tail(line,
                                               Ada.Strings.Unbounded.Length(line) - space);
--            Ada.Text_IO.Put_Line("Item <" & Ada.Strings.Unbounded.To_String(item) &
--                                   ">, location <" & Ada.Strings.Unbounded.To_String(location) &
--                                   "> MIME <" & Ada.Strings.Unbounded.To_String(mime) & ">");
            el.file := location;
            el.mime := mime;
            directory.Insert(Ada.Strings.Unbounded.To_String(item), el);
         end if;
      end loop;
      Ada.Text_IO.Close(file);
   end;
   --
   -- Convert a hex digit character to a number
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
   end;
   --
   -- URL Decode a string.
   --
   function url_decode(s : String) return String is
      t : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
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
      return Ada.Strings.Unbounded.To_String(t);
   end;

end;
