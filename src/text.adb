package body text is

   --
   -- Send the contents of the specified text type file out to the client
   -- program.  If the file can't be opened, a HTTP 404 NOT FOUND code is
   -- returned instead of 200 OK.
   --
   procedure send_file_with_headers(s : GNAT.Sockets.Stream_Access;
                                    mime : String; name : String) is
      line : Ada.Strings.Unbounded.Unbounded_String;
      file : Ada.Text_IO.File_Type;
   begin
      begin
         Ada.Text_IO.Open(File     => file,
                          Mode     => Ada.Text_IO.In_File,
                          Name     => name);
      exception
         when others =>
            http.internal_error(s, name);
            return;
      end;
      http.ok(s, mime);
      while not Ada.Text_IO.End_Of_File(file) loop
         line := Ada.Text_IO.Unbounded_IO.Get_Line(file);
         String'Write(s, Ada.Strings.Unbounded.To_String(line) & CRLF);
      end loop;
      Ada.Text_IO.Close(file);
   end;
   --
   -- This procedure sends a text file to the client with headers.  If the file
   -- cannot be opened, the procedure simply returns.
   --
   procedure send_file_without_headers(s : GNAT.Sockets.Stream_Access;
                                    name : String) is
      line : Ada.Strings.Unbounded.Unbounded_String;
      file : Ada.Text_IO.File_Type;
   begin
      begin
         Ada.Text_IO.Open(File     => file,
                          Mode     => Ada.Text_IO.In_File,
                          Name     => name);
      exception
         when others =>
            return;
      end;
      while not Ada.Text_IO.End_Of_File(file) loop
         line := Ada.Text_IO.Unbounded_IO.Get_Line(file);
         String'Write(s, Ada.Strings.Unbounded.To_String(line) & CRLF);
      end loop;
      Ada.Text_IO.Close(file);
   end;

end;
