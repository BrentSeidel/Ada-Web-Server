package body binary is

   --
   -- Send the contents of the specified binary type file out to the client
   -- program.  If the file can't be opened, a HTTP 404 NOT FOUND code is
   -- returned instead of 200 OK.
   --
   -- Note that currently the file is sent byte by byte.  This is likely not the
   -- most efficient way, but it works, and for now there is no need for maximum
   -- performance.
   --
   procedure send_file_with_headers(s : GNAT.Sockets.Stream_Access;
                                    mime : String; name : String) is
      buff : Character;
      file : Char_IO.File_Type;
   begin
      begin
         Char_IO.Open(File     => file,
                          Mode     => Char_IO.In_File,
                          Name     => name);
      exception
         when others =>
            http.internal_error(s, name);
            return;
      end;
      http.ok(s, mime);
      while not Char_IO.End_Of_File(file) loop
         Char_IO.Read(file, buff);
         Character'Write(s, buff);
      end loop;
      Char_IO.Close(file);
   end;

end;
