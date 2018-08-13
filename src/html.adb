package body html is

   --
   -- Generate a standard HTML heading.  This just has a title attribute
   --
   procedure html_head(s : GNAT.Sockets.Stream_Access; title: String) is
   begin
      String'Write(s, "<html><head><title>" & title & "</title></head><body>" & CRLF);
   end;
   --
   -- Generate a simple HTML heading with style sheet
   --
   procedure html_head(s : GNAT.Sockets.Stream_Access; title: String; style : String) is
   begin
      String'Write(s, "<html><head>" & CRLF);
      String'Write(s, "<title>" & title & "</title>" & CRLF);
      String'Write(s, "<link rel=""stylesheet"" type=""text/css"" href=""" & style & """>" & CRLF);
      String'Write(s, "</head><body>" & CRLF);
   end;
   --
   -- Generate a standard HTML ending.  The ending should be in a file.  If the
   -- file can't be found, a minimal ending is substatuted.
   --
   procedure html_end(s : GNAT.Sockets.Stream_Access; name: String) is
      line : Ada.Strings.Unbounded.Unbounded_String;
      file : Ada.Text_IO.File_Type;
   begin
      begin
         Ada.Text_IO.Open(File     => file,
                          Mode     => Ada.Text_IO.In_File,
                          Name     => name);
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
   end;

end;
