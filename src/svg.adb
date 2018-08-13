package body svg is

   --
   -- Display a thermometer SVG showing the value parameter.  This procedire
   -- does the extraction and checking of parameters and then calls another
   -- procedure to do the actual display.
   --
   procedure thermometer(s : GNAT.Sockets.Stream_Access; p : web_common.params.Map) is
      value : Integer;
      max : Integer := 250;
      min : Integer := 0;
   begin
      if (web_common.params.Contains(p, "min")) then
         begin
            min := Integer'Value(web_common.params.Element(p, "min"));
         exception
            when others =>
               min := 0;
         end;
      end if;
      if (web_common.params.Contains(p, "max")) then
         begin
            max := Integer'Value(web_common.params.Element(p, "max"));
         exception
            when others =>
               max := 250;
         end;
      end if;
      if (web_common.params.Contains(p, "value")) then
         begin
            value := Integer'Value(web_common.params.Element(p, "value"));
         exception
            when others =>
               value := min;
         end;
         if (value < min) then
            value := min;
         elsif (value > max) then
            value := max;
         end if;
      end if;
      svg.thermometer(s, Float(min), Float(max), Float(value));
   end;
   --
   -- Display a thermometer type graphic.  The value is clipped to be between
   -- the min and max.  The range between min and max is divided into 10
   -- labeled ranges.  There is no fancy fiddling to adjust the min and max to
   -- make nice ranges display.  That is the job of the calling software.
   --
   -- This could be combined with the procedure above.
   --
   procedure thermometer(s : GNAT.Sockets.Stream_Access; min : Float; max : Float; value : Float) is
      start : Integer;
      scale : Float := (max - min) / 250.0;
      height : Integer := Integer(value*scale - min);
   begin
      http.ok(s, "image/svg+xml");
      String'Write(s, "<?xml version=""1.0"" encoding=""UTF-8"" standalone=""no""?>" & CRLF);
      String'Write(s, "<svg version=""1.1"" baseProfile=""full""" & CRLF);
      String'Write(s, "width=""100"" height=""350""" & CRLF);
      String'Write(s, "xmlns=""http://www.w3.org/2000/svg"" >" & CRLF);
      --
      -- Black outline
      --
      String'Write(s, "<circle fill=""black"" cx=""40"" cy=""300"" r=""30"" />" & CRLF);
      String'Write(s, "<rect fill=""black"" width=""40"" height=""250"" x=""20"" y=""30"" />" & CRLF);
      String'Write(s, "<circle fill=""black"" cx=""40"" cy=""30"" r=""20"" />" & CRLF);
      --
      -- Labels
      --
      for i in Integer range 0 .. 10 loop
         String'Write(s, "<path d=""M60 " & Integer'Image(280 - i*25) &
                        " h10 "" stroke=""black"" stroke-width=""1""/>" & CRLF);
         String'Write(s, "<text x=""72"" y=""" &
                        Ada.Strings.Fixed.Trim(Integer'Image(283 - i*25), Ada.Strings.Left) &
                        """ fill=""black"" font-size=""14"" stroke-width=""1"">" &
                        Integer'Image(Integer(min + Float(i)*(max-min)/(scale*10.0))) & "</text>" & CRLF);
      end loop;
      --
      -- White fill
      --
      String'Write(s, "<circle fill=""white"" cx=""40"" cy=""30"" r=""10"" />" & CRLF);
      String'Write(s, "<rect fill=""white"" width=""20"" height=""250"" x=""30"" y=""30"" />" & CRLF);
      --
      -- Red mark
      --
      String'Write(s, "<circle fill=""red"" cx=""40"" cy=""300"" r=""20"" />" & CRLF);
      String'Write(s, "<rect fill=""red"" width=""20"" height=""20"" x=""30"" y=""280"" />" & CRLF);
      if (height > 0) then
         start := 280 - height;
         String'Write(s, "<rect fill=""red"" width=""20"" height=""" & Integer'Image(height) &
                        """ x=""30"" y=""" & Integer'Image(start) & """ />" & CRLF);
      end if;
      --
      -- Closing stuff
      --
      String'Write(s, "</svg>" & CRLF);
   end;
   --
   -- Display a round dial with a pointer to the appropriate value. The following
   -- parameters are supported:
   -- min - The minimum displayed value
   -- max - The maximum displayed value
   -- value - The value to display.
   --
   -- The value is clamped to be between min and max.  It is assumed that min is
   -- less than max.  Bad things may happen if it isn't.
   --
   procedure dial(s : GNAT.Sockets.Stream_Access; p : web_common.params.Map) is
      value : Integer;
      max : Integer := 250;
      min : Integer := 0;
      scale : Float;
      angle : Integer;
   begin
      --
      -- Extract the parameters
      --
      if (web_common.params.Contains(p, "min")) then
         begin
            min := Integer'Value(web_common.params.Element(p, "min"));
         exception
            when others =>
               min := 0;
         end;
      end if;
      if (web_common.params.Contains(p, "max")) then
         begin
            max := Integer'Value(web_common.params.Element(p, "max"));
         exception
            when others =>
               max := 250;
         end;
      end if;
      if (web_common.params.Contains(p, "value")) then
         begin
            value := Integer'Value(web_common.params.Element(p, "value"));
         exception
            when others =>
               value := min;
         end;
         if (value < min) then
            value := min;
         elsif (value > max) then
            value := max;
         end if;
      end if;
      scale := Float(max - min) /270.0;
      --
      -- Send headers
      --
      http.ok(s, "image/svg+xml");
      String'Write(s, "<?xml version=""1.0"" encoding=""UTF-8"" standalone=""no""?>" & CRLF);
      String'Write(s, "<svg version=""1.1"" baseProfile=""full""" & CRLF);
      String'Write(s, "width=""300"" height=""300""" & CRLF);
      String'Write(s, "xmlns=""http://www.w3.org/2000/svg"" >" & CRLF);
      --
      -- Draw the graphics
      --
      String'Write(s, "<circle fill=""white"" cx=""150"" cy=""150"" r=""140""" &
                   " stroke-width=""2"" stroke=""black""/>" & CRLF);
      --
      -- Labels
      --
      for i in Integer range 0 .. 10 loop
         String'Write(s, "<g transform=""rotate(" & Integer'Image(i*27 - 45) &
                        ",150,150)"">" & CRLF);
         String'Write(s, "<path d=""M10 150 h10"" stroke=""black"" stroke-width=""1""/>" & CRLF);
         String'Write(s, "<text x=""22"" y=""152"" fill=""black"" font-size=""14"" stroke-width=""1"">" &
                        Integer'Image(Integer(Float(min) + Float(i)*Float(max-min)/10.0)) & "</text>" & CRLF);
         String'Write(s, "</g>" & CRLF);
      end loop;
      --
      -- Draw pointer
      --
      angle := Integer(Float(value - min)*270.0/Float(max - min) - 45.0);
      String'Write(s, "<g transform=""rotate(" & Integer'Image(angle) &
                     ",150,150)"">" & CRLF);
      String'Write(s, "<path d=""M25 150 H160"" stroke=""red"" stroke-width=""3""/>" & CRLF);
      String'Write(s, "<path d=""M150 140 V160"" stroke=""red"" stroke-width=""3""/>" & CRLF);
      String'Write(s, "</g>" & CRLF);
      --
      -- Closing stuff
      --
      String'Write(s, "</svg>" & CRLF);
   end;


end;
