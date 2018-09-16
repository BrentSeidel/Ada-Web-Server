With Ada.Strings;
with Ada.Strings.Unbounded;
use type Ada.Strings.Unbounded.Unbounded_String;
with GNAT.Sockets;
with web_common;
--
--  This package contains an assorment of SVG widgets that can be used by
--  procedurally generated web pages.
--
package svg is

   --
   --  Display a thermometer SVG showing the value parameter.  This procedure
   --  handles getting and checking the parameters.  The following parameters
   --  are supported:
   --    min - The minimum displayed value
   --    max - The maximum displayed value
   --    value - The value to display.
   --
   -- The value is clamped to be between min and max.  If the parameters are
   -- invalid, a Red-X is produced.
   --
   procedure thermometer(s : GNAT.Sockets.Stream_Access;
                         h : web_common.params.Map;
                         p : web_common.params.Map);
   --
   --  Display a round dial with a pointer to the appropriate value. The
   --  following parameters are supported:
   --    min - The minimum displayed value
   --    max - The maximum displayed value
   --    value - The value to display.
   --
   -- The value is clamped to be between min and max.  If the parameters are
   -- invalid, a Red-X is produced.
   --
   procedure dial(s : GNAT.Sockets.Stream_Access;
                  h : web_common.params.Map;
                  p : web_common.params.Map);

private
   CRLF : String renames web_common.CRLF;
   --
   --  Send the standard SVG header.
   --
   procedure svg_header(s : GNAT.Sockets.Stream_Access; width : Integer; height : Integer);
   --
   --  Display a thermometer type graphic.  The value is clipped to be between
   --  the min and max.  The range between min and max is divided into 10
   --  labeled ranges.  There is no fancy fiddling to adjust the min and max to
   --  make nice ranges display.  That is the job of the calling software.
   --
   procedure thermometer(s : GNAT.Sockets.Stream_Access; min : Float; max : Float; value : Float);
   --
   --  Display a red X for replacing instruments when an error occurs.  This is
   --  called locally from some other function that decides when the error has
   --  occured.
   --
   procedure red_x(s : GNAT.Sockets.Stream_Access; width : Integer; height : Integer);
end;
