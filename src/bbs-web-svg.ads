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
with Ada.Strings;
with Ada.Strings.Unbounded;
use type Ada.Strings.Unbounded.Unbounded_String;
with GNAT.Sockets;
--
--  This package contains an assorment of SVG widgets that can be used by
--  procedurally generated web pages.
--
package BBS.web.svg is

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
                         h : params.Map;
                         p : params.Map)
     with Global => (Input => CRLF);
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
                  h : params.Map;
                  p : params.Map)
     with Global => (Input => CRLF);

private
   --
   --  Send the standard SVG header.
   --
   procedure svg_header(s : GNAT.Sockets.Stream_Access; width : Integer; height : Integer)
     with Global => (Input => CRLF);
   --
   --  Display a thermometer type graphic.  The value is clipped to be between
   --  the min and max.  The range between min and max is divided into 10
   --  labeled ranges.  There is no fancy fiddling to adjust the min and max to
   --  make nice ranges display.  That is the job of the calling software.
   --
   procedure thermometer(s : GNAT.Sockets.Stream_Access; min : Float; max : Float; value : Float)
     with Global => (Input => CRLF);
   --
   --  Display a red X for replacing instruments when an error occurs.  This is
   --  called locally from some other function that decides when the error has
   --  occured.
   --
   procedure red_x(s : GNAT.Sockets.Stream_Access; width : Integer; height : Integer)
     with Global => (Input => CRLF);

end BBS.web.svg;
