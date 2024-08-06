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
with GNAT.Sockets;
package BBS.web.html is
   --
   --  For procedurally generated html documents, a few helper functions.  Note
   --  that the procedure should already have taken care of the HTTP headers.
   --
   --  Generate a simple HTML heading
   --
   procedure html_head(s : GNAT.Sockets.Stream_Access; title: String)
     with Global => (Input => CRLF);
   --
   --  Generate a simple HTML heading with style sheet
   --
   procedure html_head(s : GNAT.Sockets.Stream_Access; title: String; style : String)
     with Global => (Input => CRLF);
   --
   --  Generate a standard HTML ending
   --
   procedure html_end(s : GNAT.Sockets.Stream_Access; name: String)
     with Global => (Input => CRLF);

end BBS.web.html;
