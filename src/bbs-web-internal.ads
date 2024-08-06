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
with GNAT.Sockets;
--
--  This package contains the various internal routines to generate pages, data.
--  figures, and the like.  This is where the main customization will occure.
--
package BBS.web.internal is
   --
   --  Return the count of transactions as an xml message
   --
   procedure xml_count(s : GNAT.Sockets.Stream_Access;
                       h : params.Map;
                       p : params.Map);
   --
   --  Display information sent in a form
   --
   procedure target(s : GNAT.Sockets.Stream_Access;
                    h : params.Map;
                    p : params.Map);
   --
   --  Request that the configuration file be reloaded.
   --
   procedure html_reload_config(s : GNAT.Sockets.Stream_Access;
                                h : params.Map;
                                p : params.Map);
   --
   -- Set the web exit flag.
   --
   procedure html_set_exit(s : GNAT.Sockets.Stream_Access;
                           h : params.Map;
                           p : params.Map);
   --
   -- Raise an exception to test task exception handling
   --
   procedure html_raise(s : GNAT.Sockets.Stream_Access;
                        h : params.Map;
                        p : params.Map);

end BBS.web.internal;
