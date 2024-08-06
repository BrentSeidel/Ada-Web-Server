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
package BBS.web.server is
   --
   --  This is the web server.  In initializes the network interface and enters
   --  an infinite loop processing requests.  The passed paraemters are:
   --    internals   - A map of the names of internal item and procedure
   --    config_name - The name of the configuration file
   --    port        - The port to listen on
   --
   --  Note that this procedure never returns.  Eventually code should be added
   --       to shut the server down and exit.  It may also be turned into a task
   --       which may allow multiple servers to be run simultaneously.
   --
   procedure server(internals : proc_tables.Map;
                    config_name : String;
                    port : GNAT.Sockets.Port_Type);
   --
   --  Handle the details of the http request.  When a request comes in, the
   --  socket is passed via the start entry point to the task.  The task handles
   --  the processing and closes the socket when finished.  It then waits for
   --  another request.  The end_task entry is used to terminate the task.
   --
   task type request_handler is
      entry start(socket : GNAT.Sockets.Socket_Type;
                  internals : proc_tables.Map;
                  dir : dictionary.Map);
      entry end_task;
   end request_handler;

   --
   -- Flag to control display of debug messages
   --
   debug : bbs.web.protected_flag; -- Display requests.

private
   --
   --  Number of handler tasks to create.  The number is somewhat arbitrary and
   --  should be based on the expected load.
   --
   num_handlers : constant Natural := 10;

end BBS.web.server;
