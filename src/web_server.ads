with Ada.Strings.Unbounded;
use type Ada.Strings.Unbounded.Unbounded_String;
with GNAT.Sockets;
with web_common;

package web_server is
   --
   --  Build the map for internal procedure calls.  The key strings must match
   --  the identifications in the configuration file.  This needs to be here
   --  so that the various internal routines are accessable.
   --
   procedure build_internal_map;
   --
   -- This is the web server.  In initializes the network interface and enters
   -- an infinite loop processing requests.
   --
   procedure server;
   --
   -- Handle the details of the http request.  When a request comes in, the
   -- socket is passed via the start entry point to the task.  The task handles
   -- the processing and closes the socket when finished.  It then waits for
   -- another request.  The end_task entry is used to terminate the task.
   --
   task type request_handler is
      entry start(socket : GNAT.Sockets.Socket_Type);
      entry end_task;
   end request_handler;

private
   --
   -- Number of handler tasks to create.  The number is somewhat arbitrary and
   -- should be based on the expected load.
   --
   num_handlers : constant Natural := 10;

end web_server;
