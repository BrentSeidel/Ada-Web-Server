with Ada.Strings.Unbounded;
use type Ada.Strings.Unbounded.Unbounded_String;
with GNAT.Sockets;
with bbs.web_common;

package bbs.web_server is
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
   procedure server(internals : bbs.web_common.proc_tables.Map;
                    config_name : String;
                    port : GNAT.Sockets.Port_Type);
   --
   --  Handle the details of the http request.  When a request comes in, the
   --  socket is passed via the start entry point to the task.  The task handles
   --  the processing and closes the socket when finished.  It then waits for
   --  another request.  The end_task entry is used to terminate the task.
   --
   task type request_handler is
      entry start(socket : GNAT.Sockets.Socket_Type);
      entry end_task;
   end request_handler;

private
   --
   --  Number of handler tasks to create.  The number is somewhat arbitrary and
   --  should be based on the expected load.
   --
   num_handlers : constant Natural := 10;
   --
   --  Map to translate names of internal items to the procedures to actually
   --  call.
   --
   internal_map : bbs.web_common.proc_tables.Map;

end bbs.web_server;
