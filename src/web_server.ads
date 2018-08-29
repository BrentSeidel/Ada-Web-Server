with Ada.Strings.Unbounded;
use type Ada.Strings.Unbounded.Unbounded_String;
with GNAT.Sockets;
--with http;
--use type http.request_type;
with web_common;

package web_server is
   --
   -- This is the web server.  In initializes the network interface and enters
   -- an infinite loop processing requests.
   --
   procedure server;
   --
   -- Simple procedure to decode internally generated pages.  It's used by both
   -- GET and POST methods and so should be common.
   --
   procedure decode_internal(s : GNAT.Sockets.Stream_Access; name : String;
                             p : web_common.params.Map);
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
--   type handler_ptr is access request_handler;
   --
   -- Flag to indicate that the configuration file has changed and needs to be
   -- reloaded.  This would typically be used during development or debugging.
   --
   reload_configuration : Boolean := False;

private
   --
   -- Number of handler tasks to create.  The number is somewhat arbitrary and
   -- should be based on the expected load.
   --
   num_handlers : constant Natural := 10;

end web_server;
