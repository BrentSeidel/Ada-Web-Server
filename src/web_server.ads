with Ada.Text_IO;
with Ada.Strings.Unbounded;
use type Ada.Strings.Unbounded.Unbounded_String;
with GNAT.Sockets;
with http;
use type http.request_type;
with html;
with text;
with binary;
with internal;
with svg;
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
   -- Handle the details of the http request.  This is done as a task.  Once a
   -- network connection is made, the stream for that connection is handed off
   -- to a task which processes the request, runs to completion, and then exits.
   --
   task type request_handler is
      entry start(socket : GNAT.Sockets.Socket_Type);
   end request_handler;
   type handler_ptr is access request_handler;

end web_server;
