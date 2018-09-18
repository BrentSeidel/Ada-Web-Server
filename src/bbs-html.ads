with Ada.Strings.Unbounded;
use type Ada.Strings.Unbounded.Unbounded_String;
with GNAT.Sockets;
with bbs.web_common;

package bbs.html is

   --
   --  For procedurally generated html documents, a few helper functions.  Note
   --  that the procedure should already have taken care of the HTTP headers.
   --
   --  Generate a simple HTML heading
   --
   procedure html_head(s : GNAT.Sockets.Stream_Access; title: String);
   --
   --  Generate a simple HTML heading with style sheet
   --
   procedure html_head(s : GNAT.Sockets.Stream_Access; title: String; style : String);
   --
   --  Generate a standard HTML ending
   --
   procedure html_end(s : GNAT.Sockets.Stream_Access; name: String);

private
   CRLF : String renames bbs.web_common.CRLF;

end bbs.html;