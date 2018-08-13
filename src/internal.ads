with Ada.Streams;
With Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
use type Ada.Strings.Unbounded.Unbounded_String;
with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with GNAT.Sockets;
with http;
with html;
with svg;
with web_common;
--
-- This package contains the various internal routines to generate pages, data.
-- figures, and the like.  This is where the main customization will occure.
--
package internal is
   --
   -- Return the count of transactions as an xml message
   --
   procedure xml_count(s : GNAT.Sockets.Stream_Access);
   --
   -- Display the configuration data as a table.
   --
   procedure html_show_config(s : GNAT.Sockets.Stream_Access);
   --
   -- Display information sent in a form
   --
   procedure target(s : GNAT.Sockets.Stream_Access; p : web_common.params.Map);

private
   CRLF : String renames web_common.CRLF;
end;
