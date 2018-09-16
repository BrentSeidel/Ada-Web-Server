With Ada.Strings;
with Ada.Strings.Unbounded;
use type Ada.Strings.Unbounded.Unbounded_String;
with GNAT.Sockets;
with web_common;
--
--  This package contains the various internal routines to generate pages, data.
--  figures, and the like.  This is where the main customization will occure.
--
package internal is
   --
   --  Return the count of transactions as an xml message
   --
   procedure xml_count(s : GNAT.Sockets.Stream_Access;
                       h : web_common.params.Map;
                       p : web_common.params.Map);
   --
   --  Display the configuration data as a table.
   --
   procedure html_show_config(s : GNAT.Sockets.Stream_Access;
                              h : web_common.params.Map;
                              p : web_common.params.Map);
   --
   --  Display information sent in a form
   --
   procedure target(s : GNAT.Sockets.Stream_Access;
                    h : web_common.params.Map;
                    p : web_common.params.Map);
   --
   --  Request that the configuration file be reloaded.
   --
   procedure html_reload_config(s : GNAT.Sockets.Stream_Access;
                                h : web_common.params.Map;
                                p : web_common.params.Map);

private
   CRLF : String renames web_common.CRLF;
end;
