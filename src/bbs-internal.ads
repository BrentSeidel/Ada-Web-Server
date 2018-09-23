with GNAT.Sockets;
with bbs.web_common;
--
--  This package contains the various internal routines to generate pages, data.
--  figures, and the like.  This is where the main customization will occure.
--
package bbs.internal is
   --
   --  Return the count of transactions as an xml message
   --
   procedure xml_count(s : GNAT.Sockets.Stream_Access;
                       h : bbs.web_common.params.Map;
                       p : bbs.web_common.params.Map);
   --
   --  Display information sent in a form
   --
   procedure target(s : GNAT.Sockets.Stream_Access;
                    h : bbs.web_common.params.Map;
                    p : bbs.web_common.params.Map);
   --
   --  Request that the configuration file be reloaded.
   --
   procedure html_reload_config(s : GNAT.Sockets.Stream_Access;
                                h : bbs.web_common.params.Map;
                                p : bbs.web_common.params.Map);

private
   CRLF : String renames bbs.web_common.CRLF;

end bbs.internal;
