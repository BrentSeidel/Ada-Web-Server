with GNAT.Sockets;
--with bbs.web;
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
