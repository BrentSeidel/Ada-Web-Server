with GNAT.Sockets;
with bbs.svg;
with bbs.internal;
with bbs.web_server;
with bbs.web_common;

procedure web is
   internal_map : bbs.web_common.proc_tables.Map;
   --
   --  Build the map for internal procedure calls.  The key strings must match
   --  the identifications in the configuration file.  The generated map is
   --  used by both the GET and POST methods.
   --
   procedure build_internal_map is
   begin
      --
      --  ******************************************************
      --  Customization goes here to add any internally routines
      --  to generate responses.
      --
      internal_map.Insert("thermometer", bbs.svg.thermometer'Access);
      internal_map.Insert("dial", bbs.svg.dial'Access);
      internal_map.Insert("configure", bbs.internal.html_show_config'Access);
      internal_map.Insert("target", bbs.internal.target'Access);
      internal_map.Insert("reload", bbs.internal.html_reload_config'Access);
      internal_map.Insert("counter", bbs.internal.xml_count'Access);
   end;

begin
   build_internal_map;
   bbs.web_server.server(internal_map, "config.txt", 31415);
end web;
