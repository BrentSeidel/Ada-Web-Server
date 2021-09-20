with Ada.Text_IO;
with Ada.Exceptions;
with GNAT.Sockets;
with bbs.svg;
with bbs.internal;
with bbs.http;
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
      internal_map.Insert("target", bbs.internal.target'Access);
      internal_map.Insert("reload", bbs.internal.html_reload_config'Access);
      internal_map.Insert("counter", bbs.internal.xml_count'Access);
      internal_map.Insert("raise", BBS.internal.html_raise'Access);
      internal_map.Insert("exit", BBS.internal.html_set_exit'Access);
   end;

begin
   --
   -- Set debugging flags to appropriate values.
   --
   bbs.web_server.debug.set;
   bbs.http.debug_req.set;
   bbs.http.debug_head.clear;
   --
   -- Build the map for internal routines.
   --
   build_internal_map;
   --
   -- Start the web server.  This does not return normally.
   --
   bbs.web_server.server(internal_map, "config.txt", 31415);
   --
   --  This is used to cause a file not found exception for testing purposes.
   --  Comment out the line above this one and uncomment this one to test.
   --
--   bbs.web_server.server(internal_map, "bad-name.bad", 31415);
exception
   when err: Others =>
      Ada.Text_IO.Put_Line("Unhandled exception occured during operation.");
      Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Information(err));
end web;
