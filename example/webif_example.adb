--
--  Author: Brent Seidel
--  Date: 6-Aug-2024
--
--  This file is part of Simple Ada Web Server.
--  Simple Ada Web Server is free software: you can redistribute it and/or
--  modify it under the terms of the GNU General Public License as published
--  by the Free Software Foundation, either version 3 of the License, or (at
--  your option) any later version.
--
--  Simple Ada Web Server is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
--  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
--  more details.
--
--  You should have received a copy of the GNU General Public License along
--  with Simple Ada Web Server. If not, see <https://www.gnu.org/licenses/>.--
--
with Ada.Text_IO;
with Ada.Exceptions;
with GNAT.Sockets;
with BBS.web.svg;
with BBS.web.internal;
with BBS.web.http;
with BBS.web.server;
with BBS.web;

procedure webif_example is
   internal_map : BBS.web.proc_tables.Map;
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
      internal_map.Insert("thermometer", BBS.web.svg.thermometer'Access);
      internal_map.Insert("dial", BBS.web.svg.dial'Access);
      internal_map.Insert("target", BBS.web.internal.target'Access);
      internal_map.Insert("reload", BBS.web.internal.html_reload_config'Access);
      internal_map.Insert("counter", BBS.web.internal.xml_count'Access);
      internal_map.Insert("raise", BBS.web.internal.html_raise'Access);
      internal_map.Insert("exit", BBS.web.internal.html_set_exit'Access);
   end;

begin
   --
   -- Set debugging flags to appropriate values.
   --
   BBS.web.server.debug.set;
   BBS.web.http.debug_req.set;
   BBS.web.http.debug_head.clear;
   --
   -- Build the map for internal routines.
   --
   build_internal_map;
   --
   -- Start the web server.  This does not return normally.
   --
   BBS.web.server.server(internal_map, "config.txt", 31415);
   --
   --  This is used to cause a file not found exception for testing purposes.
   --  Comment out the line above this one and uncomment this one to test.
   --
--   bbs.web_server.server(internal_map, "bad-name.bad", 31415);
exception
   when err: Others =>
      Ada.Text_IO.Put_Line("Unhandled exception occured during operation.");
      Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Information(err));
end webif_example;
