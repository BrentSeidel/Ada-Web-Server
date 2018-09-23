with Ada.Characters.Latin_1;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Strings.Unbounded;
use type Ada.Strings.Unbounded.Unbounded_String;
with GNAT.Sockets;
--
--  This package contains assorted common constants and data for the web server.
--
package bbs.web_common is

   --
   --  Record type containing a file name and a MIME type.  Used to identify
   --  files to serve to the client.  These are unbounded strings because Ada
   --  doesn't let you use unconstrained Strings here.
   --
   type element is record
      file : Ada.Strings.Unbounded.Unbounded_String; -- File name
      mime : Ada.Strings.Unbounded.Unbounded_String; -- MIME type
   end record;
   --
   --  Instantiate a hashed map indexed by a string and containing records.
   --  Used to translate from requested items to actual files to serve.
   --
   package dictionary is new Ada.Containers.Indefinite_Hashed_Maps
     (Element_Type => element,
      Key_Type => String,
      Hash => Ada.Strings.Hash_Case_Insensitive,
      Equivalent_Keys => Ada.Strings.Equal_Case_Insensitive);
   --
   --  Instantiate a hashed map indexed by a string and containing strings.
   --  Used to contain a parameter list.  Can also be used to contain headers
   --  and values.
   --
   package params is new Ada.Containers.Indefinite_Hashed_Maps
     (Element_Type => String,
      Key_Type => String,
      Hash => Ada.Strings.Hash_Case_Insensitive,
      Equivalent_Keys => Ada.Strings.Equal_Case_Insensitive);
   --
   --  Define a type for the user procedures.  This is for a map used to map
   --  the internal procedures.  The parameters are:
   --    s - The stream to write output to.
   --    p - Any passed parameters from the HTTP request
   --    h - HTTP request headers.
   --
   type user_proc is access procedure (s : GNAT.Sockets.Stream_Access;
                                       p : params.Map;
                                       h : params.Map);
   --
   --  Instantiate a hashed map indexed by a string and containing procedure
   --  accesses.  Used as a table to identify which internal procedure to call
   --  for internal requests.
   --
   package proc_tables is new Ada.Containers.Indefinite_Hashed_Maps
     (Element_Type => user_proc,
      Key_Type => String,
      Hash => Ada.Strings.Hash_Case_Insensitive,
      Equivalent_Keys => Ada.Strings.Equal_Case_Insensitive);
   --
   --  Procedure to load the directory from a file.  Clears the directory map
   --  and then reads data from a file to load the directory.
   --
   procedure load_directory(name : String; map : out dictionary.Map)
     with Global => Null;
   --
   --  Convert a single hex digit character to a number
   --
   function hex_digit(c : Character) return Integer
     with Global => Null,
       Post => (hex_digit'Result >= 0 and hex_digit'Result <= 15);
   --
   --  URL Decode a string.
   --
   function url_decode(s : String) return String
     with Global => Null;
   --
   --  Common data.
   --
   CRLF : constant String := Ada.Characters.Latin_1.CR & Ada.Characters.Latin_1.LF;
   server_header : constant String := "Server: Custom Ada 2012 Server" & CRLF;
   --
   --  A counter to provide some data to send to the client.
   --
   counter : Integer := 0;
   --
   --  A protected type for maintaining a counter of active request_handler
   --  tasks.
   --
   protected type protected_counter is
      procedure increment;
      procedure decrement;
      function read return Integer;
   private
      value : integer := 0;
   end protected_counter;
   --
   --  A counter to keep track of how many request_handler tasks are active.
   --  The value should be a low positive number.  If it goes negative, a
   --  problem has occured.  If the value tends to increase, it means that the
   --  handler tasks are not terminating.
   --
   task_counter : protected_counter;
   --
   --  Flag to indicate that the configuration file has changed and needs to be
   --  reloaded.  This would typically be used during development or debugging.
   --
   reload_configuration : Boolean := False;
end bbs.web_common;
