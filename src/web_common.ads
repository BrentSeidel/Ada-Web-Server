with Ada.Characters.Latin_1;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Strings.Unbounded;
use type Ada.Strings.Unbounded.Unbounded_String;
with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with GNAT.Sockets;
--
-- This package contains assorted common constants and data for the web server.
--
package web_common is

   --
   -- Record type containing a file name and a MIME type.  Used to identify
   -- files to serve to the client.
   --
   type element is record
      file : Ada.Strings.Unbounded.Unbounded_String; -- File name
      mime : Ada.Strings.Unbounded.Unbounded_String; -- MIME type
   end record;
   --
   -- Instantiate a hashed map indexed by a string and containing records.  Used
   -- to translate from requested items to actual files to serve.
   --
   package dictionary is new Ada.Containers.Indefinite_Hashed_Maps
     (Element_Type => element,
      Key_Type => String,
      Hash => Ada.Strings.Hash_Case_Insensitive,
      Equivalent_Keys => Ada.Strings.Equal_Case_Insensitive);
   --
   -- Instantiate a hashed map indexed by a string and containing strings.  Used
   -- to contain a parameter list.  Can also be used to contain headers and values.
   --
   package params is new Ada.Containers.Indefinite_Hashed_Maps
     (Element_Type => String,
      Key_Type => String,
      Hash => Ada.Strings.Hash_Case_Insensitive,
      Equivalent_Keys => Ada.Strings.Equal_Case_Insensitive);
   --
   -- Procedure to load the directory from a file.
   --
   procedure load_directory(name : String);
   --
   -- Convert a single hex digit character to a number
   --
   function hex_digit(c : Character) return Integer;
   --
   -- URL Decode a string.
   --
   function url_decode(s : String) return String;
   --
   -- Common data.
   --
   directory : dictionary.Map;
   port : constant GNAT.Sockets.Port_Type := 31415;
   CRLF : constant String := Ada.Characters.Latin_1.CR & Ada.Characters.Latin_1.LF;
   --
   -- A counter to provide some data to send to the client.
   --
   counter : Integer := 0;
end;
