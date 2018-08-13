# Ada-Web-Server
This is a simple web server designed to be used as a front end to embedded systems.
It should be able to run on small Linux based computers such as a BeagleBone Black
or Raspberry Pi.  Unless you are only interested in serving a few static pages,
the code will require modifications for your application.

## Customization
### Ada
The primary Ada files for customization are web_server.adb, internal.ads, and
internal.adb.  Depending on you application you may wish to add additional code
and/or tasks to perform data collection.  This will be all compiled together into
one monolithic executable.

The specific changes needed for the web server are:
#### web_server.adb
Update the decode_internal() procedure to call your internal code to generate web
pages.

#### internal.ads and internal.adb
Add routines here to generate your web pages and provide any other necessary data.

### Other
Some other files will also need to be updated for your application.
#### config.txt
This is a list of all the supported URLs that can be requested and what the server
should do for each one.  It is a simple text file with three space separated fields
on each line.  The first is the URL, the second is the file name or code to identify
an internal routine and the last is the MIME type for files or "internal" for
internal routines.
#### *.html
Create whatever HTML files that you need.
#### *.js
Create whatever JavaScript files that you need.

## Examples
There are some example HTML and JS files as well as example internal code.  You
should be able to build the existing code and have a functioning system.  It may
not do anything useful, but you should be able to look at the existing code and
see how to modify it for your application.

## License
This code is licensed as GPL V3.0.  If you wish to use it under a different license.
please contact the author.
