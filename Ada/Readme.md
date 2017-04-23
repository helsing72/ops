# Ada OPS Implementation #

The Ada implementation now supports sending and receiving of Multicast topics.

The **opsc** IDL-compiler can generate Ada code for IDL objects and the needed factories.

The implementation uses features from *Ada 2012* and uses the *XMLAda* library for parsing ops configuration files and the *Win32Ada* library for socket communication.

The implementation has so far only been tested with ***GNAT GPS 2016*** on *Windows 10*.
