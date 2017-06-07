# Ada OPS Implementation #

The Ada implementation now has full functionality (i.e. the same as C++) with the following exceptions:

  - Subscribers don't have support for deadline timers
  - Subscribers don't have support for checking publication ID
  - Subscribers don't expose the history buffer

The implementation uses features from *Ada 2012* and uses the *XMLAda* library for parsing ops configuration files and the *GNAT.Sockets* library for socket communication.

The implementation has so far only been tested with ***GNAT GPS 2016*** on *Windows 10*.

Tip:
Set environment symbol *GPR_PROJECT_PATH* to include path to the *ops4.gpr* file
