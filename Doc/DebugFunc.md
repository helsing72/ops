# Debug Functionality #

Currently only implemented in C++ and only if the symbol **OPS_ENABLE_DEBUG_HANDLER** is defined in OPSTypeDefs.h or when compiling the OPS library. Each application also need to enable the functionality by calling the static method *DebugHandler::SetKey()* with an application unique key (used to direct a request to a specific application in a larger system).

The debug functionality can then be enabled/disabled for the system by defining the domains *debugMcPort* tag and setting the value > 0, see [configuration](OpsConfig.md).

The debug functionality includes at least the following (useful for testing):
* list the Publishers and Subscribers that the application has set up,

* individually enabled/disabled Publishers and Subscribers,

* for a Publisher change the Publication ID, block a number of messages, inject messages and query current status,

* for a Subscriber block a number of messages and query current status.

For exact functionality see description in the idl-file *Common/idl/DebugRequestResponseData.idl*.

See also [DebugConsole](DebugConsole.md) which is a tool for accessing the debug functionality.
