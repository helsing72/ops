# Debug Console #
***DebugConsole*** is a tool for accessing the built-in debug facilities in OPS.

TBD

The following shows the available command line arguments:

```
Version 2019-11-29

  Usage:
    DebugConsole [-?][-v] [-cfg file] -list

      -? | -h         Help
      -v              Verbose
      -cfg file       ops_config.xml file to use (default ops_config.xml in CWD)
      -list           lists all OPS Domains in given config file with debugging enabled

    DebugConsole -cfg file -d domain <command> [-k key]

      -d domain       OPS Domain to use
      -lk             List all instance keys
      -lp             List all publishers (*)
      -ls             List all subscribers (*)
                        * can be limited by giving a key (-k)

    DebugConsole -cfg file -d domain -k key <command>

      -k key          Key to set in sent debug message
      -lpa            List info from all publisher topics
      -lpi name       List info from publisher topic 'name'
      -lsa            List info from all subscriber topics
      -lsi name       List info from subscriber topic 'name'

    DebugConsole -cfg file -d domain -k key -e n -n name -c cmd -p1 num

      -e type         Entity type (2 = Publisher, 3 = Subscriber)
      -n name         Entity name (for pub/sub the topic name)
      -c cmd          Command to send to entity
      -p1 num         Value for parameter 1
```

See also [debug functionality](DebugFunc.md).
