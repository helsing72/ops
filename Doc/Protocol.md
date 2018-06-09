# Binary Protocol #
When data is sent over OPS, it is always packaged in something called OPSMessage, see [Understanding OPSObject and OPSMessage](OpsMessage.md). The OPSMessage is then serialized and split up into one or more segments where each segment is limited to a maximum of 60000 bytes, see also [Sending Large Messages > 60000 bytes](LargeMessages.md).

To see how individual data types are serialized, see [Defining Data Types in IDL](IDLLanguage.md).

All data is serialized using Little Endian byte order.

## OPSMessage ##
An OPSMessage is serialized into the following:
```
Field                 Type          Bytes
-----                 ----          -----
key                   string        4 + 1    (always = "k")
message type          byte          1
pub prio              byte          1
pub identification    int64         8
pub name              string        4 + n    n = length of Publisher name, if any set
topic name            string        4 + n    n = length of Topic name
top level key         string        4        (always = "")
address               string        4        (always = "")
data                  OPSObject*    
    data type         string        4 + n    n = length of 'data type', see note below
    OPSObject
       key            string        4 + n    n = length of 'key', see note below
    DerivedObject
       fields         ...           ...      depends on sent object
```
** Note: 'data type' **

The data type field is a concatenation of all datatypes in the inheritance hierarchy for the message sent.

** Note: 'key'**

The key field sent is the Publisher key if it has been set. If it hasn't been set, the key will be the sent message key, which defaults to "k" if not set by the user.

## Segments ##
The serialized data from OPSMessage is split into one or more segments as described above.

To be able to correctly reassemble segments into a complete message, each segment starts with a segment header (14 bytes):

```
Field                 Type          Bytes
-----                 ----          -----
protocol id                         4        = "opsp"
version               short         2        currently = 5
total # segments      int           4
segment number        int           4
```
The header is directly followed by the serialized data from the message, max 60000-14 bytes. Rest of the serialized data, if any, continues in a new segment.

## Transport specifics ##

When *TCP* is used as transport, each segment is preceded by the following *TCP* unique header (22 bytes):

```
Field                 Type          Bytes
-----                 ----          -----
protocol id                         18       = "opsp_tcp_size_info"
length of data        int           4        size of segment
```
