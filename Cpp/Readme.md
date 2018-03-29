# C++ OPS Configuration #

All configurations are placed in file *OPSTypeDefs.h*.

#### USE_C11 ####
Defined to enable the use of C++11 *std::mutex*, *std::thread*, *std::condition_variable*
instead of Boost or WIN32/Linux specific calls.

#### OVERRIDE_DEFAULT_NO_C11 ###
Define this to override the default settings and disable the use of C++11 features for backward compatibility. This will use the Boost library and WIN32/Linux specific calls instead of C++11.

#### OPSSLIM_NORESERVE ####
Define this to remove *Reservable* from *OPSMessage*.

#### REPLACE_TRANSPORT_LAYER ####
Define this to remove IOService.cpp, Sender.cpp, Receiver.cpp, DeadlineTimer.cpp, NetworkSupport.cpp and TimeHelper.cpp
thats using the Boost library, so you can use your own implementations (e.g. for targets that has no Boost implementation).

#### REPLACE_OPS_CONFIG ####
Define this to remove the OPSConfig file reader, so you can implement your own for targets without a filesystem.

#### FIXED_NO_STD_STRING ####
Defined to disable the *std::string* interface in the *fixed_string* class.

#### USE_FIXED_LENGTH_STRINGS ####
Define this to use the *fixed_string* class instead of *std::string* in OPS. When this is defined you can also define the maximum length of different string types in OPS, see below.

Note that using *fixed_strings* in IDL's works both with and without this defined.

##### FIXED_OBJECT_NAME_SIZE #####
Max name length for: DomainId, ParticipantId, TopicName, PublisherName, SubscriberName, etc.
If you use ops::utilities::nnn() the length need to be able to handle Domain::TopicName.
Default size is 50.

##### FIXED_MESSAGE_KEY_SIZE #####
Max length of key set by user on publisher/message and subscriber filter. Default size is 60.

##### FIXED_TYPE_ID_SIZE #####
Max TypeString length and depends on IDL type names and inheritance depth. Default size is 256.

##### FIXED_CHANNEL_ID_SIZE #####
Max length of channel ID specified in the OPS configuration. Default size is 20.

##### FIXED_FILENAME_SIZE #####
Max path/filename length. Default size is 1024.

#### ON_BIG_ENDIAN_MACHINE ####
OPS uses Little Endian data serialization to improve the performance since
thats the native packing for x86 and it also works on Arm.
If OPS is compiled for a Big Endian machine (and it need to communicate with a
little endian machine via OPS) you need to define this to keep the binary compatibility.

#### DEBUG_OPSOBJECT_COUNTER ####
Define this to add counting of create/delete of OPSObject() (only if also USE_C11 is defined).
This also adds a debug function for reading the current number of living OPSObjects.
