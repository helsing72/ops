# Data Class Slicing and spareBytes #

As stated earlier, OPS support inheritance between IDL Data classes. This fact raises the question on how OPS handles situation where some parts (components) of a system not fully can understand a datatype. Will information be lost then?

The answer is that OPS has chosen to not slice away information in such cases because components that receive the same data later might understand it better.

Considering the following case:

You have an IDL class describing some sort of user data:

```
package users;

class UserData
{
   string nickname;
   string fullName;
}
```

and a topic UserTopic for that data.

Later on, you decided you need to extend UserData to include telephonenumber of the user as well, so you create

```
package users;

class ExtendedUserData extends UserData
{
   string telephoneNumber;
}
```

The made up problem now is that you have a very complicated relay infrastructure for passing UserData on UserTopic around in your system, e.g. you may have domain relays and a storage mechinsim for all data being published. Consequently, you do not want to update, or even recompile, your relay infrastructure but rather just the producers of the UserData and the clients that display it in some way.

Here the question arise again, is this possible, or will the relays that did not know about ExtendedUserData at the time of compilation "slice" away the telephoneNumber before republishing the message?

The answer is no, OPS will keep all the bytes it does not understand in a byte array field called spareBytes which is a member of OPSObject. In this way OPS allows you to create services like logging or relaying that act on very basic implementations and does not need to be recompiled every time you make updates to your interfaces.

You can access the spare bytes like this in C++:

```
UserData* data = subscriber.getDataReference();
std::vector<char> spares = data->spareBytes;
```

and in Java:

```
UserData data = subscriber.getData();
byte[] spares = data.spareBytes;
```

but normally you do not need to care about them at all.