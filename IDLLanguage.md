# The OPS IDL Language #
The OPS IDL language is a language for defining data types (or data classes) in a programming language neutral way. The point is to define data types to be communicated over OPS once in OPS IDL and then ”compile” the IDL code into the desired target programming languages. In addition to a language specific data class it is also necessary to generate additional code to enable type safe and developer friendly communication for the data types.

## Basics ##
The OPS IDL language resembles very simplified Java and is due to its minimalistic design easy to learn and read. As mention above, the language is made for designing data classes, and that is in fact also the only thing it can do. A data class definition in OPS IDL must contain the following:

  * One and only one package definition
  * One and only one class definition

```
package samples;
class SampleData
{

}
```

The package definition consists of the reserved word package followed by the name of the package for the data class. Package in OPS IDL is equivalent to package in Java or namespace in C++ or C#. The package shall be expressed as a single word or as a series of dot separated words, e.g. “samples” or “samples.subsamples” etc and the package declaration shall be ended with a “;”.
The class definition is the word class followed by a single word for the class name followed by a body marked with an opening and a closing brace, “{     }”. Within the body of the class, an arbitrary number of fields can be declared. Each field can be of any of the OPS IDL defined basic types, an array of a basic type, another user defined data class or an array of such a data class.

The following table shows a listing of all available OPS IDL types and their corresponding  representations in common programming languages:

|OPS IDL|C++|Java|C#|Serialized on the network|
|:------|:--|:---|:-|:------------------------|
|class  |class|class|class|-                        |
|package|namespace|package|namespace|-                        |
|boolean|bool|boolean|bool|1 byte                   |
|byte   |char|byte|byte|1  byte                  |
|short  |int16|short|short|2 bytes                  |
|int    |int32|int |int|4 bytes                  |
|long   |int64|long|long|8 bytes                  |
|float  |float|float|float|4 byte                   |
|double |double|double|double|8 bytes                  |
|string |std::string|String|string|4 bytes (size) + 1 byte per character (8-bit)|
|T      |T  |T   |T |sizeof( T )              |
|T`[``]`|std::vector< T >|java.util.Vector< T >|List< T >|4 bytes (size) + sizeof( T ) per element|

Or as IDL code example:

```
package samples;
class SampleData
{
   boolean boo;
   byte b;
   short sh;
   int i;
   long l;
   float f;
   double d;
   string s;
   UserData uData;

   boolean[] boos;
   byte[] bytes;
   short[] shorts;
   int[] ints;
   long[] longs;
   float[] floats;
   double[] doubles;
   string[] strings;
   UserData[] uDatas; 

}
```

This yields the following generated code in [C++](SampleDataCpp.md) and [Java](SampleDataJava.md).

## Inheritance ##

In addition to the simple approach above, OPS IDL also supports inheritance to some extent. A class can extend one other class (multiple inheritance is not supported):

```
package samples;
class ChildData extends SampleData
{

}
```

Which in this case makes child data extend all fields from SampleData.

As we saw in the sample above it is possible to declare fields of another user defined type, for example, we could declare a field of our new type as follows:

`ChildData childData;`

This is of course OK, but the inheritance does not give us much more then saving a few lines of IDL.
What we normally would like to do when using inheritance is to have some sort of polymorfism. And the natural way to do that would seem to be to declare the field of the base type:

1.
`SampleData childData;`

While this is a valid declaration it does not give us the opportunity to use the field as  a ChildData. This because OPS tries to use static memory allocation as often as possible and this type of declaration will generate code (if allowed by the target language) that allocates the field on the stack rather than on the heap and thus not allow it to change implementation dynamically .
To tell the OPS code generators to allow for dynamic memory allocation and support for polymorfism the keyword virtual is used like this:

2.
`virtual SampleData childData;`

To clearify things a bit let us see what the two ways of declaring a field would affect the code generation in C++.

In case 1. The field would be declared the same way in C++ as in IDL:

`SampleData childData;`

And thus have its default constructor called on creation of the container class.

Case 2. on the other hand, would generate a pointer declaration like this:

`SampleData* childData;`

Which then allows the implemation of the pointer to be decided and changed by the user dynamically.

Arrays are treated in the same manner, to declare an array of object whos implementation is unknown at compile time use the following approach:

`virtual SampleData[] childDatas;`

To see a useful situation when, how and why to use inheritance in OPS, have a look at the WeatherStationExample.

## Comments ##

Two types of comments are allowed in OPS IDL:

1. `//This is a comment for the rest of the line...`

and

2. `/* This is an ended comment, max one line though... */`

More than just beeing two ways of accomplishing the same thing, comment as in 1. stays in IDL and will not be visible in the generated code while comments like in 2. will be.

Se also, [Tools - IDL Builder](IDLCompilerTutorial.md) to se how to edit and compile OPS IDL files.





