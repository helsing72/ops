
with System,
     Ada.Unchecked_Deallocation;

use  System;

package Ctv is

  -- Modular types (can be used with operator and, or, ...)
  type Unsigned_Int64 is mod 2**64;
  for Unsigned_Int64'Size use 64;

  type Unsigned_Int32 is mod 2**32;
  for Unsigned_Int32'Size use 32;

  type Unsigned_Int16 is mod 2**16;
  for Unsigned_Int16'Size use 16;

  type Unsigned_Int8 is mod 2**8;
  for Unsigned_Int8'Size use 8;

  -- Integer types
  type Integer64 is new Long_Long_Integer;
  for Integer64'Size use 64;

  type Integer32 is range -16#8000_0000# .. 16#7FFF_FFFF#;
  for Integer32'Size use 32;

  type Integer16 is range -16#8000# .. 16#7FFF#;
  for Integer16'Size use 16;

  type Integer8  is range -16#80# .. 16#7F#;
  for Integer8'Size use 8;


  -- Floating point types
  type Float64 is new Long_Float;
  type Float32 is new Float;


  -- Byte array e.g. used in sending and receiving from sockets
--  type ByteArray_T  is array ( Unsigned_Int16 range <> ) of aliased Byte_T;
--  type ByteArray_At is access all ByteArray_T;    
  
--  procedure Dealloc is new Ada.Unchecked_Deallocation( ByteArray_T, ByteArray_At );


  -- Here are some constants which define the bit positions in a portable
  -- manner, depending on the bit ordering (System.Default_Bit_Order).
  MSB_Is_Bit0 : constant := Boolean'Pos(Default_Bit_Order = High_Order_First); -- PowerPC
  LSB_Is_Bit0 : constant := Boolean'Pos(Default_Bit_Order = Low_Order_First);  -- Intel x86
  
end Ctv;


