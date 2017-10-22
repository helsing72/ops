unit uOps.NetworkSupport;

(**
*
* Copyright (C) 2016-2017 Lennart Andersson.
*
* This file is part of OPS (Open Publish Subscribe).
*
* OPS (Open Publish Subscribe) is free software: you can redistribute it and/or modify
* it under the terms of the GNU Lesser General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* OPS (Open Publish Subscribe) is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public License
* along with OPS (Open Publish Subscribe).  If not, see <http://www.gnu.org/licenses/>.
*)

interface

  function doSubnetTranslation(addr : AnsiString) : AnsiString;

  function isValidNodeAddress(addr : AnsiString) : Boolean;
  function isMyNodeAddress(addr : AnsiString) : Boolean;

implementation

uses SysUtils,
     Winapi.Windows,
     Winapi.IpHlpApi,
     Winapi.IpRtrMib,
     Winapi.Winsock;

/// ------------------------------------------
/// Helper to get all IP interfaces

procedure VVGetIpAddrTable(var p: PMibIpAddrTable; var Size: Cardinal; const bOrder: BOOL);
var
  Res: DWORD;
begin
  p := nil;
  Size := 0;
  if @GetIpAddrTable = nil then Exit;   //Not implemented in this windows version
  Res := GetIpAddrTable(p,Size,bOrder);
  if Res=ERROR_INSUFFICIENT_BUFFER then begin
    Getmem(p,Size);
    // Caller must free this buffer when it is no longer used
    FillChar(p^,Size,#0);
    Res := GetIpAddrTable(p,Size,bOrder);
  end;
  if Res <> NO_ERROR then begin
    if Assigned(p) then FreeMem(p);
    p := nil;
    Size := 0;
  end;
end;

function IpAddressToString(Addr: DWORD): AnsiString;
var
  InAddr: TInAddr;
begin
  InAddr.S_addr := Addr;
  Result := AnsiString(inet_ntoa(InAddr));
end;

// If argument contains a "/" we assume it is on the form:  subnet-address/subnet-mask
// e.g "192.168.10.0/255.255.255.0" or "192.168.10.0/24"
// In that case we loop over all interfaces and take the first one that matches
// i.e. the one whos interface address is on the subnet
function doSubnetTranslation(addr : AnsiString) : AnsiString;
var
  Idx : Integer;
  subnet, mask : AnsiString;
  subnetIp, subnetMask : DWORD;
  Size: ULONG;
  p: PMibIpAddrTable;
  i: integer;
begin
  Result := addr;

  // If no '/' we just return the given address
  Idx := Pos('/', string(addr));
  if Idx = 0 then Exit;

  subnet := Copy(addr, 1, Idx-1);
  mask   := Copy(addr, Idx+1, MaxInt);

  subnetIp := inet_addr(PAnsiChar(subnet));
  if Length(mask) <= 2 then begin
    // Expand to the number of bits given
    subnetMask := StrToInt(string(mask));
    subnetMask := (((1 shl subnetMask)-1) shl (32 - subnetMask)) and $FFFFFFFF;
    subnetMask := ntohl(subnetMask);
  end else begin
    subnetMask := inet_addr(PAnsiChar(mask));
  end;

  VVGetIpAddrTable(p, Size, False);
  if Assigned(p) then begin
    try
      with p^ do begin
        for i := 0 to dwNumEntries - 1 do begin
          with table[i] do begin
            if (dwAddr and subnetMask) = (subnetIp and subnetMask) then begin
              Result := IpAddressToString(dwAddr);
              Break;
            end;
          end;
        end;
      end;
    finally
      FreeMem(p);
    end;
  end;
end;

function isValidNodeAddress(addr : AnsiString) : Boolean;
var
  Ip : DWORD;
begin
  Result := False;

  //std::cout << "isValidNodeAddress(): " << addr << std::endl;
  if addr = '' then Exit;

  Ip := ntohl(inet_addr(PAnsiChar(addr)));
  //std::cout << "isValidNodeAddress(): " << std::hex << Ip << std::dec << std::endl;
  if Ip = 0 then Exit;
  if Ip >= $E0000000 then Exit;  // Skip multicast and above
  Result := True;
end;

function isMyNodeAddress(addr : AnsiString) : Boolean;
var
  Ip : DWORD;
  Size: ULONG;
  p: PMibIpAddrTable;
  i: integer;
begin
  Result := False;

  //std::cout << "isMyNodeAddress(): " << addr << std::endl;
  if addr = '' then Exit;

  Ip := ntohl(inet_addr(PAnsiChar(addr)));
  //std::cout << "isMyNodeAddress(): " << std::hex << Ip << std::dec << std::endl;

  if Ip = $7F000001 then begin
    Result := True;  // localhost
    Exit;
  end;

  VVGetIpAddrTable(p, Size, False);
  if Assigned(p) then begin
    try
      with p^ do begin
        for i := 0 to dwNumEntries - 1 do begin
          with table[i] do begin
            if DWORD(htonl(dwAddr)) = Ip then begin
              Result := True;
              Break;
            end;
          end;
        end;
      end;
    finally
      FreeMem(p);
    end;
  end;
end;

end.

