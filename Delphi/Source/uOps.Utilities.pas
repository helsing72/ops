unit uOps.Utilities;

(**
*
* Copyright (C) 2016 Lennart Andersson.
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

uses uOps.Types,
     uOps.Topic;

type
  TOPSUtilities = class(TObject)
  public
    // -------------------------------------------------------------------------------
    // Methods that work on Topic names that use the syntax 'Domain::TopicName'

    // Returns a full topic name on the format 'Domain::TopicName'
    class function fullTopicName(domainName : string; topicName : string) : string;

    // Returns the topic name part
    class function topicName(name : string) : string;

    // Returns the domain name part
    class function domainName(name : string) : string;

    // -------------------------------------------------------------------------------
    // Misc methods

    // Returns true if given topic is of expected type
    class function VerifyTopicType(top : TTopic; expectedType : string) : Boolean;

  end;

implementation

uses SysUtils;

// Builds a full topic name on the format 'Domain::TopicName'
class function TOPSUtilities.fullTopicName(domainName : string; topicName : string) : string;
begin
  Result := domainName + '::' + topicName;
end;

// Parses a topic name that includes domain name using syntax 'Domain::TopicName'
// and return the topic name part
class function TOPSUtilities.topicName(name : string) : string;
var
  Idx : Integer;
begin
  Idx := Pos('::', name);
  if Idx > 0 then begin
    Result := Copy(name, Idx + 2, MaxInt);
  end else begin
    Result := name;
  end;
end;

// Parses a topic name that includes domain name using syntax 'Domain::TopicName'
// and return the domain name part
class function TOPSUtilities.domainName(name : string) : string;
var
  Idx : Integer;
begin
  Idx := Pos('::', name);
  if Idx > 0 then begin
    Result := Copy(name, 1, Idx - 1);
  end else begin
    Result := '';
  end;
end;

// Returns true if given topic is of expected type
class function TOPSUtilities.VerifyTopicType(top : TTopic; expectedType : string) : Boolean;
begin
  Result := string(top.TypeID) = expectedType;
end;

end.

