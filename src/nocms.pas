program nocms;

{$mode objfpc}{$H+}

uses 
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  SysUtils, StrUtils, lazutf8, nclex;

var
    sitePath : AnsiString;

begin
  
  sitePath := 'TestingPath';
  WriteLn('Hello Static Web!');
end.
