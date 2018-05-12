program nocms;

{$mode objfpc}{$H+}

uses 
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  SysUtils, StrUtils, lazutf8, nclex;

var
    sitePath : AnsiString;
    lex : TNcLexer;

begin
  
  sitePath := 'TestingPath';

  (* Create the lexer instance and lex our site *)
  lex := TNcLexer.Create();

  WriteLn('Hello Static Web!');
end.
