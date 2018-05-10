unit nclex;

{$mode objfpc}{$H+}

interface

uses chardfa;

type
    ELexTokenType = (
        (* None, is used when there is no Token needed *)
        ENcLexNone,

        ENcLexTagOpen, ENcLexTagClose, ENcLexIdent, ENcLexString, ENcLexNum,
        ENcLexTagSlash, ENcLexText, ENcLexSpecial
        );
    
    PNcLexToken = ^TNcLexToken;
    TNcLexToken = record
        TokenType : ELexTokenType;
        Name : AnsiString;
        LexValue : AnsiString;
        LineNum : Cardinal;
        CharNum : Cardinal;
    end;
    
    TNcLexTokenArray = Array of TNcLexToken;
    
    TNcLexer = class(TObject)
        private
            FDfa : TCharDFA;
            FCurChar, FCurLine: Cardinal;
            FTokenList : TNcLexTokenArray;
        public
            constructor Create();
            destructor Destroy(); Override;

            procedure HandleDFAToken(token : PDFAToken);
            
            function Lex(s : AnsiString) : TNcLexTokenArray;
    end;

implementation

uses sysutils, lazutf8;

var
    // CharList : Array[0..1] of AnsiString = ('a', '四');
    DigitCL : Array[0..9] of AnsiString = ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9');
    HexCL : Array[0..15] of AnsiString = ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
    LowerAlphaCL : Array[0..25] of AnsiString = (
        'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 
        'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 
        'u', 'v', 'w', 'x', 'y', 'z');
        
    UpperAlphaCL : Array[0..25] of AnsiString = (
        'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 
        'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 
        'U', 'V', 'W', 'X', 'Y', 'Z');
    
    TextOtherCL : Array[0..19] of AnsiString = (
        '-', '_', '(', ')', '*', '^', '%', '$', '#', '@',
        '!', '~', '`', ',', '.', '?', '\', '|', ':', ';'
        );
        

    WhitespaceCL : Array[0..3] of AnsiString = (#13, #10, #7, #32);


constructor TNcLexer.Create();
var
    StartState, WhitespaceState,
    StartTag, TagSlash, TagIdent, 
    TagAttr, TagString, TagNumber, 
    TagEnd: TDFAState;

begin
    self.FCurLine := 1;
    self.FCurChar := 0;
    SetLength(self.FTokenList, 0);

    FDfa := TCharDFA.Create();

    (* Assign the LexToken Generator *)
    FDfa.SetTokenHandler(@Self.HandleDFAToken);
    
    (* configure DFA to Lex LnfwSource *)
    StartState := TDFAState.Create('START', 'START', Integer(ENcLexNone));
    WhitespaceState := TDFAState.Create('WHITESPACE', 'WS', Integer(ENcLexNone));

    //CommentState := TDFAState.Create('COMMENT', 'CMNT', Integer(ELfnwLexComment));
    //CommentEndState := TDFAState.Create('COMMENT', 'CMNT', Integer(ELfnwLexComment));


    FDfa.addState(StartState); (* Must add the First "Start" State, before all others *)
    FDfa.addState(WhitespaceState);
    
    FDfa.AddState(StartTag);


    (* Loop whitespace back to start, we don't care about it *)
    StartState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(WhitespaceCL), StartState, False));

    (* Handle comments *)
    {StartState.AddDelta(TDFADelta.Create(TDFAComp_Is.Create(';'), CommentState, False));
    CommentState.AddDelta(TDFADelta.Create(TDFAComp_IsNot.Create(#10), CommentState));
    CommentState.AddDelta(TDFADelta.Create(TDFAComp_Is.Create(#10), CommentEndState, False));

    (* Handle Register *)
    StartState.AddDelta(TDFADelta.Create(TDFAComp_Is.Create('R'), RegisterState));
    RegisterState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(DigitCL), RegisterNumberState));
    RegisterState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(UpperAlphaCL), OpState));
    RegisterNumberState.AddDelta(TDFADelta.Create(TDFAComp_IsNotIn.Create(DigitCL), RegisterEndState, False, True));

    (* Handle Ops *)
    StartState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(UpperAlphaCL), OpState));
    OpState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(UpperAlphaCL), OpState));
    OpState.AddDelta(TDFADelta.Create(TDFAComp_IsNotIn.Create(UpperAlphaCL), OpEndState, False, True));

    (* Handle Hex Literal *)
    StartState.AddDelta(TDFADelta.Create(TDFAComp_Is.Create('x'), LiteralHexState, False));
    LiteralHexState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(LowerAlphaCL), LabelState));
    LiteralHexState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(HexCL), LiteralHexDigitState));
    LiteralHexDigitState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(HexCL), LiteralHexDigitState));
    LiteralHexDigitState.AddDelta(TDFADelta.Create(TDFAComp_IsNotIn.Create(HexCL), LiteralHexEndState, False, True));

    (* Handle Dec Literal *)
    StartState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(DigitCL), LiteralDecState));
    LiteralDecState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(DigitCL), LiteralDecState));
    LiteralDecState.AddDelta(TDFADelta.Create(TDFAComp_IsNotIn.Create(DigitCL), LiteralDecEndState, False, True));


    (* Handle Address *)
    StartState.AddDelta(TDFADelta.Create(TDFAComp_Is.Create('@'), AddressState));

    (* Handle Label *)
    StartState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(LowerAlphaCL), LabelState));
    LabelState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(LowerAlphaCL), LabelState));
    LabelState.AddDelta(TDFADelta.Create(TDFAComp_Is.Create('_'), LabelState));
    LabelState.AddDelta(TDFADelta.Create(TDFAComp_Is.Create(':'), LabelDefState, False));
    LabelState.AddDelta(TDFADelta.Create(TDFAComp_IsIn.Create(WhitespaceCL), LabelEndState, False));
    }
    
end;


destructor TNcLexer.Destroy();
begin
    FreeAndNil(Fdfa);
    SetLength(self.FTokenList, 0);
    
    inherited Destroy();
end;

procedure TNcLexer.HandleDFAToken(token : PDFAToken);
var pToken : PNcLexToken;
    i : Integer;
begin
  WriteLn('#TOKEN: ', token^.TokenName, ' -> ', token^.TokenVal);

  i := Length(self.FTokenList);
  SetLength(self.FTokenList, i + 1);
  pToken := @self.FTokenList[i];

  pToken^.LexValue := token^.TokenVal;
  pToken^.TokenType := ELexTokenType(token^.TokenId);
  pToken^.Name := token^.TokenName;

end;

function TNcLexer.Lex(s : AnsiString) : TNcLexTokenArray;
var
    len : Integer;
    curCodePoint : AnsiString;
    curP, endP : PChar;
    reprocessCodePoint : Boolean;
begin
    curP := PChar(s);
    endP := curP + Length(s);
    
    while curP < endP do
    begin
        len := UTF8CodePointSize(CurP);
        SetLength(curCodePoint, len);
        Move(curP^, curCodePoint[1], len);
        
        if curCodePoint = #10 then
        begin
            self.FCurChar := 0;
            self.FCurLine := self.FCurLine + 1;
        end
        else if curCodePoint = #13 then
        begin
            (* Ignore cariage return *)
        end
        else
        begin
            Inc(self.FCurChar);
            //WriteLn('Line: ', curLineNum, ', Char: ', curCharNum, ', => ', curCodePoint);
        end;
        //Write(curCodePoint);

        reprocessCodePoint := False;
        
        (* Pass char into dfa state *)
        if not self.FDfa.nextChar(curCodePoint, reprocessCodePoint) then
        begin
            WriteLn('Error: no debugging info yet.');
        end;

        if not reprocessCodePoint then
            Inc(curP, len)
        else
        begin

        end;
    end;

    Result := self.FTokenList;

end;

end.
