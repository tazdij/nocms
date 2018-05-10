unit chardfa;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils;

type
    PDFAToken = ^TDFAToken;
    TDFAToken = record
        TokenId : Integer;
        TokenVal : AnsiString;
        TokenName: AnsiString;
        TokenCharStart : Integer;
        TokenLine : Integer;
    end;

    TDFATokenHandler = procedure(ADfaToken : PDFAToken) of object;

    TDFAComparator = class(TObject)
        public
            procedure Free(); Virtual; Abstract;
            function Compare(AInput : AnsiString) : Boolean; Virtual; Abstract;
    end;

    TDFAComp_IsIn = class(TDFAComparator)
        private
            FCharList : Array of AnsiString;
        public
            constructor Create(ACharList : Array of AnsiString);
            destructor Destroy(); Override;
            
            procedure Free(); Override;
            function Compare(AInput : AnsiString) : Boolean; Override;
    end;
    
    TDFAComp_IsNotIn = class(TDFAComparator)
        private
            FCharList : Array of AnsiString;
        public
            constructor Create(ACharList : Array of AnsiString);
            destructor Destroy(); Override;
            
            procedure Free(); Override;
            function Compare(AInput : AnsiString) : Boolean; Override;
    end;

    TDFAComp_Is = class(TDFAComparator)
        private
            FChar : AnsiString;
        public
            constructor Create(AChar : AnsiString);
            destructor Destroy(); Override;

            procedure Free(); Override;
            function Compare(AInput : AnsiString) : Boolean; Override;
    end;

    TDFAComp_IsNot = class(TDFAComparator)
        private
            FChar : AnsiString;
        public
            constructor Create(AChar : AnsiString);
            destructor Destroy(); Override;

            procedure Free(); Override;
            function Compare(AInput : AnsiString) : Boolean; Override;
    end;
    
    TDFAComp_And = class(TDFAComparator)
        private
            FComparators : Array of TDFAComparator;
        public
            constructor Create(AComps : Array of TDFAComparator);
            destructor Destroy(); Override;
            
            procedure Free(); Override;
            function Compare(AInput : AnsiString) : Boolean; Override;
    end;
    
    TDFAComp_Or = class(TDFAComparator)
        private
            FComparators : Array of TDFAComparator;
        public
            constructor Create(AComps : Array of TDFAComparator);
            destructor Destroy(); Override;
            
            procedure Free(); Override;
            function Compare(AInput : AnsiString) : Boolean; Override;
    end;
    
    TDFAState = class;

    TDFADelta = class
        private
            FComparator : TDFAComparator;
            FDestination : TDFAState;
            FAddToBuffer : Boolean;
            FReprocess : Boolean;

        public
            constructor Create(AComparator : TDFAComparator; ADestination : TDFAState; AAddToBuffer : Boolean = True; AReprocess : Boolean = False);
            destructor Destroy(); Override;
    end;

    TCharDFA = class;
    
    TDFAState = class(TObject)
        private
            FDeltas : Array of TDFADelta;
            FName : AnsiString;
            FIdent : AnsiString;
            FTokenId : Integer;
            FDfa : TCharDFA;

            function GetIsLeaf() : Boolean;
        public
            function ProcessChar(c : AnsiString;  var reprocess : Boolean) : Boolean;
            procedure AddDelta(delta : TDFADelta);
            constructor Create(AName : AnsiString; AIdent : AnsiString; ATokenId : Integer);
            destructor Destroy(); Override;

            property IsLeaf : Boolean read GetIsLeaf;
            
            
    end;

    TCharDFA = class
        private
            FStates : Array of TDFAState;
            FCurState : TDFAState;
            FStartState : TDFAState;
            FBuffer : AnsiString;
            FTokenHandler : TDFATokenHandler;
        protected
            
        public
            constructor Create();
            destructor Destroy(); Override;
            
            procedure AddState(state : TDFAState);
            procedure SetTokenHandler(handler : TDFATokenHandler);
            function nextChar(c : AnsiString; var reprocess : Boolean) : Boolean;
    end;

implementation

constructor TDFAComp_IsIn.Create(ACharList : Array of AnsiString);
var 
    el : AnsiString;
    i : Integer;
begin
    inherited Create();
    SetLength(Self.FCharList, Length(ACharList));
    i := 0;

    for el in ACharList do
    begin
       Self.FCharList[i] := Copy(el, 1, 1);
       Inc(i); 
    end;

end;

destructor TDFAComp_IsIn.Destroy();
begin
    inherited Destroy();
end;

procedure TDFAComp_IsIn.Free();
begin
    SetLength(Self.FCharList, 0);
    Self.FCharList := Nil;
end;

function TDFAComp_IsIn.Compare(AInput : AnsiString) : Boolean;
var
    el : AnsiString;
begin
    Result := False;
    for el in Self.FCharList do
    begin
        if el = AInput then
        begin
            Result := True;
            break;
        end;
    end;
end;

constructor TDFAComp_IsNotIn.Create(ACharList : Array of AnsiString);
var 
    el : AnsiString;
    i : Integer;
begin
    inherited Create();
    SetLength(Self.FCharList, Length(ACharList));
    i := 0;

    for el in ACharList do
    begin
       Self.FCharList[i] := Copy(el, 1, 1);
       Inc(i); 
    end;

end;

destructor TDFAComp_IsNotIn.Destroy();
begin
    inherited Destroy();
end;

procedure TDFAComp_IsNotIn.Free();
begin
    SetLength(Self.FCharList, 0);
    Self.FCharList := Nil;
end;

function TDFAComp_IsNotIn.Compare(AInput : AnsiString) : Boolean;
var
    el : AnsiString;
begin
    Result := True;
    for el in Self.FCharList do
    begin
        if el = AInput then
        begin
            Result := False;
            break;
        end;
    end;
end;

constructor TDFAComp_Is.Create(AChar : AnsiString);
begin
    inherited Create();

    self.FChar := AChar;
end;

destructor TDFAComp_Is.Destroy();
begin
    inherited Destroy();
end;

procedure TDFAComp_Is.Free();
begin
    Self.FChar := '';
end;

function TDFAComp_Is.Compare(AInput : AnsiString) : Boolean;
var
    el : AnsiString;
begin
    Result := True;

    if AnsiCompareStr(AInput, FChar) <> 0 then
        Result := False;
end;


constructor TDFAComp_IsNot.Create(AChar : AnsiString);
begin
    inherited Create();

    self.FChar := AChar;
end;

destructor TDFAComp_IsNot.Destroy();
begin
    inherited Destroy();
end;

procedure TDFAComp_IsNot.Free();
begin
    Self.FChar := '';
end;

function TDFAComp_IsNot.Compare(AInput : AnsiString) : Boolean;
var
    el : AnsiString;
begin
    Result := True;

    if AnsiCompareStr(AInput, FChar) = 0 then
        Result := False;
end;



constructor TDFAComp_And.Create(AComps : Array of TDFAComparator);
var i : Integer;
    comp : TDFAComparator;
begin
    inherited Create();
    
    SetLength(Self.FComparators, Length(AComps));
    i := 0;
    
    for comp in AComps do
    begin
        Self.FComparators[i] := comp;
        Inc(i);
    end;
    
end;

destructor TDFAComp_And.Destroy();
begin

    inherited Destroy();
end;

procedure TDFAComp_And.Free();
begin

end;

function TDFAComp_And.Compare(AInput : AnsiString) : Boolean;
var comp : TDFAComparator;
begin
    Result := True;
    for comp in self.FComparators do
    begin
        if not comp.Compare(AInput) then
        begin
            Result := False;
            break;
        end;
    end;
end;

constructor TDFAComp_Or.Create(AComps : Array of TDFAComparator);
var i : Integer;
begin
    inherited Create();

    SetLength(Self.FComparators, Length(AComps));

    for i := 0 to Length(AComps) - 1 do
    begin
        self.FComparators[i] := AComps[i];
    end;
end;

destructor TDFAComp_Or.Destroy();
begin
    WriteLn('Destroy Or');
    inherited Destroy();
end;

procedure TDFAComp_Or.Free();
var comp : TDFAComparator;
    i : Integer;
begin
    WriteLn('Freeing Or');
    for i := 0 to Length(Self.FComparators) - 1 do
    begin
        comp := Self.FComparators[i];
        //Self.FComparators[i] := nil;
        Self.FComparators[i].Free();
        FreeAndNil(Self.FComparators[i]);
        //comp := nil;
    end;
    
    SetLength(Self.FComparators, 0);
    Self.FComparators := nil;
end;

function TDFAComp_Or.Compare(AInput : AnsiString) : Boolean;
var comp : TDFAComparator;
begin
    Result := False;
    for comp in self.FComparators do
    begin
        if comp.Compare(AInput) then
        begin
            Result := True;
            break;
        end;
    end;
end;

constructor TDFADelta.Create(AComparator : TDFAComparator; ADestination : TDFAState; AAddToBuffer : Boolean = True; AReprocess : Boolean = False);
begin
     self.FComparator := AComparator;
     self.FDestination := ADestination;
     self.FAddToBuffer := AAddToBuffer;
     self.FReprocess := AReprocess;
end;

destructor TDFADelta.Destroy();
begin
    FreeAndNil(self.FComparator);

    inherited Destroy();
end;

constructor TDFAState.Create(AName : AnsiString; AIdent : AnsiString; ATokenId : Integer);
begin
     FName := AName;
     FIdent := AIdent;
     FTokenId := ATokenId;
end;

destructor TDFAState.Destroy();
var curDelta : TDFADelta;
    i : Integer;
begin
    for i := 0 to Length(self.FDeltas) - 1 do
    begin
        curDelta := self.FDeltas[i];
        FreeAndNil(curDelta);
    end;
    
    inherited Destroy();
end;

function TDFAState.GetIsLeaf() : Boolean;
begin
    Result := False;

    if Length(self.FDeltas) = 0 then
       Result := True;
end;

function TDFAState.ProcessChar(c : AnsiString; var reprocess : Boolean) : Boolean;
var delta : TDFADelta;
begin
    Result := False;

    for delta in self.FDeltas do
    begin
      if delta.FComparator.Compare(c) then
      begin
          Result := True;

          (* We need to handle the transition *)
          if delta.FReprocess then
          begin
              (* Handle reversing the buffer, somehow *)
              reprocess := True;
          end;

          if delta.FAddToBuffer then
          begin
              (* Add the current character to the DFA Buffer for current token *)
              self.FDfa.FBuffer := self.FDfa.FBuffer + c;
          end;

          (* Move to Dest state *)
          self.FDfa.FCurState := delta.FDestination;


          Exit;
      end;
    end;
end;

procedure TDFAState.AddDelta(delta : TDFADelta);
var numDeltas : Integer;
begin
    numDeltas := Length(self.FDeltas);
    SetLength(self.FDeltas, numDeltas + 1);
    self.FDeltas[numDeltas] := delta;
end;

constructor TCharDFA.Create();
begin

end;

destructor TCharDFA.Destroy();
var state : TDFAState;
    i : Integer;
begin
    (* Free up all States *)
    for i := 0 to Length(self.FStates) - 1 do
    begin
        state := self.FStates[i];
        (* Call free on state, then freeandnil *)
        //state.Free();
        FreeAndNil(State);

    end;


    inherited Destroy();
end;

procedure TCharDFA.AddState(state : TDFAState);
var numStates : Integer;
begin
    numStates := Length(self.FStates);
    SetLength(self.FStates, numStates + 1);
    self.FStates[numStates] := state;

    if not Assigned(self.FCurState) then
       self.FCurState := state;

    if not Assigned(self.FStartState) then
       self.FStartState := state;

    // Put a reference to this DFA in the state (used for changing current state)
    state.FDfa := self;
end;

procedure TCharDFA.SetTokenHandler(handler : TDFATokenHandler);
begin
    self.FTokenHandler := handler;
end;

function TCharDFA.nextChar(c : AnsiString; var reprocess : Boolean) : Boolean;
var
    pToken : PDFAToken;
begin

    (* Test each Delta in the current state, if it can process this char *)
    Result := FCurState.ProcessChar(c, reprocess);

    if Result = True then
    begin
        (* Test if state is a leaf *)
        if self.FCurState.IsLeaf then
        begin
            (* This is a dead end, it is time to generate a token, clear buffer, and got to StartState *)
            New(pToken);

            pToken^.TokenName := self.FCurState.FName;
            pToken^.TokenId := self.FCurState.FTokenId;
            pToken^.TokenVal := Copy(self.FBuffer, 1, Length(self.FBuffer));

            self.FTokenHandler(pToken);

            Dispose(pToken);

            self.FBuffer := '';

            (* Go Back to StartState *)
            self.FCurState := self.FStartState;
        end;
    end;

    (* return if the char was processed correctly *)
    //Result := True;
end;

end.
