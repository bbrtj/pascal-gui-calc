unit CalcState;

{$mode objfpc}{$H+}{$J-}

interface

uses
	Classes, SysUtils, StrUtils, Fgl, Controls, Math,
	PN, PNBase;


type
	TCalcHandler = class
	private
		FName: ShortString;
		FRenamed: Boolean;
		FParser: TPN;
		FFrame: TControl;
		FCalculated: Double;

		procedure SetName(NewName: ShortString);
		function IsNameDefault(): Boolean;
        function FindVariableOccurrence(const Expr, Name: String): Integer;
	public
		constructor Create(const HandlerName: String; Frame: TControl);
		destructor Destroy; override;

		function Calculate(const Expr: String): String;
        function RenameVariable(const Expr, OldName, NewName: String): String;

		property Name: ShortString read FName write SetName;
		property DefaultName: Boolean read IsNameDefault;
		property Frame: TControl read FFrame write FFrame;
		property LastCalculated: Double read FCalculated;
	end;

	TCalcHandlerList = specialize TFPGObjectList<TCalcHandler>;

	TCalcState = class
	private
		FCalcs: TCalcHandlerList;
		FMemory: String;
		FDirty: Boolean;
		FSavedAs: String;
	public
		constructor Create();
		destructor Destroy; override;

		function AddCalculator(const HandlerName: String; Frame: TControl): TCalcHandler;
		function AddCalculator(const Calc: TCalcHandler): TCalcHandler;
		function RemoveCalculator(Calc: TCalcHandler): TControl;
		procedure SetVariablesAndConstants(Parser: TPN);

		property AllCalculators: TCalcHandlerList read FCalcs;
		property Memory: String read FMemory write FMemory;
		property Dirty: Boolean read FDirty write FDirty;
		property SavedAs: String read FSavedAs write FSavedAs;
	end;

var
	GlobalCalcState: TCalcState;

implementation

uses CalcFrame;

constructor TCalcHandler.Create(const HandlerName: String; Frame: TControl);
begin
	FParser := TPN.Create;
	FName := HandlerName;
	FRenamed := false;
	FFrame := Frame;
	FCalculated := 0;
end;

destructor TCalcHandler.Destroy;
begin
	FParser.Free;
end;

procedure TCalcHandler.SetName(NewName: ShortString);
begin
	FName := NewName;
	FRenamed := true;
end;

function TCalcHandler.IsNameDefault(): Boolean;
begin
	result := not FRenamed;
end;


function TCalcHandler.Calculate(const Expr: String): String;
var
	LFormat: TFormatSettings;
begin
	LFormat.DecimalSeparator := cDecimalSeparator;

	FParser.ParseString(Expr);
	GlobalCalcState.SetVariablesAndConstants(FParser);
	FCalculated := FParser.GetResult;
	result := FloatToStr(FCalculated, LFormat);
end;

function TCalcHandler.RenameVariable(const Expr, OldName, NewName: String): String;
var
    FoundPosition: Integer;
begin
    result := Expr;
    while True do begin
    	FoundPosition := self.FindVariableOccurrence(result, OldName);
        if FoundPosition < 1 then break;
		result := StuffString(result, FoundPosition, Length(OldName), NewName);
	end;
end;

function TCalcHandler.FindVariableOccurrence(const Expr, Name: String): Integer;
var
    CurrentItem: TItem;
begin
    FParser.ParseString(Expr);

    while not FParser.Stack.Empty() do begin
		CurrentItem := FParser.Stack.Pop();
		if CurrentItem.ItemType <> itVariable then continue;
		if CurrentItem.VariableName = Name then exit(CurrentItem.ParsedAt);
	end;

	result := 0;
end;
constructor TCalcState.Create();
begin
	FCalcs := TCalcHandlerList.Create;

	FMemory := '';
	FDirty := False;
	FSavedAs := '';
end;

destructor TCalcState.Destroy;
begin
	FCalcs.Free;
end;

function TCalcState.AddCalculator(const HandlerName: String; Frame: TControl): TCalcHandler;
begin
	result := self.AddCalculator(TCalcHandler.Create(HandlerName, Frame));
end;

function TCalcState.AddCalculator(const Calc: TCalcHandler): TCalcHandler;
begin
	result := Calc;
	FCalcs.Add(result);
end;

function TCalcState.RemoveCalculator(Calc: TCalcHandler): TControl;
begin
	result := Calc.Frame;
	FCalcs.Remove(Calc);
end;

procedure TCalcState.SetVariablesAndConstants(Parser: TPN);
var
	LCalc: TCalcHandler;
begin
	Parser.ClearVariables;

	// constants
	Parser.DefineVariable('PI', PI);
	Parser.DefineVariable('PHI', (1 + 5 ** 0.5) / 2);

	for LCalc in FCalcs do
		Parser.DefineVariable(LCalc.Name, LCalc.LastCalculated);
end;

initialization
	GlobalCalcState := TCalcState.Create;

finalization
	GlobalCalcState.Free;
end.

