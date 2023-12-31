unit CalcState;

{$mode objfpc}{$H+}{$J-}

interface

uses
	Classes, SysUtils, Fgl, Controls, Math,
	PN, PNBase;


type
	TCalcHandler = class
	private
		FName: ShortString;
		FParser: TPN;
		FFrame: TControl;
		FCalculated: Double;
	public
		constructor Create(const vName: String; vFrame: TControl);
		destructor Destroy; override;

		function Calculate(const vExpr: String): String;

		property Name: ShortString read FName write FName;
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

		function AddCalculator(const vName: String; vFrame: TControl): TCalcHandler;
		function AddCalculator(const vCalc: TCalcHandler): TCalcHandler;
		function RemoveCalculator(vCalc: TCalcHandler): TControl;
		procedure SetVariablesAndConstants(vParser: TPN);

		property AllCalculators: TCalcHandlerList read FCalcs;
		property Memory: String read FMemory write FMemory;
		property Dirty: Boolean read FDirty write FDirty;
		property SavedAs: String read FSavedAs write FSavedAs;
	end;

var
	GlobalCalcState: TCalcState;

implementation

constructor TCalcHandler.Create(const vName: String; vFrame: TControl);
begin
	FParser := TPN.Create;
	FName := vName;
	FFrame := vFrame;
	FCalculated := 0;
end;

destructor TCalcHandler.Destroy;
begin
	FParser.Free;
end;

function TCalcHandler.Calculate(const vExpr: String): String;
var
	vFormat: TFormatSettings;
begin
	vFormat.DecimalSeparator := cDecimalSeparator;

	FParser.ParseString(vExpr);
	GlobalCalcState.SetVariablesAndConstants(FParser);
	FCalculated := FParser.GetResult;
	result := FloatToStr(FCalculated, vFormat);
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

function TCalcState.AddCalculator(const vName: String; vFrame: TControl): TCalcHandler;
begin
	result := self.AddCalculator(TCalcHandler.Create(vName, vFrame));
end;

function TCalcState.AddCalculator(const vCalc: TCalcHandler): TCalcHandler;
begin
	result := vCalc;
	FCalcs.Add(result);
end;

function TCalcState.RemoveCalculator(vCalc: TCalcHandler): TControl;
begin
	result := vCalc.Frame;
	FCalcs.Remove(vCalc);
end;

procedure TCalcState.SetVariablesAndConstants(vParser: TPN);
var
	vCalc: TCalcHandler;
begin
	vParser.ClearVariables;

	// constants
	vParser.DefineVariable('PI', PI);
	vParser.DefineVariable('PHI', (1 + 5 ** 0.5) / 2);

	for vCalc in FCalcs do
		vParser.DefineVariable(vCalc.Name, vCalc.LastCalculated);
end;

initialization
	GlobalCalcState := TCalcState.Create;

finalization
	GlobalCalcState.Free;
end.

