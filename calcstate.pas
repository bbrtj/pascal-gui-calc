unit CalcState;

{$mode ObjFPC}{$H+}

interface

uses
	Classes, SysUtils, Fgl, Controls,
	PN;


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
	public
		constructor Create();
		destructor Destroy; override;

		function AddCalculator(const vName: String; vFrame: TControl): TCalcHandler;
		function RemoveCalculator(vCalc: TCalcHandler): TControl;
		procedure SetVariables(vParser: TPN);

		property AllCalculators: TCalcHandlerList read FCalcs;
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
begin
	FParser.ParseString(vExpr);
	GlobalCalcState.SetVariables(FParser);
	// TODO: try/catch
	FCalculated := FParser.GetResult;
	result := FloatToStr(FCalculated);
end;

constructor TCalcState.Create();
begin
	FCalcs := TCalcHandlerList.Create;
end;

destructor TCalcState.Destroy;
begin
	FCalcs.Free;
end;

function TCalcState.AddCalculator(const vName: String; vFrame: TControl): TCalcHandler;
begin
	result := TCalcHandler.Create(vName, vFrame);
	FCalcs.Add(result);
end;

function TCalcState.RemoveCalculator(vCalc: TCalcHandler): TControl;
begin
	result := vCalc.Frame;
	FCalcs.Remove(vCalc);
end;

procedure TCalcState.SetVariables(vParser: TPN);
var
	vCalc: TCalcHandler;
begin
	vParser.ClearVariables;

	for vCalc in FCalcs do
		vParser.DefineVariable(vCalc.Name, vCalc.LastCalculated);
end;

initialization
	GlobalCalcState := TCalcState.Create;

finalization
	GlobalCalcState.Free;
end.

