unit CalcState;

{$mode ObjFPC}{$H+}

interface

uses
	Classes, SysUtils, Fgl, Controls,
	PN;


type
	TCalcHandler = class
	private
		FName: String[20];
		FParser: TPN;
		FFrame: TControl;
		FCalculated: Double;
	public
		constructor Create(const vName: String; vFrame: TControl);
		destructor Destroy; override;

		function Calculate(const vExpr: String): String;

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
	// TODO: Add variables
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

initialization
	GlobalCalcState := TCalcState.Create;

finalization
	GlobalCalcState.Free;
end.

