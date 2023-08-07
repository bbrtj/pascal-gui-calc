unit MainWindow;

{$mode ObjFPC}{$H+}

interface

uses
	Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ActnList,
	ExtCtrls,
	CalcFrame, CalcState;

type

	{ TMainForm }

 TMainForm = class(TForm)
		DoCalc: TAction;
		DoCalcAll: TAction;
		ActionShortcuts: TActionList;
		NewCalc: TAction;
		procedure AddCalculator(Sender: TObject);
		procedure CalculateAll(Sender: TObject);
		procedure CalculateOne(Sender: TObject);
		procedure FormCreate(Sender: TObject);
	private
		procedure AddCalculator();
	public


	end;

var
	MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.AddCalculator();
var
	CalcView: TCalcView;
begin
	CalcView := TCalcView.Create(self);
	self.InsertControl(CalcView);

	CalcView.Handler := GlobalCalcState.AddCalculator(
		CalcView.Expression.Caption, CalcView
	);
end;

procedure TMainForm.FormCreate(Sender: TObject);

begin
	self.AddCalculator();
end;

procedure TMainForm.AddCalculator(Sender: TObject);
begin
	AddCalculator();
end;

procedure TMainForm.CalculateAll(Sender: TObject);
var
	CalcHandler: TCalcHandler;
begin
	for CalcHandler in GlobalCalcState.AllCalculators do begin
		TCalcView(CalcHandler.Frame).Calculate;
	end;
end;

procedure TMainForm.CalculateOne(Sender: TObject);
var
	CalcHandler: TCalcHandler;
begin
	for CalcHandler in GlobalCalcState.AllCalculators do begin
		if TCalcView(CalcHandler.Frame).IsSelected then
			TCalcView(CalcHandler.Frame).Calculate;
	end;
end;

end.

