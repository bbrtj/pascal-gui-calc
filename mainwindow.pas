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
		ActionList1: TActionList;
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
	self.AddCalculator();
end;

end.

