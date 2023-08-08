unit MainWindow;

{$mode ObjFPC}{$H+}

interface

uses
	Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ActnList,
	ExtCtrls, Menus, LCLIntf,
	CalcFrame, CalcState;

type

	{ TMainForm }

 TMainForm = class(TForm)
		ExitProgram: TAction;
		DoCalc: TAction;
		DoCalcAll: TAction;
		ActionShortcuts: TActionList;
		MainMenu: TMainMenu;
		MenuItemCalculateAll: TMenuItem;
		MenuItemAddCalculator: TMenuItem;
		MenuItemSyntax: TMenuItem;
		MenuItemHelp: TMenuItem;
		MenuItemCalculate: TMenuItem;
		MenuItemCalculator: TMenuItem;
		MenuItemFile: TMenuItem;
		MenuItemNew: TMenuItem;
		MenuItemOpen: TMenuItem;
		MenuItemSave: TMenuItem;
		MenuItemExit: TMenuItem;
		NewCalc: TAction;
		Separator1: TMenuItem;
		Separator2: TMenuItem;
		procedure NewCalcExecute(Sender: TObject);
		procedure DoCalcAllExecute(Sender: TObject);
		procedure DoCalcExecute(Sender: TObject);
		procedure ExitProgramExecute(Sender: TObject);
		procedure FormCreate(Sender: TObject);
	private
		procedure AddCalculator();
		procedure UpdatePosition;
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
	UpdatePosition;

	CalcView := TCalcView.Create(self);
	self.InsertControl(CalcView);

	CalcView.Handler := GlobalCalcState.AddCalculator(
		CalcView.Expression.Caption, CalcView
	);
end;

procedure TMainForm.UpdatePosition;
var
	rect: TRect;
begin
	// Workaround for Left and Top not updating in some WMs
    GetWindowRect(self.Handle, rect);
    self.Left := rect.Left;
    self.Top := rect.Top;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
	self.AddCalculator();
end;

procedure TMainForm.NewCalcExecute(Sender: TObject);
begin
	AddCalculator();
end;

procedure TMainForm.DoCalcAllExecute(Sender: TObject);
var
	CalcHandler: TCalcHandler;
begin
	for CalcHandler in GlobalCalcState.AllCalculators do begin
		TCalcView(CalcHandler.Frame).Calculate;
	end;
end;

procedure TMainForm.DoCalcExecute(Sender: TObject);
var
	CalcHandler: TCalcHandler;
begin
	for CalcHandler in GlobalCalcState.AllCalculators do begin
		if TCalcView(CalcHandler.Frame).IsSelected then
			TCalcView(CalcHandler.Frame).Calculate;
	end;
end;

procedure TMainForm.ExitProgramExecute(Sender: TObject);
begin
	Close;
end;

end.

