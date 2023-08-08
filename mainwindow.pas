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
		ActionCalculate: TAction;
		ActionExitProgram: TAction;
		ActionCalculateAll: TAction;
		ActionShortcuts: TActionList;
		MainMenu: TMainMenu;
		MenuItemCalculateAll: TMenuItem;
		MenuItemAddCalculator: TMenuItem;
		MenuItemSyntax: TMenuItem;
		MenuItemHelp: TMenuItem;
		MenuItemCalculator: TMenuItem;
		MenuItemFile: TMenuItem;
		MenuItemNew: TMenuItem;
		MenuItemOpen: TMenuItem;
		MenuItemSave: TMenuItem;
		MenuItemExit: TMenuItem;
		ActionNewCalculator: TAction;
		Separator1: TMenuItem;
		Separator2: TMenuItem;
		procedure ActionCalculateExecute(Sender: TObject);
  		procedure ActionNewCalculatorExecute(Sender: TObject);
		procedure ActionCalculateAllExecute(Sender: TObject);
		procedure ActionExitProgramExecute(Sender: TObject);
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

procedure TMainForm.ActionNewCalculatorExecute(Sender: TObject);
begin
	AddCalculator();
end;

procedure TMainForm.ActionCalculateExecute(Sender: TObject);
var
	CalcHandler: TCalcHandler;
begin
	for CalcHandler in GlobalCalcState.AllCalculators do begin
		if TCalcView(CalcHandler.Frame).IsSelected then
			TCalcView(CalcHandler.Frame).Calculate;
	end;
end;

procedure TMainForm.ActionCalculateAllExecute(Sender: TObject);
var
	CalcHandler: TCalcHandler;
begin
	for CalcHandler in GlobalCalcState.AllCalculators do begin
		TCalcView(CalcHandler.Frame).Calculate;
	end;
end;

procedure TMainForm.ActionExitProgramExecute(Sender: TObject);
begin
	Close;
end;

end.

