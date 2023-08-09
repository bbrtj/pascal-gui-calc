unit MainWindow;

{$mode ObjFPC}{$H+}

interface

uses
	Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ActnList,
	ExtCtrls, Menus, LCLIntf, StrUtils,
	CalcFrame, CalcState;

type

	{ TMainForm }

 TMainForm = class(TForm)
		ActionSaveAs: TAction;
		ActionNew: TAction;
		ActionOpen: TAction;
		ActionSave: TAction;
		ActionCalculate: TAction;
		ActionExitProgram: TAction;
		ActionCalculateAll: TAction;
		ActionShortcuts: TActionList;
		MainMenu: TMainMenu;
		MenuItemSaveAs: TMenuItem;
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
		OpenDialog: TOpenDialog;
		SaveDialog: TSaveDialog;
		Separator1: TMenuItem;
		Separator2: TMenuItem;
		procedure ActionCalculateExecute(Sender: TObject);
  		procedure ActionNewCalculatorExecute(Sender: TObject);
		procedure ActionCalculateAllExecute(Sender: TObject);
		procedure ActionExitProgramExecute(Sender: TObject);
		procedure ActionNewExecute(Sender: TObject);
		procedure ActionOpenExecute(Sender: TObject);
		procedure ActionSaveAsExecute(Sender: TObject);
		procedure ActionSaveExecute(Sender: TObject);
		procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
		procedure FormCreate(Sender: TObject);
	private
		procedure AddCalculator(const customName: String = ''; const Content: String = '');
		procedure UpdatePosition;
		function CheckDirty: Boolean;

		procedure ClearCalculators();

		procedure LoadFromFile;
		procedure SaveToFile;
	public


	end;

var
	MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.AddCalculator(const customName: String; const Content: String);
var
	CalcView: TCalcView;
begin
	CalcView := TCalcView.Create(self, customName);
	self.InsertControl(CalcView);
	CalcView.Content := Content;
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

function TMainForm.CheckDirty: Boolean;
begin
	result := True;
	if GlobalCalcState.Dirty then begin
		case MessageDlg(
			'Would you like to save changes?',
			mtConfirmation,
			mbYesNoCancel,
			0,
			mbCancel
		) of
			mrYes: ActionSave.Execute;
			mrNo: GlobalCalcState.Dirty := False;
			mrCancel: exit(False);
		end;

		result := not GlobalCalcState.Dirty;
	end;
end;

procedure TMainForm.ClearCalculators;
var
	CalcHandler: TCalcHandler;
begin
	ResetNumbers;

	for CalcHandler in GlobalCalcState.AllCalculators do begin
		self.RemoveControl(CalcHandler.Frame);
		CalcHandler.Frame.Free;
	end;

	GlobalCalcState.AllCalculators.Clear;
end;

procedure TMainForm.LoadFromFile;
var
	FileContents: TStringList;
	Line: String;
	LineParts: TStringArray;
begin
	FileContents := TStringList.Create;
	FileContents.LoadFromFile(GlobalCalcState.SavedAs);

	for Line in FileContents do begin
		LineParts := SplitString(Line, ':');
		if length(LineParts) <> 2 then
			continue;
		if length(LineParts[0]) = 0 then
			continue;

		self.AddCalculator(LineParts[0], LineParts[1]);
	end;

	FileContents.Free;
end;

procedure TMainForm.SaveToFile;
var
	FileContents: TStringList;
	CalcHandler: TCalcHandler;
begin
	FileContents := TStringList.Create;
	for CalcHandler in GlobalCalcState.AllCalculators do begin
		FileContents.Append(
			CalcHandler.Name
			+ ':'
			+ TCalcView(CalcHandler.Frame).Content
		);
	end;

	FileContents.SaveToFile(GlobalCalcState.SavedAs);
	FileContents.Free;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
	SaveDialog.InitialDir := GetUserDir;
	OpenDialog.InitialDir := GetUserDir;
	self.AddCalculator();
end;

procedure TMainForm.ActionNewCalculatorExecute(Sender: TObject);
begin
	self.UpdatePosition;
	self.AddCalculator();
	GlobalCalcState.Dirty := True;
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

procedure TMainForm.ActionNewExecute(Sender: TObject);
begin
	if not self.CheckDirty() then exit;

	self.UpdatePosition();
	self.ClearCalculators();
	self.AddCalculator();
	GlobalCalcState.SavedAs := '';
end;

procedure TMainForm.ActionOpenExecute(Sender: TObject);
begin
	if not self.CheckDirty() then exit;
	if OpenDialog.Execute then begin
		GlobalCalcState.SavedAs := OpenDialog.Filename;
		self.UpdatePosition();
		self.ClearCalculators();
		self.LoadFromFile();
	end;
end;

procedure TMainForm.ActionSaveAsExecute(Sender: TObject);
begin
	if SaveDialog.Execute then
		GlobalCalcState.SavedAs := SaveDialog.Filename
	else
		exit;

	self.SaveToFile;
	GlobalCalcState.Dirty := False;
end;

procedure TMainForm.ActionSaveExecute(Sender: TObject);
begin
	if GlobalCalcState.SavedAs = '' then begin
		if SaveDialog.Execute then
			GlobalCalcState.SavedAs := SaveDialog.Filename
		else
			exit;
	end;

	self.SaveToFile;
	GlobalCalcState.Dirty := False;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
	if not self.CheckDirty() then
		CloseAction := caNone;
end;

end.

