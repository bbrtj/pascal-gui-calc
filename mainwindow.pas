unit MainWindow;

{$mode ObjFPC}{$H+}

interface

uses
	Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ActnList,
	ExtCtrls, Menus, LCLIntf, StrUtils, Math, Types,
	CalcFrame, CalcState, CalcTypes, PNBase;

type

	{ TMainForm }

 TMainForm = class(TForm, IFormWithCalculator)
		ActionRename: TAction;
		ActionSyntax: TAction;
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
		procedure ActionRenameExecute(Sender: TObject);
		procedure ActionSaveAsExecute(Sender: TObject);
		procedure ActionSaveExecute(Sender: TObject);
		procedure ActionSyntaxExecute(Sender: TObject);
		procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
		procedure FormCreate(Sender: TObject);
        procedure FormResize(Sender: TObject);
	private
		procedure AddCalculator(const CustomName: String = ''; const Content: String = '');
		procedure RemoveCalculator(CalcHandler: TObject);
        procedure AdjustPosition;
		function CheckDirty: Boolean;

		procedure ClearCalculators();

		procedure LoadFromFile;
		procedure SaveToFile;
	public
		procedure OpenFile(const Filename: String);

	end;

var
	MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.AddCalculator(const CustomName: String; const Content: String);
var
	CalcView: TCalcView;
begin
	CalcView := TCalcView.Create(self, CustomName);
	self.InsertControl(CalcView);
	CalcView.Content := Content;

	GlobalCalcState.AddCalculator(CalcView.Handler);
	self.DoOnResize;
end;

procedure TMainForm.RemoveCalculator(CalcHandler: TObject);
var
	CalcHandlerObj: TCalcHandler;
begin
	CalcHandlerObj := CalcHandler as TCalcHandler;
	self.RemoveControl(CalcHandlerObj.Frame);
	GlobalCalcState.RemoveCalculator(CalcHandlerObj);
	GlobalCalcState.Dirty := True;

	CalcHandlerObj.Frame.Free;
	self.DoOnResize;
end;

procedure TMainForm.AdjustPosition;
const
	cVisibleCalculators = 7;
var
	rect: TRect;
	VisibleN, TotalN: Integer;
	VisibleHeight, TotalHeight: Integer;
	TotalWidth: Integer;
    Ind: Integer;
begin
	TotalN := self.ControlCount - 1;
	if TotalN >= 0 then begin
		VisibleN := Min(cVisibleCalculators - 1, TotalN);
		VisibleHeight := self.ChildSizing.TopBottomSpacing * 2;
		TotalHeight := VisibleHeight + self.ChildSizing.VerticalSpacing * TotalN;
		VisibleHeight := VisibleHeight + self.ChildSizing.VerticalSpacing * VisibleN;
		TotalWidth := 0;

		for Ind := 0 to TotalN do begin
			if Ind <= VisibleN then
				VisibleHeight += self.Controls[Ind].Height;
			TotalHeight += self.Controls[Ind].Height;
			TotalWidth := Max(TotalWidth, self.Controls[Ind].Width);
		end;

		TotalWidth += self.ChildSizing.LeftRightSpacing * 2;
    end
	else begin
		// some defaults
		TotalWidth := 500;
		TotalHeight := 80;
		VisibleHeight := 80;
	end;

	GetWindowRect(self.Handle, rect);
	self.SetBounds(rect.Left, rect.Top, TotalWidth, VisibleHeight);
	self.VertScrollBar.Page := VisibleHeight;
	self.VertScrollBar.Range := TotalHeight;
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
	self.DoOnResize;
end;

procedure TMainForm.OpenFile(const Filename: String);
begin
	try
		GlobalCalcState.SavedAs := Filename;
		self.ClearCalculators();
		self.LoadFromFile();
		GlobalCalcState.Dirty := False;
	except
		on E: Exception do begin
			MessageDlg(
				'File open error',
				'Couldn''t open file ' + Filename + ': ' + E.Message,
				mtError,
				[mbOk],
				0
			);

			GlobalCalcState.SavedAs := '';
			self.AddCalculator();
		end;
	end;

end;

procedure TMainForm.LoadFromFile;
var
	FileContents: TStringList;
	Line: String;
	LineParts: TStringArray;
begin
	try
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

	finally
		FileContents.Free;
	end;
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

procedure TMainForm.FormResize(Sender: TObject);
begin
	self.AdjustPosition;
end;

procedure TMainForm.ActionNewCalculatorExecute(Sender: TObject);
begin
	self.AddCalculator();
	GlobalCalcState.Dirty := True;
end;

procedure TMainForm.ActionCalculateExecute(Sender: TObject);
var
	CalcHandler: TCalcHandler;
begin
	for CalcHandler in GlobalCalcState.AllCalculators do begin
		if TCalcView(CalcHandler.Frame).IsSelected then begin
			TCalcView(CalcHandler.Frame).Calculate;
			break;
		end;
	end;
end;

procedure TMainForm.ActionCalculateAllExecute(Sender: TObject);
var
	CalcHandler: TCalcHandler;
begin
	for CalcHandler in GlobalCalcState.AllCalculators do begin
		if not TCalcView(CalcHandler.Frame).Calculate then
			break;
	end;
end;

procedure TMainForm.ActionExitProgramExecute(Sender: TObject);
begin
	Close;
end;

procedure TMainForm.ActionNewExecute(Sender: TObject);
begin
	if not self.CheckDirty() then exit;

	self.ClearCalculators();
	self.AddCalculator();
	GlobalCalcState.SavedAs := '';
end;

procedure TMainForm.ActionOpenExecute(Sender: TObject);
var
	WasDirty: Boolean;
begin
	WasDirty := GlobalCalcState.Dirty;
	if not self.CheckDirty() then exit;
	if OpenDialog.Execute then begin
		OpenFile(OpenDialog.Filename);
	end
	else
		GlobalCalcState.Dirty := WasDirty;
end;

procedure TMainForm.ActionRenameExecute(Sender: TObject);
var
	CalcHandler: TCalcHandler;
begin
	for CalcHandler in GlobalCalcState.AllCalculators do begin
		if TCalcView(CalcHandler.Frame).IsSelected then begin
			TCalcView(CalcHandler.Frame).ActionRenameExecute(Sender);
			break;
		end;
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

procedure TMainForm.ActionSyntaxExecute(Sender: TObject);
begin
	MessageDlg(
		'Syntax reference',
		'Available operations:' + sLineBreak + TOperationInfo.Help(False)
		+ sLineBreak + 'Available constants:' + sLineBreak
		+ 'PI: the pi constant' + sLineBreak
		+ 'PHI: the golden ratio' + sLineBreak,
		mtInformation,
		[mbOk],
		0
	);
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
	if not self.CheckDirty() then
		CloseAction := caNone;
end;

end.

