unit MainWindow;

{$mode ObjFPC}{$H+}

interface

uses
	Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ActnList,
	ExtCtrls, Menus, StrUtils, Math, Types,
	CalcFrame, SyntaxHelp, CalcState, CalcTypes, PNBase;

type

	{ TMainForm }

 TMainForm = class(TForm, IFormWithCalculator)
		ActionNextFormat: TAction;
		ActionCopyText: TAction;
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
		procedure ActionCopyTextExecute(Sender: TObject);
		procedure ActionNewCalculatorExecute(Sender: TObject);
		procedure ActionCalculateAllExecute(Sender: TObject);
		procedure ActionExitProgramExecute(Sender: TObject);
		procedure ActionNewExecute(Sender: TObject);
		procedure ActionNextFormatExecute(Sender: TObject);
		procedure ActionOpenExecute(Sender: TObject);
		procedure ActionRenameExecute(Sender: TObject);
		procedure ActionSaveAsExecute(Sender: TObject);
		procedure ActionSaveExecute(Sender: TObject);
		procedure ActionSyntaxExecute(Sender: TObject);
		procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
		procedure FormCreate(Sender: TObject);
		procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
		procedure FormResize(Sender: TObject);
	private
		FToRemove: TCalcHandler;
		FOriginalTitle: String;

		procedure AddCalculator(const CustomName: String = ''; const Content: String = ''; ResultFormat: TResultFormat = rfDecimal);
		procedure DoRemoveCalculator(Arg: Int64);
		procedure RemoveCalculator(CalcHandler: TObject);
		procedure RenameCalculator(const OldName, NewName: String);
		procedure AdjustPosition;
		function CheckDirty: Boolean;

		procedure ClearCalculators();

		procedure LoadFromFile;
		procedure SaveToFile;

		procedure SetDirty(Value: Boolean);
		procedure SetSavedAs(const Value: String);
		procedure UpdateTitle();
	public
		procedure OpenFile(const Filename: String);

	end;

var
	MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.AddCalculator(const CustomName: String; const Content: String; ResultFormat: TResultFormat);
var
	CalcView: TCalcView;
begin
	CalcView := TCalcView.Create(self, CustomName);
	self.InsertControl(CalcView);
	CalcView.Content := Content;
	CalcView.Handler.ResultFormat := ResultFormat;
	CalcView.FixFormatSelection;

	GlobalCalcState.AddCalculator(CalcView.Handler);
	self.DoOnResize;
end;

procedure TMainForm.DoRemoveCalculator(Arg: Int64);
begin
	if FToRemove = nil then exit;

	self.RemoveControl(FToRemove.Frame);
	self.RemoveComponent(FToRemove.Frame);
	GlobalCalcState.RemoveCalculator(FToRemove).Free;
	self.SetDirty(True);

	self.DoOnResize;
end;

procedure TMainForm.RemoveCalculator(CalcHandler: TObject);
begin
	FToRemove := CalcHandler as TCalcHandler;
	Application.QueueAsyncCall(@self.DoRemoveCalculator, 0);
end;

procedure TMainForm.RenameCalculator(const OldName, NewName: String);
var
	CalcHandler: TCalcHandler;
	Calculator: TCalcView;
	NewContent: String;
begin
	for CalcHandler in GlobalCalcState.AllCalculators do begin
		try
				Calculator := CalcHandler.Frame as TCalcView;
				NewContent := CalcHandler.RenameVariable(Calculator.GetContent(), OldName, NewName);
				Calculator.SetContent(NewContent);
		except
			continue;
		end;
	end;
end;

procedure TMainForm.AdjustPosition;
const
	cVisibleCalculators = 7;
var
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

	self.Width := TotalWidth;
	self.Height := VisibleHeight;
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
			mrNo: self.SetDirty(False);
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
		if ExtractFileExt(Filename) <> '.calc' then
			raise Exception.Create('invalid extension');

		self.SetSavedAs(Filename);
		self.ClearCalculators();
		self.LoadFromFile();
		self.SetDirty(False);
	except
		on E: Exception do begin
			MessageDlg(
				'File open error',
				'Couldn''t open file ' + Filename + ': ' + E.Message,
				mtError,
				[mbOk],
				0
			);

			self.SetSavedAs('');
			self.ClearCalculators();
			self.AddCalculator();
		end;
	end;

end;

procedure TMainForm.LoadFromFile;
var
	FileContents: TStringList;
	Line: String;
	LineParts: TStringArray;
	ResultFormatInt: Int32;
	ResultFormat: TResultFormat;
begin
	try
		FileContents := TStringList.Create;
		FileContents.LoadFromFile(GlobalCalcState.SavedAs);

		// TODO: save and load presentation format
		for Line in FileContents do begin
			LineParts := SplitString(Line, ':');
			if (length(LineParts) < 2) or (length(LineParts) > 3) then
				continue;
			if length(LineParts[0]) = 0 then
				continue;

			if (length(LineParts) < 3) or not TryStrToInt(LineParts[2], ResultFormatInt) then
				ResultFormatInt := Ord(Low(TResultFormat));

			if (ResultFormatInt >= Ord(Low(TResultFormat))) and (ResultFormatInt <= Ord(High(TResultFormat))) then
				ResultFormat := TResultFormat(ResultFormatInt);

			self.AddCalculator(LineParts[0], LineParts[1], ResultFormat);
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
			+ ':'
			+ IntToStr(Ord(CalcHandler.ResultFormat))
		);
	end;

	FileContents.SaveToFile(GlobalCalcState.SavedAs);
	FileContents.Free;
end;

procedure TMainForm.SetDirty(Value: Boolean);
begin
	GlobalCalcState.Dirty := Value;
	self.UpdateTitle;
end;

procedure TMainForm.SetSavedAs(const Value: String);
begin
	GlobalCalcState.SavedAs := Value;
	self.UpdateTitle;
end;

procedure TMainForm.UpdateTitle();
var
	TitleText: String;
begin
	TitleText := '';


	TitleText += FOriginalTitle + ' - ';

	if GlobalCalcState.SavedAs <> '' then
		TitleText += GlobalCalcState.SavedAs
	else
		TitleText += 'Unnamed';

	if GlobalCalcState.Dirty then
		TitleText += ' ***';

	self.Caption := TitleText;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
	SaveDialog.InitialDir := GetUserDir;
	OpenDialog.InitialDir := GetUserDir;
	FToRemove := nil;
	FOriginalTitle := self.Caption;
	self.AddCalculator();
end;

procedure TMainForm.FormDropFiles(Sender: TObject; const FileNames: array of string);
begin
	if not self.CheckDirty() then exit;

	if length(FileNames) > 0 then
		self.OpenFile(FileNames[0]);
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
	self.AdjustPosition;
end;

procedure TMainForm.ActionNewCalculatorExecute(Sender: TObject);
begin
	self.AddCalculator();
	self.SetDirty(True);
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

procedure TMainForm.ActionCopyTextExecute(Sender: TObject);
var
	CalcHandler: TCalcHandler;
begin
	for CalcHandler in GlobalCalcState.AllCalculators do begin
		if TCalcView(CalcHandler.Frame).IsSelected then begin
			TCalcView(CalcHandler.Frame).ActionCopyTextExecute(Sender);
			break;
		end;
	end;
end;

procedure TMainForm.ActionNextFormatExecute(Sender: TObject);
var
	CalcHandler: TCalcHandler;
begin
	for CalcHandler in GlobalCalcState.AllCalculators do begin
		if TCalcView(CalcHandler.Frame).IsSelected then begin
			TCalcView(CalcHandler.Frame).ActionNextFormatExecute(Sender);
			break;
		end;
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

	self.ClearCalculators();
	self.AddCalculator();
	self.SetSavedAs('');
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
		self.SetDirty(WasDirty);
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
		self.SetSavedAs(SaveDialog.Filename)
	else
		exit;

	self.SaveToFile;
	self.SetDirty(False);
end;

procedure TMainForm.ActionSaveExecute(Sender: TObject);
begin
	if GlobalCalcState.SavedAs = '' then begin
		if SaveDialog.Execute then
			self.SetSavedAs(SaveDialog.Filename)
		else
			exit;
	end;

	self.SaveToFile;
	self.SetDirty(False);
end;

procedure TMainForm.ActionSyntaxExecute(Sender: TObject);
begin
	SyntaxHelpForm.Show;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
	if not self.CheckDirty() then
		CloseAction := caNone;
end;

end.

