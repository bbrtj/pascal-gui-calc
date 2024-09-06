unit CalcFrame;

{$mode ObjFPC}{$H+}

interface

uses
	SysUtils, Classes, Math, Forms, Controls, StdCtrls,
	Dialogs, Menus, Buttons, ActnList, Clipbrd, ExtCtrls,
	CalcState, CalcTypes, PNBase;

type

	{ TCalcView }

 	TCalcView = class(TFrame)
		ActionNextFormat: TAction;
		ActionFormatBinary: TAction;
		ActionFormatOctal: TAction;
		ActionFormatDecimal: TAction;
		ActionFormatHexadecimal: TAction;
		ActionFormatScientific: TAction;
		ActionCopyText: TAction;
		ActionCalculate: TAction;
		ActionRename: TAction;
		ActionRemove: TAction;
		CalcEdit: TEdit;
		CalcResultEdit: TEdit;
		ErrorLabel: TLabel;
		Expression: TPanel;
		ExpressionName: TLabel;
		LabelEquals: TLabel;
		MenuButton: TButton;
		WarningLabel: TLabel;
		CalculatorActions: TActionList;
		CalcButton: TButton;
		MenuItemNextResultFormat: TMenuItem;
		MenuItemResultFormatBinary: TMenuItem;
		MenuItemResultFormatOctal: TMenuItem;
		MenuItemResultFormat: TMenuItem;
		MenuItemResultFormatDecimal: TMenuItem;
		MenuItemResultFormatHex: TMenuItem;
		MenuItemResultFormatScientific: TMenuItem;
		MenuItemCopyText: TMenuItem;
		MenuItemCalculate: TMenuItem;
		MenuItemRemove: TMenuItem;
		MenuItemRename: TMenuItem;
		CalcMenu: TPopupMenu;
		Separator1: TMenuItem;
		Separator2: TMenuItem;
		procedure ActionCopyTextExecute(Sender: TObject);
		procedure ActionCalculateExecute(Sender: TObject);
		procedure ActionFormatBinaryExecute(Sender: TObject);
		procedure ActionFormatDecimalExecute(Sender: TObject);
		procedure ActionFormatHexadecimalExecute(Sender: TObject);
		procedure ActionFormatOctalExecute(Sender: TObject);
		procedure ActionFormatScientificExecute(Sender: TObject);
		procedure ActionNextFormatExecute(Sender: TObject);
		procedure ActionRemoveExecute(Sender: TObject);
		procedure ActionRenameExecute(Sender: TObject);
		procedure MenuButtonClick(Sender: TObject);
		procedure CalcEditChange(Sender: TObject);
		procedure FrameMouseWheel(Sender: TObject; Shift: TShiftState;
			WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
	private
		FHandler: TCalcHandler;

	public
		constructor Create(TheOwner: TComponent; customName: String = '');

		function Calculate: Boolean;
		function IsSelected(): Boolean;
		procedure SetContent(const Content: String);
		function GetContent(): String;
		procedure WriteResult();
		procedure SetError(const Error: String);
		procedure SetWarning(const Warning: String);
		procedure FixFormatSelection();

		property Content: String read GetContent write SetContent;
		property Handler: TCalcHandler read FHandler write FHandler;
	end;

procedure ResetNumbers;

implementation

{$R *.lfm}

{ TCalcView }

var
	LastView: Cardinal;

procedure ResetNumbers;
begin
	LastView := 0;
end;

procedure TCalcView.ActionCalculateExecute(Sender: TObject);
begin
	Calculate;
end;

procedure TCalcView.ActionFormatBinaryExecute(Sender: TObject);
begin
	FHandler.ResultFormat := rfBinary;
	self.WriteResult();
	(self.Owner as IFormWithCalculator).SetDirty(True);
end;

procedure TCalcView.ActionFormatDecimalExecute(Sender: TObject);
begin
	FHandler.ResultFormat := rfDecimal;
	self.WriteResult();
	(self.Owner as IFormWithCalculator).SetDirty(True);
end;

procedure TCalcView.ActionFormatHexadecimalExecute(Sender: TObject);
begin
	FHandler.ResultFormat := rfHexadecimal;
	self.WriteResult();
	(self.Owner as IFormWithCalculator).SetDirty(True);
end;

procedure TCalcView.ActionFormatOctalExecute(Sender: TObject);
begin
	FHandler.ResultFormat := rfOctal;
	self.WriteResult();
	(self.Owner as IFormWithCalculator).SetDirty(True);
end;

procedure TCalcView.ActionFormatScientificExecute(Sender: TObject);
begin
	FHandler.ResultFormat := rfScientific;
	self.WriteResult();
	(self.Owner as IFormWithCalculator).SetDirty(True);
end;

procedure TCalcView.ActionNextFormatExecute(Sender: TObject);
var
	NextFormatTag: Integer;
begin
	NextFormatTag := (Ord(FHandler.ResultFormat) + 1) mod (Ord(High(TResultFormat)) + 1);
	FHandler.ResultFormat := TResultFormat(NextFormatTag);
	self.FixFormatSelection;
	self.WriteResult();
	(self.Owner as IFormWithCalculator).SetDirty(True);
end;

procedure TCalcView.ActionCopyTextExecute(Sender: TObject);
var
	CalcLeft, CalcRight: String;
begin
	CalcLeft := Trim(CalcEdit.Text);
	CalcRight := Trim(CalcResultEdit.Text);

	Clipboard.AsText := '';
	if not self.Handler.DefaultName then begin
		if CalcLeft = CalcRight then
			Clipboard.AsText :=	self.Handler.Name + ' = ' + CalcLeft
		else
			Clipboard.AsText :=	self.Handler.Name + ': ' + CalcLeft + ' = ' + CalcRight;
	end
	else
		Clipboard.AsText :=	CalcLeft + ' = ' + CalcRight;
end;

procedure TCalcView.ActionRemoveExecute(Sender: TObject);
begin
	(self.Owner as IFormWithCalculator).RemoveCalculator(FHandler);
end;

procedure TCalcView.ActionRenameExecute(Sender: TObject);
var
	OldName: String;
	NewName: String;
begin
	OldName := self.Handler.Name;
	NewName := InputBox(
		'Rename calculator',
		'Enter new name for the calculator',
		OldName
	);

	self.ExpressionName.Caption := NewName;
	self.Handler.Name := NewName;
	(self.Owner as IFormWithCalculator).RenameCalculator(OldName, NewName);
	(self.Owner as IFormWithCalculator).SetDirty(True);
end;

procedure TCalcView.MenuButtonClick(Sender: TObject);
begin
	CalcMenu.PopUp();
end;

procedure TCalcView.CalcEditChange(Sender: TObject);
begin
	(self.Owner as IFormWithCalculator).SetDirty(True);
end;

procedure TCalcView.FrameMouseWheel(Sender: TObject; Shift: TShiftState;
	WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
	ParentScroll: TControlScrollBar;
begin
	ParentScroll := (self.Parent as TScrollingWinControl).VertScrollBar;
	ParentScroll.Position := ParentScroll.Position - Sign(WheelDelta) * ParentScroll.Increment;
	Handled := True;
end;

constructor TCalcView.Create(TheOwner: TComponent; customName: String = '');
begin
	inherited Create(TheOwner);

	inc(LastView);
	if customName = '' then
		customName := self.ExpressionName.Caption + IntToStr(LastView);

	self.Name := self.Name + IntToStr(LastView);
	self.ExpressionName.Caption := customName;

	FHandler := TCalcHandler.Create(customName, self);
	SetError('');
end;

function TCalcView.Calculate(): Boolean;
begin
	result := True;
	if CalcEdit.Text <> '' then begin
		result := False;
		try
			FHandler.Calculate(CalcEdit.Text);
			self.WriteResult();
			result := True;
		except
			on E: EParsingFailed do SetError('Parsing error: ' + E.Message);
			on E: ECalculationFailed do SetError('Evaluation error: ' + E.Message);
			on E: Exception do SetError('Error: ' + E.Message);
		end;
	end;
end;

function TCalcView.IsSelected: Boolean;
begin
	result := CalcEdit.Focused or CalcResultEdit.Focused;
end;

procedure TCalcView.SetContent(const Content: String);
begin
	CalcEdit.Text := Content;
end;

function TCalcView.GetContent: String;
begin
	result := CalcEdit.Text;
end;

procedure TCalcView.WriteResult();
var
	LOverflow: Boolean;
begin
	CalcResultEdit.Text := FHandler.GetFormatted(LOverflow);
	if LOverflow then
		SetWarning('Long number, result may not be accurate')
	else
		SetError('');
end;

procedure TCalcView.SetError(const Error: String);
begin
	ErrorLabel.Visible := True;
	ErrorLabel.Caption := Error;
	WarningLabel.Visible := False;
end;

procedure TCalcView.SetWarning(const Warning: String);
begin
	WarningLabel.Visible := True;
	WarningLabel.Caption := Warning;
	ErrorLabel.Visible := False;
end;

procedure TCalcView.FixFormatSelection();
var
	I: Integer;
	MenuItems: Array[0 .. 4] of TMenuItem;
begin
	MenuItems := [
		MenuItemResultFormatDecimal,
		MenuItemResultFormatBinary,
		MenuItemResultFormatOctal,
		MenuItemResultFormatHex,
		MenuItemResultFormatScientific
	];

	// Tag is used for marking MenuItem
	for I := Low(MenuItems) to High(MenuItems) do begin
		if MenuItems[I].Tag = Ord(FHandler.ResultFormat) then
			MenuItems[I].Checked := True;
	end;
end;

initialization
	ResetNumbers;
end.

