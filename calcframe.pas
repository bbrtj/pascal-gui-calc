unit CalcFrame;

{$mode ObjFPC}{$H+}

interface

uses
	SysUtils, Classes, Math, Forms, Controls, StdCtrls,
	Dialogs, Menus, Buttons, ActnList, Clipbrd,
	CalcState, CalcTypes, PNBase;

type

	{ TCalcView }

 	TCalcView = class(TFrame)
		ActionFormatDecimal: TAction;
		ActionFormatHexadecimal: TAction;
		ActionFormatScientific: TAction;
		ActionCopyText: TAction;
		ActionCalculate: TAction;
		ActionRename: TAction;
		ActionRemove: TAction;
		MenuButton: TButton;
		CalculatorActions: TActionList;
		CalcButton: TButton;
		CalcEdit: TEdit;
		CalcResultEdit: TEdit;
		Expression: TGroupBox;
		LabelEquals: TLabel;
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
		procedure ActionCopyTextExecute(Sender: TObject);
		procedure ActionCalculateExecute(Sender: TObject);
		procedure ActionFormatDecimalExecute(Sender: TObject);
		procedure ActionFormatHexadecimalExecute(Sender: TObject);
		procedure ActionFormatScientificExecute(Sender: TObject);
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

procedure TCalcView.ActionFormatDecimalExecute(Sender: TObject);
begin
	FHandler.ResultFormat := rfDecimal;
	self.WriteResult();
end;

procedure TCalcView.ActionFormatHexadecimalExecute(Sender: TObject);
begin
	FHandler.ResultFormat := rfHexadecimal;
	self.WriteResult();
end;

procedure TCalcView.ActionFormatScientificExecute(Sender: TObject);
begin
	FHandler.ResultFormat := rfScientific;
	self.WriteResult();
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

	self.Expression.Caption := NewName;
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
		customName := self.Expression.Caption + IntToStr(LastView);

	self.Name := self.Name + IntToStr(LastView);
	self.Expression.Caption := customName;

	FHandler := TCalcHandler.Create(customName, self);
end;

function TCalcView.Calculate(): Boolean;

	procedure ShowCalcError(const ErrType: String; const ErrMessage: String);
	begin
		MessageDlg(
			'Calculation error',
			ErrType + ' error occured while executing ' + FHandler.Name + ': ' + sLineBreak + ErrMessage,
			mtError,
			[mbOk],
			0
		);
	end;

begin
	result := True;
	if CalcEdit.Text <> '' then begin
		result := False;
		try
			FHandler.Calculate(CalcEdit.Text);
			self.WriteResult();
			result := True;
		except
			on E: EParsingFailed do ShowCalcError('Parsing', E.Message);
			on E: ECalculationFailed do ShowCalcError('Evaluation', E.Message);
			on E: Exception do ShowCalcError('Other', E.Message);
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
begin
	CalcResultEdit.Text := FHandler.GetFormatted();
end;

initialization
	ResetNumbers;
end.

