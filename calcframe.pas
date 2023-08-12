unit CalcFrame;

{$mode ObjFPC}{$H+}

interface

uses
	Classes, SysUtils, Forms, Controls, StdCtrls,
	Dialogs, Menus, Buttons, ActnList, Math, Types,
	CalcState, CalcTypes, PNBase;

type

	{ TCalcView }

 TCalcView = class(TFrame)
		ActionCalculate: TAction;
		ActionRename: TAction;
		ActionRemove: TAction;
		ActionMemoryStore: TAction;
		ActionMemoryRead: TAction;
		CalculatorActions: TActionList;
		CalcButton: TButton;
		CalcEdit: TEdit;
		CalcResultEdit: TEdit;
		CopyButton: TButton;
		Expression: TGroupBox;
		LabelEquals: TLabel;
		MenuItemCalculate: TMenuItem;
		MenuItemRemove: TMenuItem;
		MenuItemRename: TMenuItem;
		PasteButton: TButton;
		CalcMenu: TPopupMenu;
		Separator1: TMenuItem;
		procedure ActionCalculateExecute(Sender: TObject);
		procedure ActionMemoryReadExecute(Sender: TObject);
		procedure ActionMemoryStoreExecute(Sender: TObject);
		procedure ActionRemoveExecute(Sender: TObject);
		procedure ActionRenameExecute(Sender: TObject);
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

procedure TCalcView.ActionMemoryReadExecute(Sender: TObject);
begin
	CalcEdit.Text := GlobalCalcState.Memory;
end;

procedure TCalcView.ActionMemoryStoreExecute(Sender: TObject);
begin
	GlobalCalcState.Memory := CalcEdit.Text;
end;

procedure TCalcView.ActionRemoveExecute(Sender: TObject);
begin
	(self.Owner as IFormWithCalculator).RemoveCalculator(FHandler);
end;

procedure TCalcView.ActionRenameExecute(Sender: TObject);
var
	NewName: String;
begin
	NewName := InputBox(
		'Rename calculator',
		'Enter new name for the calculator',
		self.Handler.Name
	);

	self.Expression.Caption := NewName;
	self.Handler.Name := NewName;
	GlobalCalcState.Dirty := True;
end;

procedure TCalcView.CalcEditChange(Sender: TObject);
begin
	GlobalCalcState.Dirty := True;
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
			CalcResultEdit.Text := FHandler.Calculate(CalcEdit.Text);
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

initialization
	ResetNumbers;
end.

