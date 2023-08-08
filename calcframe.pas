unit CalcFrame;

{$mode ObjFPC}{$H+}

interface

uses
	Classes, SysUtils, Forms, Controls, StdCtrls,
	Dialogs, Menus, Buttons, ActnList,
	CalcState;

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
	private
		FHandler: TCalcHandler;

	public
		constructor Create(TheOwner: TComponent); override;

		procedure Calculate();
		function IsSelected(): Boolean;

		property Handler: TCalcHandler read FHandler write FHandler;
	end;

implementation

{$R *.lfm}

{ TCalcView }

var
   LastView: Cardinal;

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
	GlobalCalcState.RemoveCalculator(self.Handler);
	TForm(Owner).RemoveControl(self);
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

	// TODO: validate NewName

	self.Expression.Caption := NewName;
	self.Handler.Name := NewName;
end;

constructor TCalcView.Create(TheOwner: TComponent);
begin
	inherited Create(TheOwner);

	inc(LastView);
	self.Name := self.Name + IntToStr(LastView);
	self.Expression.Caption := self.Expression.Caption + IntToStr(LastView);
end;

procedure TCalcView.Calculate();
begin
	CalcResultEdit.Text := FHandler.Calculate(CalcEdit.Text);
end;

function TCalcView.IsSelected: Boolean;
begin
	result := CalcEdit.Focused or CalcResultEdit.Focused;
end;

initialization
	LastView := 0;
end.

