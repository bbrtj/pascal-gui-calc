unit CalcFrame;

{$mode ObjFPC}{$H+}

interface

uses
	Classes, SysUtils, Forms, Controls, StdCtrls,
	CalcState;

type

	{ TCalcView }

 TCalcView = class(TFrame)
		CalcButton: TButton;
		CalcEdit: TEdit;
		CalcResultEdit: TEdit;
		CopyButton: TButton;
		Expression: TGroupBox;
		PasteButton: TButton;
		procedure CalcButtonClick(Sender: TObject);
		procedure CopyButtonClick(Sender: TObject);
		procedure PasteButtonClick(Sender: TObject);
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

procedure TCalcView.CalcButtonClick(Sender: TObject);
begin
	Calculate;
end;

procedure TCalcView.CopyButtonClick(Sender: TObject);
begin
	GlobalCalcState.Memory := CalcEdit.Text;
end;

procedure TCalcView.PasteButtonClick(Sender: TObject);
begin
	CalcEdit.Text := GlobalCalcState.Memory;
end;

constructor TCalcView.Create(TheOwner: TComponent);
begin
	inherited Create(TheOwner);

	inc(LastView);
	self.Name := self.Name + IntToStr(LastView);
	self.Expression.Caption := self.Expression.Caption + IntToStr(LastView);
end;

procedure TCalcView.Calculate;
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

