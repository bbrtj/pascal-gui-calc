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
	private
		FHandler: TCalcHandler;

	public
		constructor Create(TheOwner: TComponent); override;

		property Handler: TCalcHandler read FHandler write FHandler;
	end;

implementation

{$R *.lfm}

{ TCalcView }

var
   LastView: Cardinal;

procedure TCalcView.CalcButtonClick(Sender: TObject);
begin
	CalcResultEdit.Text := FHandler.Calculate(CalcEdit.Text);
end;

constructor TCalcView.Create(TheOwner: TComponent);
begin
	inherited Create(TheOwner);

	inc(LastView);
	self.Name := self.Name + IntToStr(LastView);
	self.Expression.Caption := self.Expression.Caption + IntToStr(LastView);
end;

initialization
	LastView := 0;
end.

