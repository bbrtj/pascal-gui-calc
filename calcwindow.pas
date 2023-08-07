unit CalcWindow;

{$mode ObjFPC}{$H+}

interface

uses
	Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

	{ TCalcForm }

 TCalcForm = class(TForm)
		CalcButton: TButton;
		CalcEdit: TEdit;
		CalcResultEdit: TEdit;
		PasteButton: TButton;
		CopyButton: TButton;
		Expression: TGroupBox;
	private

	public

	end;

var
	CalcForm: TCalcForm;

implementation

{$R *.lfm}

end.

