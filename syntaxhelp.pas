unit SyntaxHelp;

{$mode ObjFPC}{$H+}

interface

uses
	Forms, StdCtrls, ListFilterEdit, Classes, PNBase;

type

	{ TSyntaxHelpForm }

	TSyntaxHelpForm = class(TForm)
		HelpList: TListBox;
		HelpListFilter: TListFilterEdit;
		procedure FormCreate(Sender: TObject);
	private

	public

	end;

var
	SyntaxHelpForm: TSyntaxHelpForm;

implementation

{$R *.lfm}

{ TSyntaxHelpForm }

procedure TSyntaxHelpForm.FormCreate(Sender: TObject);
begin
	HelpList.Items.Text := TOperationInfo.FullHelp(False)
		+ 'PI: the pi constant' + sLineBreak
		+ 'PHI: the golden ratio';
	HelpListFilter.Items.Text := HelpList.Items.Text;
end;

end.

