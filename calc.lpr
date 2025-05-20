program calc;

{$mode objfpc}{$H+}

uses
	{$IFDEF UNIX}
	cthreads,
	{$ENDIF}
	{$IFDEF HASAMIGA}
	athreads,
	{$ENDIF}
	Interfaces, // this includes the LCL widgetset
	Forms, lazcontrols, MainWindow, CalcFormatters,
	{ you can add units after this }
	SysUtils, SyntaxHelp;

{$R *.res}

begin
	RequireDerivedFormResource:=True;
	Application.Scaled := True;
	Application.Initialize;
	Application.CreateForm(TMainForm, MainForm);
	if ParamCount > 0 then
		MainForm.OpenFile(ParamStr(1));
	Application.CreateForm(TSyntaxHelpForm, SyntaxHelpForm);
	Application.Run;
end.

