unit CalcTypes;

{$mode ObjFPC}{$H+}

interface

uses
	Classes, SysUtils;

type
	IFormWithCalculator = interface
	['{7a8a8caf-387f-11ee-b14a-002b67685373}']
		procedure AddCalculator(const CustomName: String = ''; const Content: String = '');
		procedure RemoveCalculator(CalcHandler: TObject);
	end;

implementation

end.

