unit CalcFormatters;

{$mode objfpc}{$H+}{$J-}

interface

uses SysUtils, Math, PNBase;

function FloatToHex(Value: Extended; const Settings: TFormatSettings): String;

implementation

{ helper }
function IntToHex(Value: Int64): String;
const
	HexValues: Array[0 .. 15] of Char = '0123456789ABCDEF';
begin
	result := '';
	repeat
		result := HexValues[Value mod 16] + result;
		Value := Value div 16;
	until Value = 0;
end;

function FloatToHex(Value: Extended; const Settings: TFormatSettings): String;
const
	cLongestFraction = 10;
var
	LFraction: Extended;
	I: Int32;
begin
	result := '';
	if Value < 0 then begin
		result += '-';
		Value := -1 * Value;
	end;

	result += '0x';
	result += IntToHex(Floor(Value));

	LFraction := Value - Floor(Value);
	if LFraction < 1 / Power(16, cLongestFraction) then exit;

	result += Settings.DecimalSeparator;
	for I := 1 to cLongestFraction do begin
		LFraction *= 16;
		result += IntToHex(Floor(LFraction));
		LFraction -= Floor(LFraction);
	end;
end;

end.

