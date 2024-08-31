unit CalcFormatters;

{$mode objfpc}{$H+}{$J-}

interface

uses SysUtils, Math, PNBase;

function FloatToBase(Value: Extended; const Settings: TFormatSettings): String;

implementation

{ helper }
function IntToDigitBase(Value: Int64; Base: UInt8 = 10): String;
const
	HexValues: Array[0 .. 15] of Char = '0123456789ABCDEF';
begin
	if (Base > 16) or (Base < 2) then
		raise Exception.Create('Invalid base');

	result := '';
	repeat
		result := HexValues[Value mod Base] + result;
		Value := Value div Base;
	until Value = 0;
end;

{ small hack: uses currency fields from TFormatSettings to specify base }
function FloatToBase(Value: Extended; const Settings: TFormatSettings): String;
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

	result += Settings.CurrencyString;
	result += IntToDigitBase(Floor(Value), Settings.CurrencyFormat);

	LFraction := Value - Floor(Value);
	if LFraction < 1 / Power(Settings.CurrencyFormat, cLongestFraction) then exit;

	result += Settings.DecimalSeparator;
	for I := 1 to cLongestFraction do begin
		LFraction *= Settings.CurrencyFormat;
		result += IntToDigitBase(Floor(LFraction), Settings.CurrencyFormat);
		LFraction -= Floor(LFraction);
	end;
end;

end.

