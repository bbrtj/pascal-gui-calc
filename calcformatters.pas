unit CalcFormatters;

{$mode objfpc}{$H+}{$J-}

interface

uses SysUtils, Math, PNBase;

function FloatToBase(Value: TNumber; const Settings: TFormatSettings; var Overflow: Boolean): String;

implementation

const
	cDigits: Array[0 .. 15] of Char = '0123456789ABCDEF';
	cFormattingPrecision = 15;

{ helper }
function IntToDigitBase(Value: TNumber; Base: UInt8 = 10): String;
var
	LMod: TNumber;
begin
	if (Base > 16) or (Base < 2) then
		raise Exception.Create('Invalid base');

	result := '';
	repeat
		Value := Value / Base;
		LMod := Frac(Value) * Base;
		result := cDigits[Round(LMod)] + result;
		Value := Int(Value);
	until Value = 0;
end;

{ small hack: uses currency fields from TFormatSettings to specify base }
function FloatToBase(Value: TNumber; const Settings: TFormatSettings; var Overflow: Boolean): String;
var
	LDigits: Int32;
	LFraction: TNumber;
	LFractionDigits: Int32;
	LFractionDigitsUsed: Int32;
	LFractionString: String;
	I: Int32;
begin
	result := '';
	if Value < 0 then begin
		result += '-';
		Value := -1 * Value;
	end;

	result += Settings.CurrencyString;
	result += IntToDigitBase(Int(Value), Settings.CurrencyFormat);
	LDigits := Floor(Log10(Value));

	LFraction := Frac(Value);
	LFractionDigits := cFormattingPrecision - LDigits;
	LFraction := RoundTo(LFraction, -LFractionDigits);

	// TODO: these are not actually digits in the loop, so it needs more work
	LFractionString := '';
	LFractionDigitsUsed := 0;
	for I := 1 to LFractionDigits do begin
		LFraction := RoundTo(LFraction * Settings.CurrencyFormat, -LFractionDigits + I);
		if LFraction = 0 then break;

		LFractionString += IntToDigitBase(Int(LFraction), Settings.CurrencyFormat);
		LFraction := Frac(LFraction);
		Inc(LFractionDigitsUsed);
	end;

	if Length(LFractionString) > 0 then
		result += Settings.DecimalSeparator + LFractionString;

	Overflow := LDigits + LFractionDigitsUsed >= cFormattingPrecision;
end;

end.

