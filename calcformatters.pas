unit CalcFormatters;

{$mode objfpc}{$H+}{$J-}

interface

uses SysUtils, Math, PNBase;

function FloatToBase(Value: TNumber; const Settings: TFormatSettings; var Overflow: Boolean): String;

implementation

const
	cDigits: Array[0 .. 15] of Char = '0123456789ABCDEF';
	cFormattingPrecision = {$IFDEF WINDOWS}12{$ELSE}15{$ENDIF};

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
	function ValidDigit(Digit: Char): Boolean;
	begin
		result := (Ord(Digit) >= Ord('0')) and (Ord(Digit) <= Ord('9'));
	end;
var
	LFloatRec: TFloatRec;
	LNumber: TNumber;
	LNumberString: String;
	LFraction: TNumber;
	LFractionString: String;
	LFractionDigits: UInt32;
	LPrecisionOverflow: Boolean;
	I: Int32;
begin
	LNumberString := '';
	FloatToDecimal(LFloatRec, Value, cFormattingPrecision + 1, cFormattingPrecision);

	result := '';
	if LFloatRec.Negative then result += '-';

	// seems like some of the string to float routines use shortstrings
	if LFloatRec.Exponent >= $ff then
		exit(result + 'Inf');
	if LFloatRec.Exponent < -$ff then
		exit('0');

	LPrecisionOverflow := True;
	for I := 0 to cFormattingPrecision - 1 do
		LPrecisionOverflow := LPrecisionOverflow and (Ord(LFloatRec.Digits[I]) <> 0);
	Overflow := LPrecisionOverflow or (Abs(LFloatRec.Exponent) >= cFormattingPrecision);

	for I := 0 to LFloatRec.Exponent - 1 do begin
		if (I < cFormattingPrecision) and ValidDigit(LFloatRec.Digits[I]) then
			LNumberString += LFloatRec.Digits[I]
		else
			LNumberString += '0';
	end;

	if LNumberString = '' then LNumberString := '0';

	LFractionString := '';
	for I := -1 downto LFloatRec.Exponent do begin
		LFractionString += '0';
	end;

	for I := Max(LFloatRec.Exponent, 0) to cFormattingPrecision - 1 do begin
		if not ValidDigit(LFloatRec.Digits[I]) then
			break;
		LFractionString += LFloatRec.Digits[I];
	end;

	LNumber := StrToFloat(LNumberString, Settings);
	LFractionDigits := Length(LFractionString);
	if LFractionDigits > 0 then
		LFraction := StrToFloat('0' + Settings.DecimalSeparator + LFractionString, Settings)
	else
		LFraction := 0;

	result += Settings.CurrencyString;
	result += IntToDigitBase(LNumber, Settings.CurrencyFormat);

	LFractionString := '';
	for I := 1 to cFormattingPrecision do begin
		LFraction := RoundTo(LFraction * Settings.CurrencyFormat, -cFormattingPrecision + I);
		if LFraction = 0 then break;

		LFractionString += IntToDigitBase(Int(LFraction), Settings.CurrencyFormat);
		LFraction := Frac(LFraction);
	end;

	if Length(LFractionString) > 0 then
		result += Settings.DecimalSeparator + LFractionString;
end;

end.

