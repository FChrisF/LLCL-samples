unit Unit2;

//
// Compare and search for identical files (same size, same content)
//   (Indicate a valid search path for the LLCL files before compiling)
//
// Results unit
//

// Copyright (c) 2016 ChrisF
// Distributed under the terms of the MIT license: see LICENSE.txt

{$IFDEF FPC}
  {$mode objfpc}{$H+}
//  {$mode delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Grids, ClipBrd;

type
{$IFDEF FPC}
  TFileSize       = uint64;
{$ELSE}
  {$if (CompilerVersion < 18)}   // Before Delphi 2006
  TFileSize       = cardinal;
  {$else}
  TFileSize       = uint64;
  {$ifend}
{$ENDIF}

type

  { TForm2 }

  TForm2 = class(TForm)
    Button1: TButton;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    StaticText3: TStaticText;
    StaticText4: TStaticText;
    Button2: TButton;
    StringGrid1: TStringGrid;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { private declarations }
    gWidthDiff: Integer;
    gHeightDiff: Integer;
    sWidthDiff: Integer;
    MaxLenFileName1: Integer;
    MaxLenFileName2: Integer;
{$IFNDEF FPC}
    FirstAdd: Boolean;
{$ENDIF}
  public
    { public declarations }
    procedure CenterForm;
    procedure InitDirs(Const DirName1: String; Const DirName2: String);
    procedure AddSameFiles(Const FileName1: String; Const FileName2: String; Const FilesSize: TFileSize; Const NbrCouples: Integer);
  end;

var
  Form2: TForm2;

//------------------------------------------------------------------------------

implementation

{$IFDEF FPC}
  {$R *.lfm}
{$ELSE}
  {$R *.dfm}
{$ENDIF}

{$if Defined(FPC) and not Defined(UNICODE)}
  {$define FPC_NON_UNICODE16}
{$ifend}

uses
{$IFDEF FPC_NON_UNICODE16}
  LazUTF8,
{$ENDIF}
  Unit1;

const
  CHAR_MINUS      = '-';					// Minus Character
  STR_EXCLAM      = ' ! ';

function  LPaddedNumber(Const Value: Integer; Const PrefLen: Integer): String; forward;
function  PaddedString(Const Value: String; Const PrefLen: Integer; Const LeftPad: Boolean): String; forward;
procedure MaxWinLength(Const Value: String; Var CurrentMax: Integer); forward;

//------------------------------------------------------------------------------

procedure TForm2.FormCreate(Sender: TObject);
begin
  // For FormResize
  gHeightDiff := Height - StringGrid1.Height;
  gWidthDiff := Width - StringGrid1.Width;
  sWidthDiff := Width - StaticText3.Width;
  // Grid title
  StringGrid1.Cells[0, 0] := 'C';   // For several couples (i.e. file identical with more than one other file)
  StringGrid1.Cells[1, 0] := 'Files';
  StringGrid1.Cells[2, 0] := 'Identical Files';
  StringGrid1.Cells[3, 0] := 'Files Size';
end;

procedure TForm2.FormResize(Sender: TObject);
var i1, i2: Integer;
begin
  // Update some controls to "follow" the form
  i2 := Height - gHeightDiff;
  if i2<0 then i2 := 0;
  i1 := Width - gWidthDiff;
  if i1<0 then i1 := 0;
  StringGrid1.SetBounds(StringGrid1.Left, StringGrid1.Top, i1, i2);
  i1 := Width - sWidthDiff;
  if i1<0 then i1 := 0;
  StaticText3.SetBounds(StaticText3.Left, StaticText3.Top, i1, StaticText3.Height);
  StaticText4.SetBounds(StaticText4.Left, StaticText4.Top, i1, StaticText4.Height);
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm2.Button2Click(Sender: TObject);
var s1: String;
var i1: Integer;
begin
  s1 := 'Directory 1: ' + StaticText3.Caption + sLineBreak;
  if StaticText4.Caption<>'' then
    s1 := s1 + 'Directory 2: ' + StaticText4.Caption + sLineBreak;
  s1 := s1 + sLineBreak + String(StringOfChar(CHAR_MINUS, MaxLenFileName1 + 3 + MaxLenFileName2 + 3 + 12 + 3)) + sLineBreak;
  for i1 := 1 to Pred(StringGrid1.RowCount) do
    s1 := s1 + PaddedString(StringGrid1.Cells[1, i1], MaxLenFileName1, False) + STR_EXCLAM
            + PaddedString(StringGrid1.Cells[2, i1], MaxLenFileName2, False) + STR_EXCLAM
            + StringGrid1.Cells[3, i1] + sLineBreak;    // (already padded)
  s1 := s1 + String(StringOfChar(CHAR_MINUS, MaxLenFileName1 + 3 + MaxLenFileName2 + 3 + 12 + 3)) + sLineBreak;
  Clipboard.AsText := s1;
end;

procedure TForm2.CenterForm;
var i1, i2: Integer;
begin
  // Center form with parent (= Form1)
  i1 := Form1.Left + ((Form1.Width - Width) div 2);
  if i1<0 then i1 := 0;
	i2 := Form1.Top + ((Form1.Height - Height) div 2);
	if i2<0 then i2 := 0;
  SetBounds(i1, i2, Width, Height);
end;

procedure TForm2.InitDirs(Const DirName1: String; Const DirName2: String);
begin
  // Initializations
  StaticText3.Caption := DirName1;
  StaticText4.Caption := DirName2;
{$IFDEF FPC}
  StringGrid1.RowCount := 1;    // Only the title
  StringGrid1.SortColRow(True, 3);      // Because, it's already the case
  StringGrid1.SortOrder := soAscending; //   due to the comparing algorithm used
{$ELSE}
  FirstAdd := True;             // Because of a bug in the VCL
  StringGrid1.RowCount := 2;    //   when fixed row is present
  {$IF Declared(LLCLVersion)}
  StringGrid1.SortColRow(True, 3);      // Because, it's already the case
  StringGrid1.SortOrder := soAscending; //   due to the comparing algorithm used
  {$IFEND}
{$ENDIF}
  MaxLenFileName1 := 1;
  MaxLenFileName2 := 1;
end;

procedure TForm2.AddSameFiles(Const FileName1: String; Const FileName2: String; Const FilesSize: TFileSize; Const NbrCouples: Integer);
var i1: Integer;
begin
{$IFNDEF FPC}
  // Skip increment the first time
  if FirstAdd then
    begin
      FirstAdd := False;
      i1 := StringGrid1.RowCount -1 ;
    end
  else
{$ENDIF}
    begin
      // Add one row data in grid
      i1 := StringGrid1.RowCount;
      StringGrid1.RowCount := i1 + 1;
    end;
  if NbrCouples=0 then
    StringGrid1.Cells[0, i1] := ''    // Nothing for the first one
  else
    StringGrid1.Cells[0, i1] := LPaddedNumber(Succ(NbrCouples), 3);
  StringGrid1.Cells[1, i1] := FileName1;
  StringGrid1.Cells[2, i1] := FileName2;
  StringGrid1.Cells[3, i1] := LPaddedNumber(FilesSize, 12);
  MaxWinLength(FileName1, MaxLenFileName1);
  MaxWinLength(FileName2, MaxLenFileName2);
end;

// Returns "padded" string from number, for better sort results
function  LPaddedNumber(Const Value: Integer; Const PrefLen: Integer): String;
begin
  result := PaddedString(IntToStr(Value), PrefLen, True);
end;

function  PaddedString(Const Value: String; Const PrefLen: Integer; Const LeftPad: Boolean): String;
var s1: String;
var i1: Integer;
begin
  s1 := Value;
  i1 := PrefLen - Length({$IFDEF FPC_NON_UNICODE16}UTF8ToWinCP(s1){$ELSE}s1{$ENDIF});
  if i1>0 then
    if LeftPad then
      s1 := String(StringOfChar(' ', i1)) + s1
    else
      s1 := s1 + String(StringOfChar(' ', i1));
  result := s1;
end;

procedure MaxWinLength(Const Value: String; Var CurrentMax: Integer);
var i1: Integer;
begin
  i1 := Length({$IFDEF FPC_NON_UNICODE16}UTF8ToWinCP(Value){$ELSE}Value{$ENDIF});
  if i1>CurrentMax then
    CurrentMax := i1;
end;

//------------------------------------------------------------------------------

end.
