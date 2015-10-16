program PePing;

// Periodic ping for a given URL

// Copyright (c) 2015 ChrisF
// Distributed under the terms of the MIT license: see LICENSE.txt

uses
  Forms,
  {$if (CompilerVersion < 18)}    // Before Delphi 2006
  UnitNoTI1 in 'UnitNoTI1.pas' {Form1};   // No TTrayicon in Form
  {$else}
  Unit1 in 'Unit1.pas' {Form1};
  {$ifend}

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.ShowMainForm:=False;  // Main Form is not first visible
  Application.Run;
end.
