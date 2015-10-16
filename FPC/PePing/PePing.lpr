program PePing;

// Periodic ping for a given URL

// Copyright (c) 2015 ChrisF
// Distributed under the terms of the MIT license: see LICENSE.txt

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Unit1
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.ShowMainForm:=False;  // Main Form is not first visible
  Application.Run;
end.

