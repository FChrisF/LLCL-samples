program FileCRC;

// CRC32 and hash computes (MD5/SHA-1) for a file

// Copyright (c) 2015 ChrisF
// Distributed under the terms of the MIT license: see LICENSE.txt

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
