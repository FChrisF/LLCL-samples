unit Unit1;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
//  {$mode delphi}
{$ENDIF}

interface

procedure CallRun;

//------------------------------------------------------------------------------

implementation

uses
  SysUtils;

const
  FILES_TOPROC    = '*.P*';           // *.PAS and *.PP
  FILENAME_INC    = 100;              // Incrementation for Dynamic Array of File Names

  STR_CLASSES     = 'CLASSES';
  STR_LCLASSES    = 'LClasses';
  FILES_CLASSES   = STR_CLASSES+'.PAS';   // Name of the Classes file (uppercase)
  FILES_LCLASSES  = STR_LCLASSES+'.pas';  // Name of the LClasses file
  FILES_EXTPROC:  array [0..Pred(5)] of String = ('.PAS','.@OPD@','.@PAS@','.PP','.@PP@');

var
  NbrFileNames:     Integer = 0;      // Number of File Names
  LengthFileNames:  Integer = 0;      // Real Number of File Names (i.e. Length of Dynamic Array of File Names)
  FileNames:        array of String;  // Dynamic Array of File Names

procedure InitFileNames(); forward;
procedure AddFileName(Const NewFileName: String); forward;
function  ProcessOnePasFile(Const FileName: String; Var NbrModifs: Integer): Boolean; forward;
function  ModifyOnePasFile(Const NameIn: String; Const NameOut: String; Const IsClasses: Boolean; Var NbrModifs: Integer): Boolean; forward;
function  MOPF_DoIt(Var InFileT: TextFile; Var OutFileT: TextFile; Const IsClasses: Boolean; Var NbrModifs: Integer): Boolean; forward;

function  BFileDelete(Const FileName: String): Longbool; forward;
function  BRenameFile_Mod(Const OldFileName: String; Const NewFileName: String): Longbool; forward;

//------------------------------------------------------------------------------

//
// Main Program Call
//
procedure CallRun;
var TSR1: TSearchRec;
var NbrModified: Integer;
var lError: Boolean;
var i1: Integer;
begin
  // Init
  WriteLn;
  InitFileNames;
  {$WARN SYMBOL_PLATFORM OFF}
  i1 := FindFirst(FILES_TOPROC, faArchive+faReadOnly, TSR1);
  {$WARN SYMBOL_PLATFORM ON}
  if i1<>0 then
    begin
      WriteLn('** ERROR **  No Pascal files found or OS error.');
      Exit;
    end;
  // Load all concerned file names
  while i1=0 do
    begin
      AddFileName(TSR1.Name);
      i1 := FindNext(TSR1);
    end;
  if NbrFileNames=0 then
    begin
      WriteLn('** ERROR **  No Pascal files to process or error.');
      Exit;
    end;
  // Process all files found
  lError := False;
  NbrModified := 0;
  for i1 := 0 to Pred(NbrFileNames) do
    begin
      if not ProcessOnePasFile(FileNames[i1], NbrModified) then
        begin
          WriteLn('** ERROR **  Error when processing Pascal file: ' + FileNames[i1]);
          Break;
        end;
    end;
  // Results
  InitFileNames;    // (Reset to 0)
  if not lError then
    if NbrModified=0 then
      WriteLn('** WARNING **  No Pascal files modified.')
    else
      WriteLn('-- OK -- Pascal files modified: ' + IntToStr(NbrModified));
end;

//
//  Initialization for File Names
//
procedure InitFileNames();
begin
  NbrFileNames := 0;
  LengthFileNames := 0;
  SetLength(FileNames, 0);
end;

//
//  Add a New File Name
//
procedure AddFileName(Const NewFileName: String);
begin
  if NbrFileNames<=LengthFileNames then
    begin
      Inc(LengthFileNames, FILENAME_INC);
      SetLength(FileNames, LengthFileNames);
    end;
  FileNames[NbrFileNames] := NewFileName;
  Inc(NbrFileNames);
end;

//
// Process One Pascal File
//
function  ProcessOnePasFile(Const FileName: String; Var NbrModifs: Integer): Boolean;
var TmpFileName, RenFileName: String;
var ExtOri, ExtOri2: String;
var IsClasses: Boolean;
begin
  result := False;
  // General (for LClasses)
  IsClasses := (UpperCase(FileName)=FILES_CLASSES);   // Because path is empty
  // .PAS file or .PP file
  if Pos(FILES_EXTPROC[0], UpperCase(FileName))>0 then
    begin
      ExtOri := FILES_EXTPROC[0];
      ExtOri2 := FILES_EXTPROC[2];
    end
  else
    begin
      ExtOri := FILES_EXTPROC[3];
      ExtOri2 := FILES_EXTPROC[4];
    end;
  // Renamed Pascal File
  RenFileName := StringReplace(FileName, ExtOri, ExtOri2, [rfIgnoreCase]);
  if (RenFileName='') or (RenFileName=FileName) then
    Exit;
  // Temporary Empty File
  TmpFileName := StringReplace(FileName, ExtOri, FILES_EXTPROC[1], [rfIgnoreCase]);
  if (TmpFileName='') or (TmpFileName=FileName) then
    Exit;
  if not BFileDelete(TmpFileName) then
    Exit;
  // Process Pascal File
  if not ModifyOnePasFile(FileName, TmpFileName, IsClasses, NbrModifs) then
    Exit;
  // Rename Files
  if not BRenameFile_Mod(FileName, RenFileName) then
    Exit
  else
    if not BRenameFile_Mod(TmpFileName, FileName) then
      Exit
    else
      if not BFileDelete(RenFileName) then
        Exit;
  // Rename the final file if Mode=LClasses and File=Classes.pas
  if IsClasses then
    if not BRenameFile_Mod(FileName, FILES_LCLASSES) then
      Exit;
  // All done
  result := True;
end;

//
//  Modify One Pascal File
//
function  ModifyOnePasFile(Const NameIn: String; Const NameOut: String; Const IsClasses: Boolean; Var NbrModifs: Integer): Boolean;
var InFileT: TextFile;
var OutFileT: TextFile;
var lError: Boolean;
begin
  result := False;
  // Checks if Pascal File is PResent
  if not FileExists(NameIn) then
    Exit;
  // Open Text Input File
  AssignFile(InFileT, NameIn);
  Filemode := fmOpenRead;
  lError := False;
  if IOResult()<>0 then
    lError := True
  else
    begin
      Reset(InFileT);
      if IOResult()<>0 then
        lError := True
    end;
  if lError then
    begin
      CloseFile(InFileT);
      IOResult();
      Exit;
    end;
  // Create Text Output File
  AssignFile(OutFileT, NameOut);
  if IOResult()<>0 then
    lError := True
  else
    begin
      Rewrite(OutFileT);
      if IOResult()<>0 then
        lError := True
    end;
  if lError then
    begin
      CloseFile(InFileT);
      IOResult();
      CloseFile(OutFileT);
      IOResult();
      Exit;
    end;
  // Process Input File
  if not MOPF_DoIt(InFileT, OutFileT, IsClasses, NbrModifs) then
    lError := True;
  // Close Files
  CloseFile(InFileT);
  if IOResult()<>0 then
    if not lError then lError := True;
  CloseFile(OutFileT);
  if IOResult()<>0 then
    if not lError then lError := True;
  result := (not lError)
end;
//  Do the Modification(s)
function  MOPF_DoIt(Var InFileT: TextFile; Var OutFileT: TextFile; Const IsClasses: Boolean; Var NbrModifs: Integer): Boolean;
var InUsesClause: Integer;  // Clause 'uses': 0=Before, 1=In, 2=After
var lError: Boolean;
var s1, s2, s3: String;
var i1: Integer;
begin
  lError := False;
  InUsesClause := 0;
  while not Eof(InFileT) do
    begin
      IOResult();
      ReadLn(InFileT, s1);
      if IOResult()=0 then
        begin
          s2 := s1;
          s3 := UpperCase(s1);
          // Everything here:
          // - must be EXACTLY equal to the tested strings
          // - doesn't accept comments in the concerned part
          if IsClasses then
            begin
              if s3='UNIT ' + STR_CLASSES + ';' then
                s2 := 'unit ' + STR_LCLASSES + ';';
            end
          else
            begin
              if InUsesClause=0 then
                if (s3='USES') or (Copy(s3, 1, 5)='USES ') then
                  InUsesClause := 1;
              if InUsesClause=1 then
                begin
                  if Pos(UpperCase(STR_LCLASSES), s3)=0 then
                    begin
                      i1 := Pos(STR_CLASSES, s3);
                      if i1>0 then
                        s2 := Copy(s1, 1, i1-1) + STR_LCLASSES + Copy(s1, i1+7, Length(s1)-(i1+7)+1);
                    end;
                  if Pos(';', s1)>0 then
                    InUsesClause := 2;    // Classes, if present, is always in the interface section for LLCL files
                end;
            end;
          if ((s1='') and (s2<>'')) or ((s1<>'') and (s2='')) then
            begin
              lError := True;
              Break;
            end
          else
            begin
              if s2<>s1 then Inc(NbrModifs);
              IOResult();
              WriteLn(OutFileT, s2);
              if IOResult()<>0 then
                begin
                  lError := True;
                  Break;
                end;
            end;
        end
      else
        begin
          lError := True;
          Break;
        end;
    end;
  result := (not lError)
end;

//------------------------------------------------------------------------------

//
// Delete a File (if Present)
//
function  BFileDelete(Const FileName: String): Longbool;
var F1: File;
begin
  if FileExists(FileName) then
    begin
      AssignFile(F1, FileName);
      IOResult();
      Erase(F1);
      IOResult();
    end;
  result := (not FileExists(FileName));
end;

//
//  Rename a File (if Present) - Modified (Existing Target File is Not Deleted First)
//
function  BRenameFile_Mod(Const OldFileName: String; Const NewFileName: String): Longbool;
var F1: File;
begin
  result := False;
  if FileExists(OldFileName) then
    begin
//      BFileDelete(NewFileName);
      if not FileExists(NewFileName) then
        begin
          AssignFile(F1, OldFileName);
          IOResult();
          Rename(F1, NewFileName);
          IOResult();
          if FileExists(NewFileName) then
            result := True;
        end;
    end;
end;

end.
 
