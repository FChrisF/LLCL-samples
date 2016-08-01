unit Unit1;

//
// Compare and search for identical files (same size, same content)
//   (Indicate a valid search path for the LLCL files before compiling)
//
// Main unit
//

// Copyright (c) 2016 ChrisF
// Distributed under the terms of the MIT license: see LICENSE.txt

{$IFDEF FPC}
  {$mode objfpc}{$H+}
//  {$mode delphi}
{$ELSE}
  {$warn UNIT_PLATFORM off}     // For FileCtrl
{$ENDIF}
{$IFDEF FPC_OBJFPC} {$DEFINE IS_FPC_OBJFPC_MODE} {$ENDIF}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, ComCtrls
  {$IFNDEF FPC}, XPMan{$ENDIF};

type

  { TForm1 }

  TForm1 = class(TForm)
{$IFNDEF FPC}
    XPManifest1: TXPManifest;
{$ENDIF}
    GroupBox1: TGroupBox;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Edit1: TEdit;
    Button4: TButton;
    Edit2: TEdit;
    Button5: TButton;
    Button6: TButton;
    ProgressBar1: TProgressBar;
    StaticText1: TStaticText;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

//------------------------------------------------------------------------------

implementation

// Possible program options
{$DEFINE USE_THREAD}          // Use thread for the files compare
{$DEFINE ERROR_STOP}          // Stop when one error found for a file

{$if Defined(FPC) and (not Defined(UNICODE))}
  {$DEFINE USE_FPC_UTF8_FILEFUNC}
{$ifend}

uses
  Unit2,
{$IFDEF USE_FPC_UTF8_FILEFUNC}
  LazFileUtils,
{$ENDIF}
{$IFDEF FPC}
  Dialogs, LCLIntF, Messages;
{$ELSE}
  FileCtrl, Windows, Messages;
{$ENDIF}

{$IFDEF FPC}
  {$R *.lfm}
{$ELSE}
  {$R *.dfm}
{$ENDIF}

const
	CHAR_ASTERISK		= '*';					// Asterisk Character
	CHAR_PERIOD			= '.';					// Period Character
	CHAR_BACKS			= '\';					// Backslash Character

	FILEBUF_SIZE1		= 4096;					// Size1 for File Buffer (Small, Because Different is Priviledged)
	FILEBUF_SIZE2		= 1048576;			// Size2 for File Buffer (Greater, Because Identical is Priviledged)
	FILESDIR_INC		= 1024;					// Increment for FilesDir arrays
	FILESDIR_MAX		= 50000;			  // Maximum for FilesDir arrays
	FILESDIR_MASK		= CHAR_ASTERISK + CHAR_PERIOD + CHAR_ASTERISK;	// Files name mask for FilesDir
{$IFDEF FPC}
  FILESDIR_ATTR   = faArchive or faReadOnly;                      // Files attribute for FilesDir
{$ELSE}
  // Avoid compilation warnings
  FILESDIR_ATTR   = $20 or $01;                                   // Files attribute for FilesDir
{$ENDIF}

type Files_Dir = record
  FileName:			String;
  FileSize:     TFileSize;
	Index:				Integer;
end;
type Files_DirArr = array of Files_Dir;

procedure DispMessage(Const StrMess: String); forward;
procedure AddMessage(Const StrMess: String); forward;
procedure DispError(Const ErrMess: String); forward;

procedure DoCompFiles(Const DirName1: String; Const DirName2: String; Const hWndButton: THandle; Const hWndButtonParent: THandle; Var ResType: Integer; Var ResStr: String); forward;
procedure CompFilesSortDir(Const DirName1: String; Const FilesDir1: Files_DirArr; Const NbrFilesDir1: Integer; Const DirName2: String; Const FilesDir2: Files_DirArr; Const NbrFilesDir2: Integer; Const Has2Dirs: Boolean; Const hWndButton: THandle; Const hWndButtonParent: THandle; Var NbrSameFiles: Integer; Var NbrErrorFiles: Integer; Var FileError: Integer; Var FileNameError: String); forward;
procedure CFSD_ButtonClick(Const hWndButton: THandle; Const hWndButtonParent: THandle); forward;
function  Compare2Files(Const FileName1: String; Const FileName2: String; Const FilesSize: TFileSize; Var FileNameErrorType: Integer): Integer; forward;

{$IFDEF USE_THREAD}
type
  TDoCompFilesThr = class(TThread)
  private
    DirName1: String;
    DirName2: String;
    TimerThr: TTimer;
    hWndButtonThr: THandle;
    hWndButtonParentThr: THandle;
    procedure OnThreadTerminate(Sender: TObject);
  public
    procedure Execute; override;
  end;

  Same_Files = record
    FileName1:    String;
    FileName2:    String;
    FilesSize:    TFileSize;
    NbrCouples:   Integer
  end;
  Same_FilesArr = array of Same_Files;

const
  SAMEFILES_INC   = 100;          // Increment for SameFiles array

function  CallDoCompFilesThr(Const DirName1: String; Const DirName2: String): Boolean; forward;
procedure ResDoCompFilesThr; forward;
{$ENDIF}

function  LoadDir(Const DirName: String; Var FilesDir: Files_DirArr; Var NbrFilesDir: Integer): Boolean; forward;
procedure SortDir(Var FilesDir: Files_DirArr; Const NbrFilesDir: Integer); forward;
procedure SD_Sort(Var FilesDir: Files_DirArr; Const StartPos: Integer; Const EndPos: Integer); forward;
function  SD_Partition(Var FilesDir: Files_DirArr; Const StartPos: Integer; Const EndPos: Integer): Integer; forward;
function  SD_PartComp(Const FilesDir: Files_DirArr; Const Ind1: Integer; Const Ind2: Integer): Integer; forward;

// Global variables
var
  gFBuffer1:		  array[0..Pred(FILEBUF_SIZE2)] of Byte;
  gFBuffer2:		  array[0..Pred(FILEBUF_SIZE2)] of Byte;
	gFilesDir1:			Files_DirArr;
	gFilesDir2:			Files_DirArr;
	gNbrFilesDir1:	Integer;
	gNbrFilesDir2:	Integer;

{$IFDEF USE_THREAD}
var
  DoCompFilesThr:     TDoCompFilesThr;
  ThreadIsRunning:    Boolean = False;
  ThreadFormDestroy:  Boolean = False;
  ThreadResType:      Integer;
  ThreadResStr:       String;
  gSameFiles:         Same_FilesArr;    // To not modify directly
  gNbrSameFiles:      Integer;          //   Form2 controls in the thread
{$ENDIF}

//------------------------------------------------------------------------------

procedure TForm1.Button1Click(Sender: TObject);
begin
  // Quits (End of program)
  //    (Cancel Button - Escape key)
  Application.Terminate;
end;

procedure TForm1.Button2Click(Sender: TObject);
{$IFNDEF USE_THREAD}
var ResType: Integer;
var ResStr: String;
{$ENDIF}
var sDirName1, sDirName2: String;
begin
  {$IFDEF USE_THREAD}
  // If abort called during thread run
  if ThreadIsRunning then
    begin
      DoCompFilesThr.Terminate;
      Exit;
    end;
  {$ENDIF}
  // Always
  Button3.Enabled := False;
  // Checks directory/directories
  sDirName1 := Trim(Edit1.Text);
  if sDirName1<>'' then
    begin
      if sDirName1[Length(sDirName1)]<>CHAR_BACKS then
        sDirName1 := sDirName1 + CHAR_BACKS;
      if not {$IFDEF USE_FPC_UTF8_FILEFUNC}DirectoryExistsUTF8{$ELSE}DirectoryExists{$ENDIF}(sDirName1) then
        begin
          DispError('  Invalid or unknown directory 1');
          Exit;
        end;
    end;
  sDirName2 := Trim(Edit2.Text);
  if sDirName2<>'' then
    begin
      if sDirName2[Length(sDirName2)]<>CHAR_BACKS then
        sDirName2 := sDirName2 + CHAR_BACKS;
      if not {$IFDEF USE_FPC_UTF8_FILEFUNC}DirectoryExistsUTF8{$ELSE}DirectoryExists{$ENDIF}(sDirName2) then
        begin
          DispError('  Invalid or unknown directory 2');
          Exit;
        end;
    end;
  if (sDirName1='') and (sDirName2='') then
    begin
      DispError('  Fill at least one directory');
      Exit;
    end;
  if sDirName1='' then
    begin
      sDirName1 := sDirName2;   // First directory always non empty
      sDirName2 := '';
    end;
  // Compares files
  if sDirName2='' then
    DispMessage('  Loading directory infos ...')
  else
    DispMessage('  Loading directories infos ...');
  ProgressBar1.Position := 0;
  ProgressBar1.Max := 0;        // Update later, with first call to stepit
  Application.ProcessMessages;
  Form2.InitDirs(sDirName1, sDirName2);
  {$IFDEF USE_THREAD}
  CallDoCompFilesThr(sDirName1, sDirName2);
  if ThreadResType<>0 then
    ResDoCompFilesThr;
  {$ELSE}
  DoCompFiles(sDirName1, sDirName2, Button6.Handle, Button6.Parent.Handle, ResType, ResStr);
  ProgressBar1.Position := 0;
  // Display results
  if ResType=0 then
    begin
      AddMessage(' done.');
      Button3.Enabled := True;
      DispMessage(ResStr);
    end
  else
    DispError(ResStr);
  {$ENDIF}
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Form2.CenterForm;
  Form2.ShowModal;
end;

procedure TForm1.Button4Click(Sender: TObject);
var s1: String;
begin
  if SelectDirectory('Select directory 1', Trim(Edit1.Text), s1) then
    Edit1.Text := s1;
end;

procedure TForm1.Button5Click(Sender: TObject);
var s1: String;
begin
  if SelectDirectory('Select directory 2', Trim(Edit2.Text), s1) then
    Edit2.Text := s1;
end;

// (Invisible button)
procedure TForm1.Button6Click(Sender: TObject);
begin
  if ProgressBar1.Max<>gNbrFilesDir1 then   // First call
    // Indicates the loading of directory/ies is done
    begin
      ProgressBar1.Max := gNbrFilesDir1;
      AddMessage(' done.' + sLineBreak + sLineBreak + '  Comparing files ...');
    end
  else
    // Updates ProgressBar indicator
    ProgressBar1.StepIt;
  {$IFNDEF USE_THREAD}
  Application.ProcessMessages;
  {$ENDIF}
end;

{$IFDEF USE_THREAD}
procedure TForm1.Timer1Timer(Sender: TObject);
var i1: Integer;
begin
  // Timer stopped
  Timer1.Enabled := False;
  // Do not display results, if thread aborted because application si closing
  if (not ThreadFormDestroy) then
    begin
      if ThreadResType=0 then
        for i1 := 0 to Pred(gNbrSameFiles) do
          Form2.AddSameFiles(gSameFiles[i1].FileName1, gSameFiles[i1].FileName2, gSameFiles[i1].FilesSize, gSameFiles[i1].NbrCouples);
      SetLength(gSameFiles, 0);   // Free memory
      ResDoCompFilesThr;
    end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  // Terminates thread if running
  if ThreadIsRunning then
    begin
      ThreadFormDestroy := True;  // Thread aborted because application si closing
      DoCompFilesThr.Terminate;
      DoCompFilesThr.WaitFor;
    end;
end;
{$ELSE}
procedure TForm1.Timer1Timer(Sender: TObject);
begin
  // Do nothing
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  // Do nothing
end;
{$ENDIF}

//------------------------------------------------------------------------------

//
// Displays a standard message (not an error)
//
procedure DispMessage(Const StrMess: String);
begin
  Form1.StaticText1.Caption := sLineBreak + StrMess;
end;

//
// Adds a standard message (not an error) to the current one
//
procedure AddMessage(Const StrMess: String);
begin
  Form1.StaticText1.Caption := Form1.StaticText1.Caption + StrMess;
end;

//
// Displays an error message
//
procedure DispError(Const ErrMess: String);
begin
  Form1.StaticText1.Caption := sLineBreak + '  *** ERROR ***' + sLineBreak + sLineBreak + ErrMess;
end;

//------------------------------------------------------------------------------

{$IFDEF USE_THREAD}
//
// Calls Files Compare Thread
//
function  CallDoCompFilesThr(Const DirName1: String; Const DirName2: String): Boolean;
begin
  // Creates thread
  result := False;
  ThreadResType := 0;
  ThreadResStr := '';
  try
    DoCompFilesThr := TDoCompFilesThr.Create(True);
  except
    ThreadResType := 1;
    ThreadResStr := '  Internal error: Can''t start thread to process files compare';
    Exit;
  end;
  result := True;
  // Initializations
  Form1.Button1.Enabled := False;
  Form1.Button2.Caption := '&Abort';
//  Form1.Button3.Enabled := False;     // Already done
  Form1.Edit1.Enabled := False;
  Form1.Edit2.Enabled := False;
  Form1.Button4.Enabled := False;
  Form1.Button5.Enabled := False;
//  Form1.ProgressBar1.Position := 0;   // Already done
  Application.ProcessMessages;
  // Thread parameters
  DoCompFilesThr.FreeOnTerminate := True;
  DoCompFilesThr.OnTerminate := {$IFDEF IS_FPC_OBJFPC_MODE}@{$ENDIF}DoCompFilesThr.OnThreadTerminate;
  DoCompFilesThr.DirName1 := DirName1;
  DoCompFilesThr.DirName2 := DirName2;
  DoCompFilesThr.TimerThr := Form1.Timer1;
  DoCompFilesThr.hWndButtonThr := Form1.Button6.Handle;
  DoCompFilesThr.hWndButtonParentThr := Form1.Button6.Parent.Handle;
  // Starts thread
  ThreadIsRunning := True;
  // Resume is deprecated since Delphi 2010+ and FPC 2.4.4 (Start should be used instead)
{$IFDEF FPC}
  DoCompFilesThr.Start;
{$ELSE FPC}
  {$if CompilerVersion >= 21}       // Delphi 2010 or after
  DoCompFilesThr.Start;
  {$else}
  DoCompFilesThr.Resume;
  {$ifend}
{$ENDIF FPC}
end;

//
// Results for Files Compare Thread
//
procedure ResDoCompFilesThr;
begin
  Form1.Button1.Enabled := True;
  Form1.Button2.Caption := '&Compare';
  Form1.Edit1.Enabled := True;
  Form1.Edit2.Enabled := True;
  Form1.Button4.Enabled := True;
  Form1.Button5.Enabled := True;
  Form1.ProgressBar1.Position := 0;
  if ThreadResType=0 then
    begin
      AddMessage(' done.');
      Form1.Button3.Enabled := True;
      DispMessage(ThreadResStr);
    end
  else
    DispError(ThreadResStr);
end;

procedure TDoCompFilesThr.Execute;
begin
  DoCompFiles(DirName1, DirName2, hWndButtonThr, hWndButtonParentThr, ThreadResType, ThreadResStr);
  ThreadIsRunning := False;
end;

procedure TDoCompFilesThr.OnThreadTerminate(Sender: TObject);
begin
  // Function run into calling thread for LLCL: can't use LLCL directly
  // Timer started
  TimerThr.Enabled := True;
end;
{$ENDIF}

//
// Files Compare
//   Search for identical files within a single
//     directory or between 2 directories
//
procedure DoCompFiles(Const DirName1: String; Const DirName2: String; Const hWndButton: THandle; Const hWndButtonParent: THandle; Var ResType: Integer; Var ResStr: String);
var Has2Dirs: Boolean;
var FileError: Integer;
var FileNameError: String;
var NbrErrorFiles, NbrSameFiles: Integer;
begin
  ResType := 1;   // (error by default)
  Has2Dirs := (DirName2<>'');
  // Load files list from directory/directoris
  if not LoadDir(DirName1, gFilesDir1, gNbrFilesDir1) then
    begin
      ResStr := '  Can''t load files list from directory 1';
      SetLength(gFilesDir1, 0);         // Free memory
      Exit;
    end;
  SortDir(gFilesDir1, gNbrFilesDir1);
  if Has2Dirs then
    begin
      if not LoadDir(DirName2, gFilesDir2, gNbrFilesDir2) then
        begin
          ResStr := '  Can''t load files list from directory 2';
          SetLength(gFilesDir1, 0);     // Free memory
          SetLength(gFilesDir2, 0);     // Free memory
          Exit;
        end;
      SortDir(gFilesDir2, gNbrFilesDir2);
    end;
  // Compare Files
  if Has2Dirs then
    CompFilesSortDir(DirName1, gFilesDir1, gNbrFilesDir1, DirName2, gFilesDir2, gNbrFilesDir2, Has2Dirs, hWndButton, hWndButtonParent, NbrSameFiles,  NbrErrorFiles, FileError, FileNameError)
  else
    CompFilesSortDir(DirName1, gFilesDir1, gNbrFilesDir1, DirName1, gFilesDir1, gNbrFilesDir1, Has2Dirs, hWndButton, hWndButtonParent, NbrSameFiles,  NbrErrorFiles, FileError, FileNameError);
  // Error(s) ?
  if FileError=0 then
    begin
      ResType := 0;
      ResStr := '  File(s) compared:  ' + IntToStr(gNbrFilesDir1);
      if Has2Dirs then
        ResStr := ResStr  + ' x ' + IntToStr(gNbrFilesDir2);
      if (gNbrFilesDir1<>0) and ((not Has2Dirs) or (gNbrFilesDir2<>0)) then
        begin
          ResStr := ResStr + sLineBreak + sLineBreak;
          if NbrSameFiles>0 then
            ResStr := ResStr + '   -- Identical couple(s) of files:  ' + IntToStr(NbrSameFiles)
          else
            ResStr := ResStr + '   -- No matches.';
          {$IFNDEF ERROR_STOP}
          if NbrErrorFiles<>0 then
            ResStr := ResStr + sLineBreak + sLineBreak + '   -- Error(s) during compare:  ' + IntToStr(NbrErrorFiles);
          {$ENDIF ERROR_STOP}
        end;
    end
  else
    {$IFDEF USE_THREAD}
    // Aborted by user
    if FileError=20 then
      begin
        if (not ThreadFormDestroy) then
          ResStr := '  Operation aborted by user';
      end
    else
    {$ENDIF}
      // Error during file processing
      ResStr := '  Can''t process file: '+FileNameError+' (Error '+IntToStr(FileError)+')';
  SetLength(gFilesDir1, 0);
  SetLength(gFilesDir2, 0);
end;

//
// Files Compare from Sorted Directory/Directories
//
procedure CompFilesSortDir(Const DirName1: String; Const FilesDir1: Files_DirArr; Const NbrFilesDir1: Integer; Const DirName2: String; Const FilesDir2: Files_DirArr; Const NbrFilesDir2: Integer; Const Has2Dirs: Boolean; Const hWndButton: THandle; Const hWndButtonParent: THandle; Var NbrSameFiles: Integer; Var NbrErrorFiles: Integer; Var FileError: Integer; Var FileNameError: String);
var FileNameErrorType: Integer;
var NbrCouples: Integer;
{$IFDEF USE_THREAD}
var LenSameFiles: Integer;
{$ENDIF}
var Ind1, Ind2: Integer;
var i1, i2, i3, i4: Integer;
var fs1, fs2: TFileSize;
begin
  NbrSameFiles := 0;
  {$IFDEF USE_THREAD}
  SetLength(gSameFiles, SAMEFILES_INC);
  LenSameFiles := SAMEFILES_INC;
  gNbrSameFiles := 0;
  {$ENDIF}
  NbrErrorFiles := 0;
  FileError := 0;
  FileNameError := '';
  // Indicate the loading of directory/ies is done (first call)
  CFSD_ButtonClick(hWndButton, hWndButtonParent);
  // Directory 1
  for i1 := 0 to Pred(NbrFilesDir1) do
    begin
      {$IFDEF USE_THREAD}
      // If abort called during thread run
      if DoCompFilesThr.Terminated then
        begin
          FileError := 20;
          Break;
        end;
      {$ENDIF}
      Ind1 := FilesDir1[i1].Index;
      fs1 := FilesDir1[Ind1].FileSize;
			NbrCouples := 0;
      // Directory 2, or same Directory
      i3 := 0;
      if Has2Dirs then i4 := 0 else i4 := Succ(i1);
      for i2 := i4 to Pred(NbrFilesDir2) do
        begin
          Ind2 := FilesDir2[i2].Index;
          fs2 := FilesDir2[Ind2].FileSize;
          if fs2<fs1 then
            Continue
          else
            if fs2>fs1 then
              Break;
          // Compare these 2 files with the same size
          i3 := Compare2Files(DirName1 + FilesDir1[Ind1].FileName, DirName2 + FilesDir2[Ind2].FileName, fs1, FileNameErrorType);
          if i3<0 then      // Error
            begin
            {$IFDEF ERROR_STOP}
              if i3<>-20 then     // Aborted by user
                begin
                  if FileNameErrorType=1 then
                    FileNameError := FilesDir1[Ind1].FileName
                  else
                    FileNameError := FilesDir2[Ind2].FileName;
                  if Has2Dirs then
                    FileNameError := FileNameError + '  in directory ' + IntToStr(FileNameErrorType) + '  ';
                end;
              FileError := Abs(i3);
              Break;
            {$ELSE}
              i3 := 0;        // Don't break
              Inc(NbrErrorFiles);
            {$ENDIF}
            end
          else
            if i3=0 then    // Same file
              begin
                {$IFDEF USE_THREAD}
                if LenSameFiles=gNbrSameFiles then
                  begin
        						Inc(LenSameFiles, SAMEFILES_INC);     // Maximum is indirectly limited
                		SetLength(gSameFiles, LenSameFiles);  //   by maximum for FilesDir arrays
                  end;
                gSameFiles[gNbrSameFiles].FileName1 := FilesDir1[Ind1].FileName;
                gSameFiles[gNbrSameFiles].FileName2 := FilesDir2[Ind2].FileName;
                gSameFiles[gNbrSameFiles].FilesSize := fs1;
                gSameFiles[gNbrSameFiles].NbrCouples := NbrCouples;
                Inc(gNbrSameFiles);
                {$ELSE}
                Form2.AddSameFiles(FilesDir1[Ind1].FileName, FilesDir2[Ind2].FileName, fs1, NbrCouples);
                {$ENDIF}
                Inc(NbrSameFiles);
                Inc(NbrCouples);
              end;
        end;
      if i3<0 then Break;
      // Update progress bar indicator
      CFSD_ButtonClick(hWndButton, hWndButtonParent);
    end;
end;
// Simulates click for PushButton 6
//   to indicate the loading of directory/ies is done
//   then, to update the progress bar indicator
procedure CFSD_ButtonClick(Const hWndButton: THandle; Const hWndButtonParent: THandle);
begin
  PostMessage(hWndButtonParent, WM_COMMAND, MAKEWPARAM(0, BN_CLICKED), hWndButton);
  {$IFNDEF USE_THREAD}
  Application.ProcessMessages;
  {$ENDIF}
end;

//
// Compare 2 Files (Using External Buffers and External Interruption)
//   Returns : 0=Identical Files, 1=Different Files, -20=Comparison Aborted, <0=Error
//
function  Compare2Files(Const FileName1: String; Const FileName2: String; Const FilesSize: TFileSize; Var FileNameErrorType: Integer): Integer;
var HFile1, HFile2: THandle;
var FilesRest, FileBufSize: TFileSize;
var i1: Integer;
begin
	// Nul Length Files
	Result := 0;
	if FilesSize=0 then Exit;
	// Data
  FilesRest := FilesSize;
  HFile1 := {$IFDEF USE_FPC_UTF8_FILEFUNC}FileOpenUTF8{$ELSE}FileOpen{$ENDIF}(FileName1, fmOpenRead or fmShareDenyWrite);
  if HFile1=THandle(-1) then
    begin
		  Result := -1;
      FileNameErrorType := 1;
    end
  else
		begin
      HFile2 := {$IFDEF USE_FPC_UTF8_FILEFUNC}FileOpenUTF8{$ELSE}FileOpen{$ENDIF}(FileName2, fmOpenRead or fmShareDenyWrite);
      if HFile2=THandle(-1) then
        begin
    		  Result := -1;
          FileNameErrorType := 2;
        end
			else
				begin
					FileBufSize := FILEBUF_SIZE1;
					while FilesRest>0 do
						begin
              {$IFDEF USE_THREAD}
              // If abort called during thread run
              if DoCompFilesThr.Terminated then
                begin
                  Result := -20;
                  Break;
                end;
              {$ENDIF}
              if FilesRest>FileBufSize then
                i1 := FileBufSize
              else
                i1 := FilesRest;
              Dec(FilesRest, i1);
              if i1>0 then
                begin
                  if FileRead(HFile1, gFBuffer1, i1) <> i1 then
										begin
											Result := -2;
                      FileNameErrorType := 1;
											Break;
										end;
                  if FileRead(HFile2, gFBuffer2, i1) <> i1 then
										begin
											Result := -2;
                      FileNameErrorType := 2;
											Break;
										end;
                  if not CompareMem(@gFBuffer1, @gFBuffer2, i1) then
                    begin
                      Result := 1;
                      Break;
                    end;
                end;
              FileBufSize := FILEBUF_SIZE2;
            end;
					FileClose(HFile2);
				end;
			FileClose(HFile1);
    end;
end;

//
//	Load Files List from Directory
//
function  LoadDir(Const DirName: String; Var FilesDir: Files_DirArr; Var NbrFilesDir: Integer): Boolean;
{$if Defined(FPC) and Defined(UNICODE)}
var SR: TUnicodeSearchRec;
{$else}
var SR: TSearchRec;
{$ifend}
var LenFilesDir: Integer;
begin
	Result := True;
	NbrFilesDir := 0;
  if {$IFDEF USE_FPC_UTF8_FILEFUNC}FindFirstUTF8{$ELSE}FindFirst{$ENDIF}(DirName + FILESDIR_MASK, FILESDIR_ATTR, SR)<>0 then Exit;
	SetLength(FilesDir, FILESDIR_INC);
	LenFilesDir := FILESDIR_INC;
	while True do
		begin
			if LenFilesDir=NbrFilesDir then
				if LenFilesDir + FILESDIR_INC<=FILESDIR_MAX then
					begin
						Inc(LenFilesDir, FILESDIR_INC);
        		SetLength(FilesDir, LenFilesDir);
					end
				else
					begin
						Result := False;
						Break;
					end;
    	FilesDir[NbrFilesDir].FileName := String(SR.Name);
    	FilesDir[NbrFilesDir].FileSize := SR.Size;
			Inc(NbrFilesDir);
			if {$IFDEF USE_FPC_UTF8_FILEFUNC}FindNextUTF8{$ELSE}FindNext{$ENDIF}(SR)<>0 then Break;
		end;
	SysUtils.FindClose(SR);
end;

//
//	Sort List of Files (File Length)
//
procedure SortDir(Var FilesDir: Files_DirArr; Const NbrFilesDir: Integer);
var i1: Integer;
begin
	for i1 := 0 to Pred(NbrFilesDir) do FilesDir[i1].Index := i1;
	SD_Sort(FilesDir, 0, Pred(NbrFilesDir));
end;
//	Quick Sort for Files
procedure SD_Sort(Var FilesDir: Files_DirArr; Const StartPos: Integer; Const EndPos: Integer);
var i1: Integer;
begin
	if StartPos<EndPos then
		begin
			i1 := SD_Partition(FilesDir, StartPos, EndPos);
			SD_Sort(FilesDir, StartPos, i1);
			SD_Sort(FilesDir, Succ(i1), EndPos);
		end;
end;
//	Partition for Quick Sort Files
function  SD_Partition(Var FilesDir: Files_DirArr; Const StartPos: Integer; Const EndPos: Integer): Integer;
var CentInd: Integer;			// Central Indice
var i1,i2,i3: Integer;
begin
	i1 := Pred(StartPos);
	i2 := Succ(EndPos);
	CentInd := StartPos;
	while True do
		begin
			repeat Dec(i2); until SD_PartComp(FilesDir, i2, CentInd)<>2;
			repeat Inc(i1); until SD_PartComp(FilesDir, i1, CentInd)<>1;
			if i1<i2 then
				begin			// Swap Values
					i3 := FilesDir[i1].Index;
					FilesDir[i1].Index := FilesDir[i2].Index;
					FilesDir[i2].Index := i3;
				end
			else
				Break;
		end;
	Result := i2;
end;
//	Compare Values (Return 0=Equal, 1=Lesser, 2=Greater)
function  SD_PartComp(Const FilesDir: Files_DirArr; Const Ind1: Integer; Const Ind2: Integer): Integer;
var i1, i2: Integer;
var fs1, fs2: TFileSize;
begin
	Result := 0;
	i1 := FilesDir[Ind1].Index;
	i2 := FilesDir[Ind2].Index;
  fs1 := FilesDir[i1].FileSize;
  fs2 := FilesDir[i2].FileSize;
	if fs1<fs2 then Result := 1
	  else if fs1>fs2 then Result := 2;
end;

end.
