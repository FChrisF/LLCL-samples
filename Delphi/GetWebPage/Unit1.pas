unit Unit1;

//
// HTTP Get/Post using Indy
//   (Indicate a valid search path for the LLCL files before compiling)
//
// This sample requires a recent version of Indy
//    (http://www.indyproject.org, or https://indy.fulgan.com/)
//
// For HTTPS URLs, OpenSSL is required for the execution (libeay32.dll, ssleay32.dll)
//
// For Lazarus/FPC, compression requires the zlib library (zlib1.dll)
//
// You can't use any visual components of Indy (only its classes)
//
//    (Program icon from "FatCow Farm Fresh Icons"
//        http://www.fatcow.com/free-icons)
//
// 1) This sample requires a 'special' version of LLCL. The
//      Classes.pas file coming with LLCL must be renamed
//      (to LClasses.pas, for instance), and all the other
//      units of LLCL must be modified to use the renamed file.
//      This is mandatory because Indy uses the standard
//      Classes.pas file
//
// 2) The SysUtils.pas and IniFiles.pas files used must be the
//      'standard' ones for Lazarus/FPC and Delphi (because of Indy):
//      so, delete or rename the ones coming from the LLCL units before
//      the compilation
//
// 3) The Variants.pas file used must be the 'standard' one for
//      Delphi: so, delete or rename the one coming from the LLCL
//      units before the compilation
//

// Copyright (c) 2015-2016 ChrisF
// Distributed under the terms of the MIT license: see LICENSE.txt

{$IFDEF FPC}
  {$mode objfpc}{$H+}
//  {$mode delphi}
{$ENDIF}

//------------------------------------------------------------------------------

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Dialogs,
  {$IFNDEF FPC} XPMan{$ELSE} LazUTF8{$ENDIF};

type

  { TForm1 }

  TForm1 = class(TForm)
{$IFNDEF FPC}
    XPManifest1: TXPManifest;
{$ENDIF}
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

procedure FormLog(Const SS: String);

//------------------------------------------------------------------------------

implementation

uses
  Windows,
  IdHTTP, IdURI, IdCookieManager, IdCompressorZLib, IdSSLOpenSSL, IdLogFile,
  ShUnit;

{$IFDEF FPC}
  {$R *.lfm}
{$ELSE}
  {$R *.dfm}
{$ENDIF}

type
  GetPostAction = (GPAGet, GPAPostData, GPAPostFile);

const
  // Current directory
  CUR_DIR = {$IFDEF MSWINDOWS}'.\'{$ELSE}'./'{$ENDIF};

  CUR_APP = 'GetWebPage';
  FILE_OUT = CUR_APP + '.html';
  FILE_LOG = CUR_APP + '.log';

  POSTBUFFER_SIZE = 65536;

procedure ControlsState(Const State: Integer; Const DoFocus: Boolean); forward;
procedure DoGetWebPage(Const GetOrPost: GetPostAction; Const StrData: String); forward;
function  GWP_SetCursor(Const ClassCursor: HCursor): HCursor; forward;
procedure GWP_StrData(Const StrData: String; Var TStrData: TStringList); forward;
procedure GWP_ParseURL(Const URL: String; Var IsHTTPS: Longbool; Var Host: String; Var SubURL: String); forward;

// Unicodestring definition for old versions of Delphi
{$IFNDEF FPC}
  {$if CompilerVersion<20}      // Before Delphi 2009
  type unicodestring = widestring;
  {$ifend}
{$ENDIF}

function  FindFileSize(Const Filename: String; Var LenFile: Int64): Boolean; forward;
function  BFileLoad(Const FileName: String; Const Posi: Longword; Const Len: Longword; Const Buffer: Pointer): Boolean; forward;

var
  MainTIdHTTP:  TIdHTTP;              // HTTP
  MainTIdCookieManager:   TIdCookieManager;   // Cookies
  MainTIdCompressorZlib:  TIdCompressorZLib;  // Compressor (ZLib)
  MainTIdSSLIOHandlerSocketOpenSSL: TIdSSLIOHandlerSocketOpenSSL;	// SSL
  HasNetComp:		Longbool = False;             // Has Net Components
  MainTIdLogFile:         TIdLogFile;         // Log File
  HasNetLogFile:          Longbool = False;   // Has Log File

  NetCall:      NetCall_Struct;       // All Net parameters and results

  PostBuffer:   array [0..Pred(POSTBUFFER_SIZE)] of Byte; // Buffer for Post Data
  PostBufferLen: Integer;

//------------------------------------------------------------------------------

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetShLogForm(@FormLog);
  // Initializes Network components
  HasNetComp:=ShInitNetSSLComp(@MainTIdHTTP,@MainTIdCookieManager,@MainTIdCompressorZlib,@MainTIdSSLIOHandlerSocketOpenSSL);
  if HasNetComp then    // OK
    begin
      MainTIdHTTP.HandleRedirects:=True;  // Allows redirection (HTTP Statut 302)
      MainTIdHTTP.RedirectMaximum:=10;
      ControlsState(001,False);
    end
  else                  // KO
    ControlsState(101,False);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  // End for Network components
  if HasNetComp then
    ShEndNetSSLComp(@MainTIdHTTP,@MainTIdCookieManager,@MainTIdCompressorZlib,@MainTIdSSLIOHandlerSocketOpenSSL);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  // Quit (End of program)
  Application.Terminate;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  // Gets Web page indicated by URL in Edit1
  DoGetWebPage(GPAGet,'');
end;

procedure TForm1.Button3Click(Sender: TObject);
var sData: String;
begin
  // Posts string data in Edit2 for Web page indicated by URL in Edit1
  //    Post parameters must be separated with a ';' (semicolon) character
  // Checks presence of string data
  sData:=Trim(Edit2.Text);
  if sData='' then
    begin
      Form1.Memo1.Lines.Clear;
      Form1.Memo1.Lines.Add(sLineBreak+' *** ERROR ***  No string data for Post');
      Exit;
    end;
  DoGetWebPage(GPAPostData,sData);
end;

procedure TForm1.Button4Click(Sender: TObject);
var FName: String;
var FSize: Int64;
begin
  // Posts file data for Web page indicated by URL in Edit1
  // Asks for the file
  OpenDialog1.Options:=OpenDialog1.Options+[ofPathMustExist, ofFileMustExist];
  OpenDialog1.Filter:='Data Files (*.dat)|*.dat|All Files (*.*)|*.*';
  // Exits, if no file selected
  if not OpenDialog1.Execute then
    Exit;
  // Checks file and size, then loads it
  FName:={$if Defined(FPC) and not Defined(UNICODE)}UTF8ToSys(OpenDialog1.FileName){$else}OpenDialog1.FileName{$ifend};
  if not FindFileSize(FName,FSize) then
    begin
      Form1.Memo1.Lines.Clear;
      Form1.Memo1.Lines.Add(sLineBreak+' *** ERROR ***  No or empty input file: '+FName);
      Exit;
    end;
  if FSize>SizeOf(PostBuffer) then
    begin
      Form1.Memo1.Lines.Clear;
      Form1.Memo1.Lines.Add(sLineBreak+' *** ERROR ***  Input file too big ('+IntToStr(SizeOf(PostBuffer))+' max)');
      Exit;
    end;
  if not BFileLoad(FName,0,FSize,@PostBuffer) then
    begin
      Form1.Memo1.Lines.Clear;
      Form1.Memo1.Lines.Add(sLineBreak+' *** ERROR ***  Can''t load input file: '+FName);
      Exit;
    end;
  PostBufferLen:=FSize;
  DoGetWebPage(GPAPostFile,'');
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  // Activate or DeActivate Indy Log File
  if CheckBox2.Checked then   // New Status
    HasNetLogFile:=ShAddConnectNetLogComp(@MainTIdHTTP,@MainTIdLogFile,CUR_DIR+FILE_LOG,True)
  else
    if HasNetLogFile then
      begin
        ShEndNetLogComp(@MainTIdHTTP,@MainTIdLogFile);
        HasNetLogFile:=False;
      end;
end;

//------------------------------------------------------------------------------

//
// Simulates a file log
//
procedure FormLog(Const SS: String);
begin
  Form1.Memo1.Lines.Add(SS);
end;

//
// Change controls states
//
procedure ControlsState(Const State: Integer; Const DoFocus: Boolean);
begin
  case State of
  001:      // All enabled
    begin
      Form1.Edit1.Enabled:=True;
      Form1.Edit2.Enabled:=True;
      Form1.Edit3.Enabled:=True;
      Form1.Button2.Enabled:=True;
      Form1.Button3.Enabled:=True;
      Form1.Button4.Enabled:=True;
      Form1.CheckBox1.Enabled:=True;
      Form1.CheckBox2.Enabled:=True;
      if DoFocus then Form1.Edit1.SetFocus;
    end;
  101:      // All disabled
    begin
      Form1.Edit1.Enabled:=False;
      Form1.Edit2.Enabled:=False;
      Form1.Edit3.Enabled:=False;
      Form1.Button2.Enabled:=False;
      Form1.Button3.Enabled:=False;
      Form1.Button4.Enabled:=False;
      Form1.CheckBox1.Enabled:=False;
      Form1.CheckBox2.Enabled:=False;
      if DoFocus then Form1.Button1.SetFocus;
    end;
  end;
end;

//
// Get Web page, using HTTP Get or Post
//
procedure DoGetWebPage(Const GetOrPost: GetPostAction; Const StrData: String);
var sURL,sHost,sSubURL,sError: string;
var IsHTTPS: Longbool;
var CallMethod: Integer;
var ErrorAbort: Longbool;
var ErrorType: Integer;
var CurrCursor,NewCursor: HCursor;
var TStrL: TStringList;
var TMemS: TMemoryStream;
var i1: Integer;
var s1: String;
begin
  sURL:=Trim(Form1.Edit1.Text);
  // Clears Displayed Cookies
  if Form1.CheckBox1.Checked then
    Form1.Edit3.Text:='';
  // Rejects Empty URL
  if sURL='' then
    begin
      Form1.Memo1.Lines.Clear;
      Form1.Memo1.Lines.Add(sLineBreak+' *** ERROR ***  No Web page URL');
      Exit;
    end;
  // Encodes and parses URL
  GWP_ParseURL(sURL,IsHTTPS,sHost,sSubURL);
  // (OpenSSLVersion may be absent for older versions of Indy)
  if IsHTTPS and (OpenSSLVersion='') then
    begin
      // OpenSSL not found within Indy
      Form1.Memo1.Lines.Clear;
      Form1.Memo1.Lines.Add(sLineBreak+' *** ERROR ***  HTTPS request but OpenSSL is missing');
      Exit;
    end;
  // Controls updates
  ControlsState(101,True);
  Form1.Memo1.Lines.Clear;
  Form1.Memo1.Lines.Add(sLineBreak+' Processing request...');
  NewCursor:=Windows.LoadCursor(0,IDC_WAIT);
  CurrCursor:=0;
  if NewCursor<>0 then
    CurrCursor:=GWP_SetCursor(NewCursor);
  Application.ProcessMessages;
  // Request preparation
  Fillchar(NetCall,SizeOf(NetCall),0);
  NetCall.Host:=sHost;
  NetCall.Referer:='';
  NetCall.ReadTimeOut:=6000;    // 6 seconds max. for the timeout
  NetCall.ClearCookies:=Form1.CheckBox1.Checked;
  NetCall.HTTPS:=IsHTTPS;
  if sSubURL='' then
    NetCall.URL:=sHost
  else
    NetCall.URL:=sHost+'/'+sSubURL;
  TStrL:=nil;
  TMemS:=nil;
  // Get or Post
  if GetOrPost=GPAGet then
    begin
      CallMethod:=0;
    end
  else
    begin
      CallMethod:=1;
      if GetOrPost=GPAPostData then
        begin
          NetCall.SendBinary:=False;
          // Fills string data
          TStrL:=TStringList.Create;
          NetCall.TStrL:=TStrL;
          GWP_StrData(StrData,TStrL);
        end
      else
        begin
          NetCall.SendBinary:=True;
          // Loads binary data
          ErrorType:=0;
          TMemS:=TMemoryStream.Create;
          NetCall.TBinaryData:=TMemS;
          try
            TMemS.Clear;
            TMemS.Write(PostBuffer,PostBufferLen);
            TMemS.Position:=0;
          except
            ErrorType:=1;
          end;
          if ErrorType<>0 then
            begin
              Form1.Memo1.Lines.Clear;
              Form1.Memo1.Lines.Add(sLineBreak+' *** ERROR ***  Can''t load binary data for Post');
              Exit;
            end;
        end;
    end;
  // Request
  ShPostGetHTMLPage(@MainTIdHTTP,@MainTIdCookieManager,CallMethod,NetCall,
                    FILE_OUT,CUR_DIR,'Web',ErrorType,ErrorAbort);
  // Clears Temporary Data
  if CallMethod=1 then
    begin
      if not NetCall.SendBinary then
        TStrL.Free
      else
        TMemS.Free;
    end;
  // Results
  Form1.Memo1.Lines.Clear;
  if ErrorType=0 then   //  OK
    begin
      Form1.Memo1.Lines.Add(sLineBreak+' Result OK.'+sLineBreak+sLineBreak
          +' Data saved in "'+FILE_OUT+'" file ('+IntToStr(NetCall.DataLength)+' bytes).');
    end
  else                  //  KO
    begin
      case ErrorType of
      12:   sError:='Socket Error'+sLineBreak+sLineBreak
              +'  Probably an unknown host in URL, or a possible Internet connection down';
      13:   sError:='Indy Error'+sLineBreak+sLineBreak
              +'  Probably an incorrect URL, or a possible timeout';
      else  sError:='Unknown Error';
      end;
      Form1.Memo1.Lines.Add(sLineBreak+' Error Type '+IntToStr(ErrorType)+' :   '+sError);
    end;
  // Displays Cookies
  s1:='';
  for i1:=0 to Pred(MainTIdCookieManager.CookieCollection.Count) do
    begin
      if i1>0 then s1:=s1+'~';
      s1:=s1+MainTIdCookieManager.CookieCollection.Cookies[i1].ClientCookie;
    end;
  Form1.Edit3.Text:=s1;
  // End
  ControlsState(001,True);
  if CurrCursor<>0 then
    GWP_SetCursor(CurrCursor);
end;
// Encodes and parses URL
procedure GWP_ParseURL(Const URL: String; Var IsHTTPS: Longbool; Var Host: String; Var SubURL: String);
const
    CHTTP   = 'HTTP://';
    CHTTPS  = 'HTTPS://';
var s1: String;
var i1: Integer;
begin
  s1:=Trim(URL);
  IsHTTPS:=False;
  // TIdURI needs a protocol
  if (Copy(UpperCase(s1),1,7)<>CHTTP) and (Copy(UpperCase(s1),1,8)<>CHTTPS) then
    s1:=CHTTP+s1;
  try
    s1:=TIdURI.URLEncode(s1);
  except
  end;
  if Copy(UpperCase(s1),1,7)=CHTTP then
    s1:=Copy(s1,8,Length(s1)-7)
  else
    if Copy(UpperCase(s1),1,8)=CHTTPS then
      begin
        s1:=Copy(s1,9,Length(s1)-8);
        IsHTTPS:=True;
      end;
  i1:=Pos('/',s1);
  if i1>1 then
    begin
      Host:=Copy(s1,1,i1-1);
      SubURL:=Copy(s1,i1+1,Length(s1)-i1+1);
    end
  else
    begin
      Host:=s1;
      SubURL:='';
    end;
end;
// Parses String Data
procedure GWP_StrData(Const StrData: String; Var TStrData: TStringList);
var i1: Integer;
var s1: String;
begin
  s1:=StrData;
  i1:=Pos(';',s1);
  while i1>0 do
    begin
      TStrData.Add(Trim(Copy(s1,1,i1-1)));
      s1:=Trim(Copy(s1,i1+1,Length(s1)-i1));
      i1:=Pos(';',s1);
    end;
  if Length(s1)>0 then
    TStrData.Add(Trim(s1));
end;
// Sets Cursor
function  GWP_SetCursor(Const ClassCursor: HCursor): HCursor;
begin
  Windows.SetClassLong(Form1.Handle,GCL_HCURSOR,ClassCursor);
  Windows.SetClassLong(Form1.Button1.Handle,GCL_HCURSOR,ClassCursor);
  Windows.SetClassLong(Form1.Memo1.Handle,GCL_HCURSOR,ClassCursor);
  result:=Windows.SetCursor(ClassCursor);
end;

//------------------------------------------------------------------------------

//
// Find file and its size
//
function  FindFileSize(Const Filename: String; Var LenFile: Int64): Boolean;
{$if Defined(FPC) and Defined(UNICODE)}
var SR: TUnicodeSearchRec;
{$else}
var SR: TSearchRec;
{$ifend}
begin
  Result:=False;
  if SysUtils.FindFirst(FileName,faAnyFile,SR)<>0 then
    exit;
  LenFile:=SR.Size;
  SysUtils.FindClose(SR);
  Result:=True;
end;

//
// Loads (Part of) a Binary File in Buffer
//
function  BFileLoad(Const FileName: String; Const Posi: Longword; Const Len: Longword; Const Buffer: Pointer): Boolean;
var HFile: File;
var i1: Longword;
begin
  Result:=False;
  AssignFile(HFile,FileName);
  FileMode:=fmOpenRead;
  IOResult();
  Reset(HFile,1);
  if IOResult()=0 then
    begin
      Seek(HFile,Posi);
      if IOResult()=0 then
        begin
          BlockRead(HFile,Buffer^,Len,i1);
          if ((IOResult()=0) and (i1=Len)) then Result:=True;
        end;
    end;
  IOResult();
  CloseFile(HFile);
  Result:=Result and (IOResult()=0);
end;

end.

