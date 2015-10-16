unit UnitNoTI1;

//
// Periodic ping for a given URL
//   (Indicate a valid search path for the LLCL files before compiling)
//
//   Notes: - LLCL_OPT_TOPFORM must be defined for this sample
//          - can be compiled with the standard VCL only for Delphi 2006+
//

// Copyright (c) 2015 ChrisF
// Distributed under the terms of the MIT license: see LICENSE.txt

{$IFDEF FPC}
  {$mode objfpc}{$H+}
//  {$mode delphi}
{$ENDIF}
{$IFDEF FPC_OBJFPC} {$DEFINE IS_FPC_OBJFPC_MODE} {$ENDIF}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Menus,
  Windows, {$IFNDEF FPC}Messages, XPMan{$ELSE}LMessages{$ENDIF};

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
    Edit1: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    Timer1: TTimer;
    PopupMenu1: TPopupMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
//    TrayIcon1: TTrayIcon;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure TrayIcon1DblClick(Sender: TObject);
  private
    { private declarations }
    TrayIcon1: TTrayIcon;
    WindowStateSave: TWindowState;
    procedure AppMinimize(Sender: TObject);
    procedure AppRestore(Sender: TObject);
  public
    { public declarations }
  end;

var
  Form1: TForm1;

//------------------------------------------------------------------------------

implementation

uses
  Winsock;

{$IFDEF FPC}
  {$R *.lfm}
  {$R PePingRes.rc}
{$ELSE}
  {$R *.dfm}
  {$R PePingRes.res PePingRes.rc}
{$ENDIF}

const
  // Current directory
  CUR_DIR = {$IFDEF MSWINDOWS}'.\'{$ELSE}'./'{$ENDIF};

type
  // TrayIcon states
  TATI_STATES = (ATIS_Query, ATIS_OK, ATIS_KO);

procedure ReadIniFile(); forward;
function  RIF_ReadString(Const FileName: String; Const AppName: String;
           Const KeyName: String; Const DefaultStr: String): String; forward;
procedure CheckDataAndStart(); forward;
procedure TrayIconState(Const NewState: TATI_STATES); forward;

function  StrToIPAddr(Const Host: String; Var HostIPAddr: TInAddr; Var ErrMess: string): Boolean; forward;
function  PingCall(Const Host: String; Var MessRet: String): Boolean; forward;

var
  CurHostURL:     String;     // Current Host URL used
  TimerInt:       Integer;    // Timer Interval (ms)

//------------------------------------------------------------------------------

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Specific for old versions of Delphi (no TTrayIcon control)
{$IFNDEF FPC}
  {$if (CompilerVersion < 18)}   // Before Delphi 2006
  TrayIcon1:=TTrayIcon.Create(self);
  TrayIcon1.PopUpMenu:=PopupMenu1;
  TrayIcon1.OnDblClick:=Form1.TrayIcon1DblClick;
  {$ifend}
{$ENDIF}
  // Application is started in SysTray mode by default
  //    (Because Main Form is not first visible, and TrayIcon yes)
  TrayIconState(ATIS_Query);
  TrayIcon1.Visible:=True;
  // Specific OnMinimize and OnRestore events
  Application.OnMinimize := {$IFDEF IS_FPC_OBJFPC_MODE}@{$ENDIF}AppMinimize;
  Application.OnRestore := {$IFDEF IS_FPC_OBJFPC_MODE}@{$ENDIF}AppRestore;
  // Reads data from .ini file
  ReadIniFile();
  // Checks/Displays them, and calls for a Ping if OK
  CheckDataAndStart();
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  // Specific for old versions of Delphi (no TTrayIcon control)
{$IFNDEF FPC}
  {$if (CompilerVersion < 18)}   // Before Delphi 2006
  TrayIcon1.Free;
  {$ifend}
{$ENDIF}
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  // Saves current window state, to be able to restore it later
  // (Case wsNormal->wsMaximized - OnPaint after OnResize, so it's OK)
  WindowStateSave:=WindowState;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  // Saves current window state, to be able to restore it later
  // (Not OK for wsNormal->wsMaximized - See FormPaint)
  WindowStateSave:=WindowState;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  // Quit (End of program)
  Application.Terminate;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  // OK (Returns in SysTray mode)
  // (Button2 = Default and Cancel Button  - Return and Escape keys)
  Application.Minimize;
end;

procedure TForm1.Button3Click(Sender: TObject);
var s1: String;
begin
  // Affects New (New Host URL, if non empty)
  s1:=Trim(Edit1.Text);
  // Rejected if empty
  if s1='' then
    StaticText2.Caption:=sLineBreak+' Error: Empty New Host.'
  else
    begin
      // New Host URL - Calls for a Ping
      //    (.Ini file is not updated)
      CurHostURL:=s1;
      CheckDataAndStart();
    end;
end;

procedure TForm1.Button4Click(Sender: TObject);
var StrMess: String;
begin
  // ReTest (Immediate call for a Ping)
  //   Also used internally when Form is just loaded, and on Timer falls
  TrayIconState(ATIS_Query);
  StaticText2.Caption:=sLineBreak+' Testing...';
  // Ping
  if PingCall(CurHostURL,StrMess) then
    TrayIconState(ATIS_OK)
  else
    TrayIconState(ATIS_KO);
  StaticText2.Caption:=sLineBreak+' '+StrMess;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  // Timer fall (Simulates click for PushButton 4 - Could also call PushButton 4 click event directly)
  PostMessage(Form1.Handle,WM_COMMAND,MAKEWPARAM(0,BN_CLICKED),Form1.Button4.Handle);
end;

procedure TForm1.MenuItem1Click(Sender: TObject);
begin
  // Show (Restores Form from PopupMenu in SysTray, and Hides TrayIcon)
  Application.Restore;
  Application.BringToFront;
end;

procedure TForm1.MenuItem2Click(Sender: TObject);
begin
  // Ping = ReTest (Immediate call for a Ping)
  //    (Calls directly click for PushButton 4)
  Button4Click(self);
end;

procedure TForm1.MenuItem3Click(Sender: TObject);
begin
  // Quit (End of program from PopupMenu in SysTray)
  //    (Calls directly click for PushButton 1)
  Button1Click(self);
end;

procedure TForm1.TrayIcon1DblClick(Sender: TObject);
begin
  // DoubleClick on TrayIcon = Show Form
  //    (Calls directly click for MenuItem 1)
  MenuItem1Click(Self);
end;

procedure TForm1.AppMinimize(Sender: TObject);
begin
  // Simulates Minimize to Systray
  Visible:=False;               // (After minimizing)
  TrayIcon1.Visible:=True;
end;

procedure TForm1.AppRestore(Sender: TObject);
begin
  TrayIcon1.Visible:=False;
  WindowState:=WindowStateSave;     // (Standard LCL needs it)
  Visible:=True;
end;

//------------------------------------------------------------------------------

//
// Reads Host URL and Timer Interval from .INI file
//
procedure ReadIniFile();
const
    INI_FILE        = CUR_DIR+'PePing.ini';
    INI_OPTIONS     = 'Options';
    INI_OURL        = 'URL';
    INI_OURLDEF     = 'google.com';
    INI_OTIMEINT    = 'Timer';
    INI_OTIMEINTDEF = '120';
var i1,i2: Integer;
var s1: String;
begin
  // Host URL
  CurHostURL:=RIF_ReadString(INI_FILE,INI_OPTIONS,INI_OURL,INI_OURLDEF);
  // Timer Interval
  s1:=RIF_ReadString(INI_FILE,INI_OPTIONS,INI_OTIMEINT,INI_OTIMEINTDEF);
  Val(s1,i1,i2); if i2<>0 then Val(INI_OTIMEINTDEF,i1,i2);
  TimerInt:=i1*1000;
end;
// Reads a string from an .ini file
function RIF_ReadString(Const FileName: String; Const AppName: String;
          Const KeyName: String; Const DefaultStr: String): String;
var a1: array [0..Pred(1024)] of Char;
begin
  a1[0]:=#00;   // Avoid compilation warnings
  {$IFDEF UNICODE}GetPrivateProfileStringW{$ELSE}GetPrivateProfileStringA{$ENDIF}
    (PChar(AppName),PChar(KeyName),PChar(DefaultStr),a1,SizeOf(a1),PChar(FileName));
  result:=String(a1);
end;

//
// Checks Host URL (present) and Timer Interval (present and <>0)
//    Displays them inside Form
//    And calls for an immediate Ping test, if both OK
//
procedure CheckDataAndStart();
var i1: Integer;
var s1: String;
begin
  i1:=TimerInt;
  s1:=CurHostURL;
  // Timer Interval <> 0 ?
  if i1=0 then
    begin
      Form1.StaticText2.Caption:=sLineBreak+' Timer not started.';
      Form1.Timer1.Enabled:=False;
    end
  else
    begin
      Form1.Timer1.Interval:=i1;
      Form1.Timer1.Enabled:=False;
    end;
  // Host URL present ?
  if s1='' then
    begin
      Form1.StaticText1.Caption:=s1;
      Form1.StaticText2.Caption:=sLineBreak+' No Host: Timer not started.';
      Form1.Timer1.Enabled:=False;
    end
  else
    begin
      Form1.StaticText1.Caption:=' '+s1+'   ('+IntToStr(i1 div 1000)+' s)';
      Form1.Timer1.Enabled:=True;
    end;
  // If OK, immediate Ping test
  if Form1.Timer1.Enabled then
    // Simulates click for PushButton 4
    PostMessage(Form1.Handle,WM_COMMAND,MAKEWPARAM(0,BN_CLICKED),Form1.Button4.Handle);
end;

//
// Sets new TrayIcon State
//
procedure TrayIconState(Const NewState: TATI_STATES);
begin
  // Modifies SysTray Hint and Icon, according to the new state
  case NewState  of
  ATIS_Query:
    begin
      Form1.TrayIcon1.Hint:=Form1.Caption+' - Testing...';
      // (Can't use Application.Icon.Handle MAINICON because
      //   previous handle are always freed in standard LCL)
      Form1.TrayIcon1.Icon.Handle:=Windows.LoadIcon(HInstance,'ZIPePingMI');
    end;
  ATIS_OK:
    begin
      Form1.TrayIcon1.Hint:=Form1.Caption+' - OK';
      Form1.TrayIcon1.Icon.Handle:=Windows.LoadIcon(HInstance,'ZIPePingOK');
    end;
  ATIS_KO:
    begin
      Form1.TrayIcon1.Hint:=Form1.Caption+' - KO';
      Form1.TrayIcon1.Icon.Handle:=Windows.LoadIcon(HInstance,'ZIPePingKO');
    end;
  end;
end;

//------------------------------------------------------------------------------

//
// DNS resolution : gets IP Address from Host URL
//    (Uses synchronous call for simplification, and for IPv4 only)
//
function StrToIPAddr(Const Host: String; Var HostIPAddr: TInAddr; Var ErrMess: String): Boolean;
var WSAData: TWSAData;
var ptrHostEnt: PHostEnt;
var ptrAddrList: PAnsiChar; //
var AnsiHost: AnsiString;   // GetHostByName can't process Unicode strings
begin
  result:=False;
  ErrMess:='';
  // Initiates for Winsock DLL use
  if WSAStartUp($0101, WSAData)<>0 then
    begin
      ErrMess:='Error: WSAStartUp during DNS resolution';
      Exit;
    end;
  // Host string conversion for domain name:
  //   - OK if only ASCII characters present
  //   - use Punycode/IDNA string for other cases
  {$IFDEF UNICODE}
  AnsiHost:=AnsiString(Host);
  {$ELSE}
  AnsiHost:=Host;     // (ASCII - No UTF8 conversion for FPC/Lazarus)
  {$ENDIF}
  // Calls for DNS resolution (IPv4 only, synchronous)
  //    GetHostByName is deprecated in Windows
  //    Should use GetAddrInfo/GetAddrInfoW for IPv6 too
  ptrHostEnt:=GetHostByName(PAnsiChar(AnsiHost));
  // Various errors are possible in fact (should call WSAGetLastError),
  //    but most probably because the Host URL is incorrect/unknown
  if not assigned(ptrHostEnt) then
    ErrMess:='Error: Host unknown (GetHostByName during DNS resolution)'
  else
    begin
      ptrAddrList:=ptrHostEnt^.h_addr^;
      // Expects at least one IP Address
      //    (Could also check if h_length=4, for sanity)
      if not assigned(ptrAddrList) then
        ErrMess:='Error: Address List during DNS resolution'
      else
        begin
          Move(ptrAddrList^,HostIPAddr.S_un_b,4);
          result:=True;
        end;
    end;
  // End of Winsock DLL use
  WSACleanUp();
end;

//
// Uses Icmp functions for simplification
//    (IPv4 only, 'Icmp.dll' -> 'Iphlpapi.dll', synchronous call)
//
function IcmpCreateFile(): THandle; stdcall; external 'Icmp.dll';
function IcmpCloseHandle(IcmpHandle: THandle): Longbool; stdcall; external 'Icmp.dll';
function IcmpSendEcho(IcmpHandle: THandle; DestinationAddress: TInAddr;
    RequestData: Pointer; RequestSize: word;
    RequestOptions: Pointer; ReplyBuffer: Pointer;
    ReplySize: Longword; Timeout: Longword) : Longword; stdcall; external 'Icmp.dll';

//
// DNS resolution for Host URL,
//    And Pings it if OK
//
function PingCall(Const Host: String; Var MessRet: String): Boolean;
var IPAddr: TInAddr;
var IHandle: THandle;
var ReplyBuffer: array[0..Pred(128)] of Byte; // At least SizeOf(ICMP_ECHO_REPLY)
var NbrEchos: Integer;
begin
  result:=False;
  // Aborts, if DNS resolution is not OK
  if not StrToIPAddr(Host, IPAddr, MessRet) then Exit;
  // IP Adress from DNS resolution
  MessRet:=' IP Address: '+IntToStr(Byte(IPAddr.S_un_b.s_b1))+'.'
            +IntToStr(Byte(IPAddr.S_un_b.s_b2))+'.'
            +IntToStr(Byte(IPAddr.S_un_b.s_b3))+'.'
            +IntToStr(Byte(IPAddr.S_un_b.s_b4));
  IHandle:=IcmpCreateFile();
  // Aborts, if can't open a handle for Icmp functions
  if IHandle=0 then
    begin
      MessRet:=MessRet+sLineBreak+' Error: IcmpCreateFile during Ping';
      Exit;
    end;
  // IPv4 only, synchronous (4s before timeout)
  NbrEchos:=IcmpSendEcho(IHandle,IPAddr,nil,0,nil,
            @ReplyBuffer,SizeOf(ReplyBuffer),4000);
  // Expects at least one echo (i.e. answer)
  if NbrEchos=0 then
    MessRet:=MessRet+'   --   ERROR Ping'
  else
    begin
      MessRet:=MessRet+'   --   Ping OK';
      result:=True;
    end;
  // Finally, closes Icmp handle
  IcmpCloseHandle(IHandle);
end;

end.

