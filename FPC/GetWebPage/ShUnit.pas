unit ShUnit;

{ Share Unit   }

// Copyright (c) 2015 ChrisF
// Distributed under the terms of the MIT license: see LICENSE.txt

{$IFDEF FPC}
  {$mode objfpc}{$H+}
//  {$mode delphi}
{$ENDIF}

//------------------------------------------------------------------------------

interface

uses
  Classes, SysUtils,      // Uses standard versions (i.e. VCL/LCL)
  // Following Indy units are also using standard versions of Classes and SysUtils
  IdHTTP, IdCookieManager, IdCompressorZLib, IdException, IdStack,
  IdSSLOpenSSL, IdLogFile;

type pTIdHTTP = ^TIdHTTP;
type pTIdCookieManager = ^TIdCookieManager;
type pTIdCompressorZLib = ^TIdCompressorZLib;
type pTIdSSLIOHandlerSocketOpenSSL = ^TIdSSLIOHandlerSocketOpenSSL;
type pTIdLogFile = ^TIdLogFile;

type NetCall_Struct = record
  URL:              String;
  Host:             String;
  Referer:          String;
  ReadTimeOut:      Integer;
  ClearCookies:     Longbool;
  TFS:              Classes.TFileStream;
  TStrL:            TStringList;
  HTTPS:            Longbool;
  SendBinary:       Longbool;
  TBinaryData:      Classes.TMemoryStream;
  //
  Result:           Integer;
  ErrorAbort:       Longbool;
  ErrorClass:       String;
  ErrorMessage:     String;
  LastError:        Integer;
  DataLength:       Integer;
end;

type
 TShFormLog = procedure (Const SS: String);

procedure SetShLogForm(const AFormLog: TShFormLog);
procedure ShLogText(Const TextType: Integer; Const SS: String);

function  ShInitNetComp(Const ptrTIdHTTP: pTIdHTTP; Const ptrTIdCookieManager: pTIdCookieManager; Const ptrTIdCompressorZLib: pTIdCompressorZLib): Longbool;
function  ShInitNetSSLComp(Const ptrTIdHTTP: pTIdHTTP; Const ptrTIdCookieManager: pTIdCookieManager; Const ptrTIdCompressorZLib: pTIdCompressorZLib; Const ptrTIdSSLIOHandlerSocketOpenSSL: pTIdSSLIOHandlerSocketOpenSSL): Longbool;
function  ShAddConnectNetLogComp(Const ptrTIdHTTP: pTIdHTTP; Const ptrTIdLogFile: pTIdLogFile; Const LogFileName: String; Const ActivateLogFile: Longbool): Longbool;
procedure ShEndNetComp(Const ptrTIdHTTP: pTIdHTTP; Const ptrTIdCookieManager: pTIdCookieManager; Const ptrTIdCompressorZLib: pTIdCompressorZLib);
procedure ShEndNetSSLComp(Const ptrTIdHTTP: pTIdHTTP; Const ptrTIdCookieManager: pTIdCookieManager; Const ptrTIdCompressorZLib: pTIdCompressorZLib; Const ptrTIdSSLIOHandlerSocketOpenSSL: pTIdSSLIOHandlerSocketOpenSSL);
procedure ShEndNetLogComp(Const ptrTIdHTTP: pTIdHTTP; Const ptrTIdLogFile: pTIdLogFile);
procedure ShActiveNetLogComp(Const ptrTIdLogFile: pTIdLogFile; Const ActivateLogFile: Longbool);
procedure ShNetHTMLGet(Var NetCall: NetCall_Struct; Const IdHTTP: TIdHTTP; Const IdCookieManager: TIdCookieManager);
procedure ShNetHTMLPost(Var NetCall: NetCall_Struct; Const IdHTTP: TIdHTTP; Const IdCookieManager: TIdCookieManager);
procedure ShPostGetHTMLPage(Const ptrTIdHTTP: pTIdHTTP; Const ptrTIdCookieManager: pTIdCookieManager; Const Mode: Integer; Var NetCall: NetCall_Struct; Const FileName: String; Const DirName: String; Const LogTxt: String; Var ErrorType: Integer; Var ErrorAbort: Longbool);

//------------------------------------------------------------------------------

implementation

procedure ShEndNetCompExt(Const ptrTIdHTTP: pTIdHTTP; Const HasHTTP: Longbool; Const ptrTIdCookieManager: pTIdCookieManager; Const HasCookie: Longbool; Const ptrTIdCompressorZLib: pTIdCompressorZLib; Const HasCompress: Longbool); forward;
procedure ShEndNetSSLCompExt(Const ptrTIdHTTP: pTIdHTTP; Const HasHTTP: Longbool; Const ptrTIdCookieManager: pTIdCookieManager; Const HasCookie: Longbool; Const ptrTIdCompressorZLib: pTIdCompressorZLib; Const HasCompress: Longbool; Const ptrTIdSSLIOHandlerSocketOpenSSL: pTIdSSLIOHandlerSocketOpenSSL; Const HasSSL: Longbool); forward;

var ShFormLog: TShFormLog;

//------------------------------------------------------------------------------

//
// Set Internal Log Procedure
//
procedure SetShLogForm(const AFormLog: TShFormLog);
begin
  ShFormLog:=AFormLog;
end;

//
// Internal Log
//
procedure ShLogText(Const TextType: Integer; Const SS: String);
var s1: String;
begin
  case TextType of
  1:    s1:=' *** ERROR *** ';    // Error
  2:    s1:=' Result: ';          // Result
  else  s1:=' ';                  // Others
  end;
  ShFormLog(s1+SS);
end;

//
// Network Components Initialization
//
function  ShInitNetComp(Const ptrTIdHTTP: pTIdHTTP; Const ptrTIdCookieManager: pTIdCookieManager; Const ptrTIdCompressorZLib: pTIdCompressorZLib): Longbool;
var HasHTTP, HasCookie, HasCompress: Longbool;
var l1: Longbool;
begin
  HasHTTP:=False;
  HasCookie:=False;
  HasCompress:=False;
  l1:=False;
  try
    ptrTIdHTTP^:=TIdHTTP.Create(Nil);
  except
    l1:=True;
  end;
  if l1 then
    ShLogText(1,'Error when Creating HTTP Component - Abort')
  else
    HasHTTP:=True;
  l1:=False;
  try
    ptrTIdCookieManager^ :=TIdCookieManager.Create(Nil);
  except
    l1:=True;
  end;
  if l1 then
    ShLogText(1,'Error when Creating Cookie Component - Abort')
  else
    HasCookie:=True;
  l1:=False;
  try
    ptrTIdCompressorZLib^:=TIdCompressorZLib.Create(Nil);
  except
    l1:=True;
  end;
  if l1 then
    ShLogText(1,'Error when Creating Compressor Component - Abort')
  else
    HasCompress:=True;
  if HasHTTP and HasCookie and HasCompress then
    begin
      ptrTIdHTTP^.AllowCookies:=True;
      ptrTIdHTTP^.CookieManager:=ptrTIdCookieManager^;
      ptrTIdHTTP^.Compressor:=ptrTIdCompressorZLib^;
      Result:=True;
    end
  else
    begin
      ShEndNetCompExt(ptrTIdHTTP,HasHTTP,ptrTIdCookieManager,HasCookie,ptrTIdCompressorZLib,HasCompress);
      Result:=False;
    end;
end;

//
// Network Components Initialization With SSL Encryption
//
function  ShInitNetSSLComp(Const ptrTIdHTTP: pTIdHTTP; Const ptrTIdCookieManager: pTIdCookieManager; Const ptrTIdCompressorZLib: pTIdCompressorZLib; Const ptrTIdSSLIOHandlerSocketOpenSSL: pTIdSSLIOHandlerSocketOpenSSL): Longbool;
var HasSSL: Longbool;
var l1: Longbool;
begin
  if not ShInitNetComp(ptrTIdHTTP,ptrTIdCookieManager,ptrTIdCompressorZLib) then
    Result:=False
  else
    begin
      HasSSL:=False;
      l1:=False;
      try
        ptrTIdSSLIOHandlerSocketOpenSSL^:=TIdSSLIOHandlerSocketOpenSSL.Create(Nil);
      except
        l1:=True;
      end;
      if l1 then
        ShLogText(1,'Error when Creating SSL Component - Abort')
      else
        HasSSL:=True;
      if HasSSL then
        begin
          ptrTIdHTTP^.IOHandler:=ptrTIdSSLIOHandlerSocketOpenSSL^;
          Result:=True;
        end
      else
        begin
          ShEndNetSSLCompExt(ptrTIdHTTP,True,ptrTIdCookieManager,True,ptrTIdCompressorZLib,True,ptrTIdSSLIOHandlerSocketOpenSSL,HasSSL);
          Result:=False;
        end;
    end;
end;

//
//	Add, Connect and Eventually Activate Network LogFile Component
//
function  ShAddConnectNetLogComp(Const ptrTIdHTTP: pTIdHTTP; Const ptrTIdLogFile: pTIdLogFile; Const LogFileName: String; Const ActivateLogFile: Longbool): Longbool;
var l1: Longbool;
begin
	l1:=False;
	try
		ptrTIdLogFile^:=TIdLogFile.Create(Nil);
	except
		l1:=True;
	end;
	if l1 then
		begin
			ShLogText(1,'Error when Creating LogFile Component (Not Aborted)');
			Result:=False;
		end
	else
		begin
			ptrTIdLogFile^.Filename:=LogFileName;
			ptrTIdHTTP^.Intercept:=ptrTIdLogFile^;
			ShActiveNetLogComp(ptrTIdLogFile,ActivateLogFile);
			Result:=True;
		end;
end;

//
// Network Components End
//
procedure ShEndNetComp(Const ptrTIdHTTP: pTIdHTTP; Const ptrTIdCookieManager: pTIdCookieManager; Const ptrTIdCompressorZLib: pTIdCompressorZLib);
begin
  ShEndNetCompExt(ptrTIdHTTP,True,ptrTIdCookieManager,True,ptrTIdCompressorZLib,True);
end;
// Extended Version
procedure ShEndNetCompExt(Const ptrTIdHTTP: pTIdHTTP; Const HasHTTP: Longbool; Const ptrTIdCookieManager: pTIdCookieManager; Const HasCookie: Longbool; Const ptrTIdCompressorZLib: pTIdCompressorZLib; Const HasCompress: Longbool);
var l1: Longbool;
begin
  l1:=False;
  if HasCookie then
    begin
      ptrTIdHTTP^.AllowCookies:=False;
      ptrTIdHTTP^.CookieManager:=Nil;
      try
        ptrTIdCookieManager^.Free;
      except
        l1:=True;
      end;
    end;
  if l1 then
    ShLogText(1,'Error when Releasing Cookie Component');
  l1:=False;
  if HasCompress then
    begin
      ptrTIdHTTP^.Compressor:=Nil;
      try
        ptrTIdCompressorZLib^.Free;
      except
        l1:=True;
      end;
    end;
  if l1 then
    ShLogText(1,'Error when Releasing Compressor Component');
  if HasHTTP then
    try
      ptrTIdHTTP^.Free;
    except
      l1:=True;
    end;
  if l1 then
    ShLogText(1,'Error when Releasing HTTP Component');
end;

//
// Network Components End With SSL Encryption
//
procedure ShEndNetSSLComp(Const ptrTIdHTTP: pTIdHTTP; Const ptrTIdCookieManager: pTIdCookieManager; Const ptrTIdCompressorZLib: pTIdCompressorZLib; Const ptrTIdSSLIOHandlerSocketOpenSSL: pTIdSSLIOHandlerSocketOpenSSL);
begin
  ShEndNetSSLCompExt(ptrTIdHTTP,True,ptrTIdCookieManager,True,ptrTIdCompressorZLib,True,ptrTIdSSLIOHandlerSocketOpenSSL,True);
end;
// Extended Version
procedure ShEndNetSSLCompExt(Const ptrTIdHTTP: pTIdHTTP; Const HasHTTP: Longbool; Const ptrTIdCookieManager: pTIdCookieManager; Const HasCookie: Longbool; Const ptrTIdCompressorZLib: pTIdCompressorZLib; Const HasCompress: Longbool; Const ptrTIdSSLIOHandlerSocketOpenSSL: pTIdSSLIOHandlerSocketOpenSSL; Const HasSSL: Longbool);
var l1: Longbool;
begin
  l1:=False;
  if HasSSL then
    begin
      ptrTIdHTTP^.IOHandler:=Nil;
      try
        ptrTIdSSLIOHandlerSocketOpenSSL^.Free;
      except
        l1:=True;
      end;
    end;
  if l1 then
    ShLogText(1,'Error when Releasing SSL Component');
  ShEndNetCompExt(ptrTIdHTTP,True,ptrTIdCookieManager,True,ptrTIdCompressorZLib,True);
end;

//
//	Network LogFile Component End
//
procedure ShEndNetLogComp(Const ptrTIdHTTP: pTIdHTTP; Const ptrTIdLogFile: pTIdLogFile);
var l1: Longbool;
begin
	l1:=False;
	ShActiveNetLogComp(ptrTIdLogFile,False);
	ptrTIdHTTP^.Intercept:=Nil;
	try
		ptrTIdLogFile^.Free;
	except
		l1:=True;
	end;
	if l1 then
		ShLogText(1,'Error when Releasing LogFile Component');
end;

//
//	Activate/Deactivate Network LogFile Component
//
procedure ShActiveNetLogComp(Const ptrTIdLogFile: pTIdLogFile; Const ActivateLogFile: Longbool);
begin
	ptrTIdLogFile^.Active:=ActivateLogFile;
end;

//
// Network HTML Get
//
procedure ShNetHTMLGet(Var NetCall: NetCall_Struct; Const IdHTTP: TIdHTTP; Const IdCookieManager: TIdCookieManager);
var ProtHead: String;
begin
  IdHTTP.Request.Host:=NetCall.Host;
  IdHTTP.Request.Referer:=NetCall.Referer;
  IdHTTP.ReadTimeOut:=NetCall.ReadTimeOut;
  IdHTTP.Request.ContentType:=EmptyStr;
  IdHTTP.Request.ContentLength:=0;
  IdHTTP.Request.UserAgent:='Mozilla/5.0 (Windows NT 5.1; rv:41.0) Gecko/20100101 Firefox/40.0';    // Some web sites don't like the old by-default one (i.e. malware);
  if NetCall.ClearCookies then IdCookieManager.CookieCollection.Clear;
  NetCall.Result:=0;
  NetCall.ErrorAbort:=False;
  NetCall.LastError:=0;
  if NetCall.HTTPS then ProtHead:='https://' else ProtHead:='http://';
  try
    IdHTTP.Get(ProtHead+NetCall.URL,NetCall.TFS);
  except
    on E: EIdSocketError do         // Socket Error
      begin
        NetCall.Result:=1;
        NetCall.ErrorClass:=E.ClassName;
        NetCall.ErrorMessage:=E.Message;
        NetCall.LastError:=E.LastError;
      end;
    on E: EIdException do           // Other Indy Errors (HTTP Error)
      begin
        NetCall.Result:=2;
        NetCall.ErrorClass:=E.ClassName;
        NetCall.ErrorMessage:=E.Message;
        NetCall.LastError:=IdHTTP.ResponseCode;
      end;
    on E: Exception do              // Other Errors
      begin
        NetCall.Result:=3;
        NetCall.ErrorClass:=E.ClassName;
        NetCall.ErrorMessage:=E.Message;
        NetCall.LastError:=-1;
      end;
    else                            // Sanity
      begin
        NetCall.Result:=4;
        NetCall.ErrorClass:='?';
        NetCall.ErrorMessage:='???';
        NetCall.LastError:=-1;
      end;
  end;
  try
    IdHTTP.IOHandler.Close;
  except;
  end;
  try
    IdHTTP.IOHandler.InputBuffer.Clear;
  except;
  end;
end;

//
// Network HTML Post
//
procedure ShNetHTMLPost(Var NetCall: NetCall_Struct; Const IdHTTP: TIdHTTP; Const IdCookieManager: TIdCookieManager);
var ProtHead: String;
begin
  IdHTTP.Request.Host:=NetCall.Host;
  IdHTTP.Request.Referer:=NetCall.Referer;
  IdHTTP.ReadTimeOut:=NetCall.ReadTimeOut;
  if not NetCall.SendBinary then
    IdHTTP.Request.ContentType:='application/x-www-form-urlencoded';
  IdHTTP.Request.UserAgent:='Mozilla/5.0 (Windows NT 5.1; rv:41.0) Gecko/20100101 Firefox/40.0';    // Some web sites don't like the old by-default one (i.e. malware);
  if NetCall.ClearCookies then IdCookieManager.CookieCollection.Clear;
  NetCall.Result:=0;
  NetCall.ErrorAbort:=False;
  NetCall.LastError:=0;
  if NetCall.HTTPS then ProtHead:='https://' else ProtHead:='http://';
  try
    if not NetCall.SendBinary then
      IdHTTP.Post(ProtHead+NetCall.URL,NetCall.TStrL,NetCall.TFS)
    else
      IdHTTP.Post(ProtHead+NetCall.URL,NetCall.TBinaryData,NetCall.TFS);
  except
    on E: EIdSocketError do         // Socket Error
      begin
        NetCall.Result:=1;
        NetCall.ErrorClass:=E.ClassName;
        NetCall.ErrorMessage:=E.Message;
        NetCall.LastError:=E.LastError;
      end;
    on E: EIdException do           // Other Indy Errors (HTTP Error)
      begin
        NetCall.Result:=2;
        NetCall.ErrorClass:=E.ClassName;
        NetCall.ErrorMessage:=E.Message;
        NetCall.LastError:=IdHTTP.ResponseCode;
      end;
    on E: Exception do              // Other Errors
      begin
        NetCall.Result:=3;
        NetCall.ErrorClass:=E.ClassName;
        NetCall.ErrorMessage:=E.Message;
        NetCall.LastError:=-1;
      end;
    else                            // Sanity
      begin
        NetCall.Result:=4;
        NetCall.ErrorClass:='?';
        NetCall.ErrorMessage:='???';
        NetCall.LastError:=-1;
      end;
  end;
  try
    IdHTTP.IOHandler.Close;
  except;
  end;
  try
    IdHTTP.IOHandler.InputBuffer.Clear;
  except;
  end;
end;

//
// Get an HTLM Page using Post or Get Method, and Save it in a Local File
//
procedure ShPostGetHTMLPage(Const ptrTIdHTTP: pTIdHTTP; Const ptrTIdCookieManager: pTIdCookieManager; Const Mode: Integer; Var NetCall: NetCall_Struct; Const FileName: String; Const DirName: String; Const LogTxt: String; Var ErrorType: Integer; Var ErrorAbort: Longbool);
var TFS: TFileStream;
var InternalError: Integer;
begin
  ErrorType:=0;
  ErrorAbort:=False;
  InternalError:=0;
  DeleteFile(DirName+FileName);
  TFS:=Nil;
  try
    TFS:=TFileStream.Create(DirName+FileName,fmCreate);
  except
    InternalError:=1;
  end;
  if InternalError<>0 then                // File not Created: IO Error
    begin
      ShLogText(1,'Can''t create '+FileName);
      ErrorType:=11;
      ErrorAbort:=True;
      Exit;
    end;
  // Post/Get HTML Page
  NetCall.TFS:=TFS;
  if Mode=1 then
    ShNetHTMLPost(NetCall,ptrTIdHTTP^,ptrTIdCookieManager^)
  else
    ShNetHTMLGet(NetCall,ptrTIdHTTP^,ptrTIdCookieManager^);
  if NetCall.Result=0 then
    begin
      ShLogText(2,'Got '+LogTxt+' Page');
      NetCall.DataLength:=NetCall.TFS.Size;
    end
  else
    begin
      ShLogText(1,'When Trying to Get '+LogTxt+' Page - Error Type: '+IntToStr(NetCall.Result)+'  Error Code: '+IntToStr(NetCall.LastError)+'  Messages  : '+NetCall.ErrorClass+'  '+NetCall.ErrorMessage);
      case NetCall.Result of
      1:      // Error Network (Socket)
        ErrorType:=12;
      2:      // Error Network (Indy/HTTP)
        ErrorType:=13;
      else    // Other Errors
        ErrorType:=40;
      end;
    end;
  TFS.Free;
  if ErrorType<>0 then
    DeleteFile(DirName+FileName);
  if NetCall.ErrorAbort then
    begin
      ErrorAbort:=True;
      Exit;
    end;
end;

end.

