{******************************************************************}
{                                                                  }
{ PC/SC Interface component                                        }
{ Helps you access a cardreader through Microsofts SmartCard API   }
{                                                                  }
{ The Original Code is PCSCConnector.pas                           }
{                                                                  }
{ The Initial Developer of the Original Code is                    }
{ Norbert Huettisch (nobbi(at)nobbi.com)                           }
{                                                                  }
{ Any suggestions and improvements to the code are appreciated     }
{                                                                  }
{ This Code uses a modified   SCardErr.pas (included)              }
{ This Code uses a modified   WinSCard.pas (included)              }
{ This code uses the original WinSmCrd.pas (included)              }
{                                                                  }
{ All originally made by Chris Dickerson (chrisd(at)tsc.com),      }
{ available as 'Interface units for the Microsoft Smart Card API'  }
{ at the Project JEDI Homepage http://www.delphi-jedi.org          }
{                                                                  }
{ Version info:                                                    }
{ 021230 - initial version                                         }
{ 030101 - routed errors from 'init' to the OnError event          }
{                                                                  }
{                                                                  }
{******************************************************************}
{                                                                  }
{ The contents of this file are subject to the                     }
{                                                                  }
{       Mozilla Public License Version 1.1 (the "License")         }
{                                                                  }
{ You may not use this file except in compliance with the License. }
{ You may obtain a copy of the License at                          }
{ http://www.mozilla.org/MPL/                                      }
{                                                                  }
{ Software distributed under the License is distributed on an      }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or   }
{ implied. See the License for the specific language governing     }
{ rights and limitations under the License.                        }
{                                                                  }
{******************************************************************}

unit PCSCConnector;

interface

uses
  Windows, Messages, Forms, Classes, SysUtils,
  SCardErr, WinSCard, WinSmCrd;

type
  TErrSource         = (esInit, esConnect, esGetStatus, esTransmit);
  TNeededPIN         = (npPIN1, npPIN2, npPUK1, npPUK2);

  TPCSCErrorEvent    = procedure(Sender: TObject; ErrSource: TErrSource; ErrCode: LongInt) of object;
  TPCSCPinEvent      = procedure(Sender: TObject; NeedPIN: TNeededPIN) of object;

const
  MAXAPDULENGTH      = 260; // CLA + INS + P1..3 + 255Bytes
  NOREADERSELECTED   = -1;
  SCARD_PCI_T0       : SCARD_IO_REQUEST = (dwProtocol:1; dbPciLength:8);
  SCARD_PCI_T1       : SCARD_IO_REQUEST = (dwProtocol:2; dbPciLength:8);
  SCARD_PROTOCOL_T0  = $00000001;
  SCARD_PROTOCOL_T1  = $00000002;
  SCARD_PROTOCOL_RAW = $00010000;
  SCARD_PROTOCOL_UNK = $00000000;

  WM_CARDSTATE     = WM_USER + 42;

  CardStatusOK           = $9000;
  CardStatusMemoryError  = $9240;
  CardStatusNoEFSelected = $9400;
  CardStatusOutOfRange   = $9402;
  CardStatusNotFound     = $9404;
  CardStatusFCDoNotMatch = $9408;
  CardStatusCHVNeeded    = $9802;
  CardStatusAuthFailed   = $9804;
  CardStatusAuthFailedBl = $9840;
  CardStatusTechProblem  = $6F00;
  CardStatusResponseData = $9F;

  GSMFileTypeRFU = 0;
  GSMFileTypeMF  = 1;
  GSMFileTypeDF  = 2;
  GSMFileTypeEF  = 4;

  GSMEfTransp    = 0;
  GSMEfLinFixed  = 1;
  GSMEfCyclic    = 3;

type
  TPCSCConnector = class;

  TReaderWatcher = class(TThread)
  private
    FOwner: TPCSCConnector;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TPCSCConnector);

  end;

  TPCSCConnector = class(TComponent)
  private
    FContext: SCARDCONTEXT;
    FCardHandle: SCARDHANDLE;
    FSelectedReaderIndex: integer;
    FReaderList: TStringlist;
    FAttrProtocol: integer;
    FAttrICCType: string;
    FAttrCardATR: string;
    FAttrVendorName: string;
    FAttrVendorSerial: string;
    FCurrentFile: string;
    FFileInfo: string;
    FDirInfo: string;
    FVoltage30: boolean;
    FVoltage18: boolean;
    FOnReaderWaiting: TNotifyEvent;
    FOnReaderListChange: TNotifyEvent;
    FOnCardInserted: TNotifyEvent;
    FOnCardActive: TNotifyEvent;
    FOnCardRemoved: TNotifyEvent;
    FOnCardInvalid: TNotifyEvent;
    FOnError: TPCSCErrorEvent;
    FOnCHVNeeded: TPCSCPinEvent;
    FReaderWatcher: TReaderWatcher;
    FActReaderState : cardinal;
    FLastReaderState: cardinal;
    FNotifyHandle: HWND;
    FCommandCls: Byte;
    procedure SetSelectedReaderIndex(Value: integer);
    procedure MessageWndProc(var Msg: TMessage);
    function  ConnectCardInSelectedReader: boolean;
    procedure ProcessReaderState(const OldState,NewState: cardinal);
    procedure GetReaderAttributes;
    procedure GetCardAttributes;
    procedure ClearReaderAttributes;
    procedure ClearCardAttributes;
    function IsReaderOpen: boolean;
    function GetReaderState: cardinal;
    procedure CloseAndDisconnect;
    procedure CardInsertedAction;
    procedure CardActiveAction;
    procedure CardRemovedAction;
    function GetIsCardConnected: boolean;
    procedure ReleaseContext;
    procedure DoOnTerminateWatcher(Sender: TObject);
    function GetSelectedReaderName: String;
    procedure ResetReaderStatVars;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Init: boolean; // init once to fetch reader list and init component or call to check for new connected/disconnected readers

    function Open: boolean; // open selected reader device
    procedure Close;

    procedure ConnectCard; // connect inserted card
    procedure DisconnectCard;

    function GetResponseFromCard(const apdu: RawByteString): RawByteString; overload;
    function GetResponseFromCard(const command: RawByteString; var data: RawByteString; var sw1, sw2: byte): boolean; overload;

    function CardStatus: integer;
    function SelectFile(const FileID: RawByteString): integer;
    function ReadBinary(const Offset, Length: integer; out Data: RawByteString): integer;
  published
    property SelectedReaderIndex: integer read FSelectedReaderIndex write SetSelectedReaderIndex default -1;
    property SelectedReaderName: String read GetSelectedReaderName;

    property OnCardInserted: TNotifyEvent read FOnCardInserted write FOnCardInserted;
    property OnCardActive: TNotifyEvent read FOnCardActive write FOnCardActive;
    property OnCardRemoved: TNotifyEvent read FOnCardRemoved write FOnCardRemoved;
    property OnCardInvalid: TNotifyEvent read FOnCardInvalid write FOnCardInvalid;
    property OnReaderWaiting: TNotifyEvent read FOnReaderWaiting write FOnReaderWaiting;
    property OnReaderListChange: TNotifyEvent read FOnReaderListChange write FOnReaderListChange;
    property OnError: TPCSCErrorEvent read FOnError write FOnError;
    property OnCHVNeeded: TPCSCPinEvent read FOnCHVNeeded write FOnCHVNeeded;

    // EZ - bei CH Krankenkassenkarten funktioniert CLS $A0 nicht, es muss $00 sein. (CLS ist das erste Byte des Commands)
    property CommandCls: Byte read FCommandCls write FCommandCls default 0;

    property ReaderList: TStringList read FReaderList;
    property CardConnected: boolean read GetIsCardConnected;
    property Opened: Boolean read IsReaderOpen;
    property ReaderState: Cardinal read GetReaderState;
    property AttrProtocol: Integer read FAttrProtocol;
    property AttrICCType: String read FAttrICCType;
    property AttrCardATR: String read FAttrCardATR;
    property AttrVendorName: String read FAttrVendorName;
    property AttrVendorSerial: String read FAttrVendorSerial;
    property CurrentFile: String read FCurrentFile;
    property FileInfo: String read FFileInfo;
    property DirInfo: String read FDirInfo;
    property Voltage30: Boolean read FVoltage30;
    property Voltage18: Boolean read FVoltage18;
  end;

procedure Register;

implementation

const
  GCGetStatus   = RawByteString(#$F2#$00#$00#$16);
  GCGetResponse = RawByteString(#$C0#$00#$00);
  GCSelectFile  = RawByteString(#$A4#$00#$00);
  GCReadBinary  = RawByteString(#$B0);

  MasterFileFID  = RawByteString(#$3f#$00);
  DFgsm900       = RawByteString(#$7f#$20);
  DFgsm1800      = RawByteString(#$7f#$21);

procedure Register;
begin
  RegisterComponents('More...', [TPCSCConnector]);
end;

function OrdD(const From: string; const Index: integer): integer;
begin
  if Index <= Length(From) then
    Result := Ord(From[Index])
  else
    Result := 0;
end;

procedure TPCSCConnector.MessageWndProc(var Msg: TMessage);
begin
  if (Msg.Msg = WM_CARDSTATE) then
  begin
    if Msg.WParam <> SCARD_S_SUCCESS then
    begin
      if Assigned(FOnError) then
        FOnError(Self, esGetStatus, Msg.WParam);
    end;

    if FActReaderState <> FLastReaderState then
    begin
      ProcessReaderState(FLastReaderState, FActReaderState);
    end;
  end else
    Msg.Result := DefWindowProc(FNotifyHandle, Msg.Msg, Msg.WParam, Msg.LParam);
end;

constructor TPCSCConnector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FReaderList   := TStringlist.Create;
  FContext      := 0;
  FCardHandle   := 0;
  FCommandCls := 0;
  FSelectedReaderIndex := -1;

  FReaderWatcher := nil;
  ResetReaderStatVars;

  ClearReaderAttributes;
  ClearCardAttributes;

  FNotifyHandle := 0;
end;

procedure TPCSCConnector.ResetReaderStatVars;
begin
  FActReaderState  := SCARD_STATE_UNAWARE;
  FLastReaderState := SCARD_STATE_UNAWARE;
end;

procedure TPCSCConnector.ReleaseContext;
begin
  if (FContext <> 0) and (SCardIsValidContext(FContext) = SCARD_S_SUCCESS) then
  begin
    SCardReleaseContext(FContext);
  end;

  FContext := 0;
end;

destructor TPCSCConnector.Destroy;
begin
  CloseAndDisconnect;
  ReleaseContext;

  FReaderList.Free;

  if (FNotifyHandle <> 0) then
    DeallocateHWnd(FNotifyHandle);

  inherited Destroy;
end;

function TPCSCConnector.Init: boolean;
var
  RetVar: cardinal;
  ReaderList: string;
  ReaderListSize: integer;
  i: integer;
begin
  Result := false;

  if (not (csDesigning in ComponentState)) and (FNotifyHandle = 0) then
    FNotifyHandle := AllocateHWnd(MessageWndProc);

  CloseAndDisconnect;
  ReleaseContext;

  RetVar := SCardEstablishContext(SCARD_SCOPE_USER, nil, nil, @FContext);

  if RetVar = SCARD_S_SUCCESS then
  begin
    ReaderListSize := 0;
    RetVar := SCardListReadersW(FContext, nil, nil, ReaderListSize);

    if RetVar = SCARD_S_SUCCESS then
    begin
      SetLength(ReaderList, ReaderListSize);
      SCardListReadersW(FContext, nil, PChar(ReaderList), ReaderListSize);

      FReaderList.Delimiter := #0;
      FReaderList.StrictDelimiter := true;
      FReaderList.DelimitedText := ReaderList;

      for i := FReaderList.Count-1 downto 0 do
      begin
        if FReaderList[i] = '' then
          FReaderList.Delete(i);
      end;

      if FReaderList.Count > 0 then
      begin
        if Assigned(FOnReaderListChange) then
          FOnReaderListChange(Self);

        Result := true;
      end;
    end else
    if Assigned(FOnError) then
      FOnError(Self, esInit, RetVar);
  end else
  if Assigned(FOnError) then
    FOnError(Self, esInit, RetVar);
end;

function TPCSCConnector.Open: boolean;
begin
  if FNotifyHandle = 0 then
    raise Exception.Create('Not initialized yet!');

  CloseAndDisconnect;

  if (FSelectedReaderIndex > NOREADERSELECTED) and
     (SCardIsValidContext(FContext) = SCARD_S_SUCCESS) then
  begin
    ResetReaderStatVars;

    FReaderWatcher := TReaderWatcher.Create(self);
    FReaderWatcher.OnTerminate := DoOnTerminateWatcher;
    FReaderWatcher.Suspended := false;

    Result := true;
  end else
    Result := false;
end;

procedure TPCSCConnector.DoOnTerminateWatcher(Sender: TObject);
begin
  if FReaderWatcher = Sender then
    FReaderWatcher := nil;
end;

procedure TPCSCConnector.Close;
begin
  if Assigned(FReaderWatcher) then
  begin
    FReaderWatcher.Terminate;
    FReaderWatcher := nil;
  end;

  ResetReaderStatVars;

  SCardCancel(FContext);

  if CardConnected then
    DisconnectCard;
end;

procedure TPCSCConnector.ConnectCard;
begin
  if not CardConnected then
  begin
    if FSelectedReaderIndex > NOREADERSELECTED then
    begin
      ConnectCardInSelectedReader;
    end;
  end;
end;

procedure TPCSCConnector.DisconnectCard;
begin
  if CardConnected then
  begin
    SCardDisconnect(FCardHandle, SCARD_RESET_CARD);
    FCardHandle := 0;
  end;
end;

procedure TPCSCConnector.CloseAndDisconnect;
begin
  if CardConnected then
    DisconnectCard;

  if IsReaderOpen then
    Close;
end;

function TPCSCConnector.ConnectCardInSelectedReader: boolean;
var
  RetVar : cardinal;
begin
  RetVar := SCardConnectW(FContext,
                          PChar(SelectedReaderName),
                          SCARD_SHARE_EXCLUSIVE,
                          SCARD_PROTOCOL_Tx,
                          FCardHandle,
                          @FAttrProtocol);
  case RetVar of
    SCARD_S_SUCCESS:
      begin
        CardActiveAction;
        Result := true;
      end;

    SCARD_W_REMOVED_CARD:
      begin
        Result := true;
      end;

    else begin
      Result := false;

      if Assigned(FOnError) then
        FOnError(Self, esConnect, RetVar);
    end;
  end;
end;

procedure TPCSCConnector.ProcessReaderState(const OldState, NewState: cardinal);
var
  CardInOld, CardInNew     : boolean;
  ReaderEmOld, ReaderEmNew : boolean;
  CardMuteOld, CardMuteNew : boolean;
  CardIgnore               : boolean;
begin
  CardInOld   := (OldState and SCARD_STATE_PRESENT) > 0;
  CardInNew   := (NewState and SCARD_STATE_PRESENT) > 0;
  ReaderEmOld := (OldState and SCARD_STATE_EMPTY) > 0;
  ReaderEmNew := (NewState and SCARD_STATE_EMPTY) > 0;
  CardMuteOld := (OldState and SCARD_STATE_MUTE) > 0;
  CardMuteNew := (NewState and SCARD_STATE_MUTE) > 0;
  CardIgnore  := (NewState and SCARD_STATE_IGNORE) > 0;

  FLastReaderState := NewState;

  if (CardMuteNew and not CardMuteold) and Assigned(FOnCardInvalid) then
    FOnCardInvalid(Self);

  if CardInNew and (not CardInOld) and (not CardMuteNew) and (not CardIgnore) then
    CardInsertedAction;

  if CardInOld and not CardInNew then
    CardRemovedAction;

  if ReaderEmNew and not ReaderEmOld and Assigned(FOnReaderWaiting) then
  begin
    FOnReaderWaiting(Self);
  end;
end;

procedure TPCSCConnector.CardInsertedAction;
begin
  if CardConnected then
    CardActiveAction;

  if Assigned(FOnCardInserted) then
    FOnCardInserted(Self);
end;

procedure TPCSCConnector.CardActiveAction;
begin
  GetReaderAttributes;

  if FAttrProtocol <> SCARD_PROTOCOL_UNK then
  begin
    GetCardAttributes;

    if Assigned(FOnCardActive) then
      FOnCardActive(Self);
  end;
end;

procedure TPCSCConnector.CardRemovedAction;
begin
  ClearReaderAttributes;
  ClearCardAttributes;

  if Assigned(FOnCardRemoved) then
    FOnCardRemoved(Self);

  DisconnectCard;
end;

procedure TPCSCConnector.SetSelectedReaderIndex(Value: Integer);
begin
  if Value <> FSelectedReaderIndex then
  begin
    CloseAndDisconnect;

    if Value < FReaderList.Count then
    begin
      FSelectedReaderIndex := Value;
    end else
    begin
      FSelectedReaderIndex := -1;
    end;
  end;
end;

function TPCSCConnector.IsReaderOpen: boolean;
begin
  Result := Assigned(FReaderWatcher) and not FReaderWatcher.Terminated;
end;

function TPCSCConnector.GetReaderState: cardinal;
begin
  Result := FActReaderState;
end;

procedure TPCSCConnector.GetReaderAttributes;
var
  RetVar : cardinal;
  ABuf   : AnsiString;
  AIBuf  : integer;
  ALen   : integer;
begin
  ABuf := StringOfChar(AnsiChar(#0), 127);

  ALen := Length(ABuf);
  RetVar := SCardGetAttrib(FCardHandle, SCARD_ATTR_ATR_STRING, Pointer(ABuf), @ALen);
  if RetVar = SCARD_S_SUCCESS then
    FAttrCardATR := Copy(ABuf, 1, ALen-1)
  else
    FAttrCardATR := '';

  ALen := Length(ABuf);
  RetVar := SCardGetAttrib(FCardHandle, SCARD_ATTR_VENDOR_NAME, Pointer(ABuf), @ALen);
  if RetVar = SCARD_S_SUCCESS then
    FAttrVendorName := Copy(ABuf, 1, ALen-1)
  else
    FAttrVendorName := '';

  ALen := Length(ABuf);
  RetVar := SCardGetAttrib(FCardHandle, SCARD_ATTR_VENDOR_IFD_SERIAL_NO, Pointer(ABuf), @ALen);
  if RetVar = SCARD_S_SUCCESS then
    FAttrVendorSerial := Copy(ABuf, 1, ALen-1)
  else
    FAttrVendorSerial := '';

  ALen := SizeOf(AIBuf);
  RetVar := SCardGetAttrib(FCardHandle, SCARD_ATTR_CURRENT_PROTOCOL_TYPE, @AIBuf, @ALen);
  if RetVar = SCARD_S_SUCCESS then
    FAttrProtocol := AIBuf
  else
    FAttrProtocol := 0;

  ALen := SizeOf(AIBuf);
  AIBuf := 0;
  RetVar := SCardGetAttrib(FCardHandle, SCARD_ATTR_ICC_TYPE_PER_ATR, @AIBuf, @ALen);
  if RetVar = SCARD_S_SUCCESS then
  begin
    case AIBuf of
      1: FAttrICCType := 'ISO7816A';
      2: FAttrICCType := 'ISO7816S';
      else FAttrICCType := 'UNKNOWN';
    end;
  end else
    FAttrICCType := '';
end;

procedure TPCSCConnector.GetCardAttributes;
begin
  if SelectFile(DFgsm900) = CardStatusOK then
  begin
    FVoltage30 := (OrdD(FDirInfo, 14) and $10) > 0;
    FVoltage18 := (OrdD(FDirInfo, 14) and $20) > 0;
  end;
end;

function TPCSCConnector.GetIsCardConnected: boolean;
begin
  Result := FCardHandle <> 0;
end;

procedure TPCSCConnector.ClearReaderAttributes;
begin
  FAttrCardATR      := '';
  FAttrVendorName   := '';
  FAttrVendorSerial := '';
  FAttrProtocol     := 0;
  FAttrICCType      := '';
end;

procedure TPCSCConnector.ClearCardAttributes;
begin
  FCurrentFile := '';
  FFileInfo    := '';
  FDirInfo     := '';
  FVoltage30   := false;
  FVoltage18   := false;
end;

function TPCSCConnector.GetResponseFromCard(const APdu: RawByteString): RawByteString;
var
  RetVar : LongInt;
  SBuf   : TBytes;
  SLen   : cardinal;
  RBuf   : TBytes;
  RLen   : cardinal;
  Ppci   : Pointer;
begin
  SLen := Length(APdu);
  SetLength(SBuf, SLen);
  Move(APdu[1], SBuf[0], SLen);

  SetLength(RBuf, MAXAPDULENGTH);
  RLen := Length(RBuf);
  ZeroMemory(@RBuf[0], RLen);

  if Length(SBuf) <= MAXAPDULENGTH then
  begin
    case FAttrProtocol of
      SCARD_PROTOCOL_T0 : Ppci := @SCARD_PCI_T0;
      SCARD_PROTOCOL_T1 : Ppci := @SCARD_PCI_T1;
      else                Ppci := nil;
    end;

    RetVar := SCardTransmit(FCardHandle, Ppci, @SBuf[0], SLen, nil, @RBuf[0], @RLen);
    if RetVar = SCARD_S_SUCCESS then
    begin
      SetLength(Result, RLen);
      Move(RBuf[0], Result[1], RLen);
    end
    else
    begin
      Result := '';
      if Assigned(FOnError) then FOnError(Self, esTransmit, RetVar);
    end;
  end;
end;

function TPCSCConnector.GetResponseFromCard(const Command: RawByteString; var Data: RawByteString; var sw1, sw2: byte): boolean;
var
  Answer : RawByteString;
  AnswerL: integer;
begin
  Answer := GetResponseFromCard(Command + Data);
  AnswerL := Length(Answer);
  if AnswerL >= 2 then
  begin
    Data := Copy(Answer, 1, AnswerL - 2);
    sw1  := Ord(Answer[AnswerL - 1]);
    sw2  := Ord(Answer[AnswerL]);

    if sw1 = CardStatusResponseData then
    begin
      Data := AnsiChar(sw2);

      if not GetResponseFromCard(AnsiChar(CommandCls) + GCGetResponse, Data, sw1, sw2) then
      begin
        Data := '';
        sw1  := 0;
        sw2  := 0;
        Result := false;
      end else
        Result := true;
    end else
      Result := true;
  end else
  begin
    Data := '';
    sw1  := 0;
    sw2  := 0;
    Result := false;
  end;
end;

function TPCSCConnector.GetSelectedReaderName: String;
begin
  if (SelectedReaderIndex > -1) and (SelectedReaderIndex < ReaderList.Count) then
    Result := ReaderList[SelectedReaderIndex]
  else
    Result := '';
end;

function TPCSCConnector.CardStatus: integer;
var
  Answer: RawByteString;
  sw1, sw2: byte;
begin
  GetResponseFromCard(AnsiChar(CommandCls) + GCGetStatus, Answer, sw1, sw2);

  Result := (sw1 shl 8) + sw2;

  if Result = CardStatusOK then
  begin
    FDirInfo := Answer;
    FCurrentFile := Copy(Answer, 5, 2);
  end else
  begin
    FDirInfo := '';
  end;
end;

function TPCSCConnector.SelectFile(const FileID: RawByteString): integer;
var
  Answer: RawByteString;
  sw1, sw2: byte;
begin
  Answer := FileID;
  GetResponseFromCard(AnsiChar(CommandCls) + GCSelectFile + AnsiChar(Length(FileID)), Answer, sw1, sw2);

  Result := (sw1 shl 8) + sw2;

  if Result = CardStatusOK then
  begin
    FCurrentFile := Copy(Answer, 5, 2);

    if OrdD(Answer, 7) = GSMFileTypeEF then
    begin
      FFileInfo := Answer;
    end else
    begin
      FDirInfo := Answer;
    end;
 end;
end;

function TPCSCConnector.ReadBinary(const Offset, Length: integer; out Data: RawByteString): integer;
var
  Command: RawByteString;
  sw1, sw2: byte;
begin
  Data := '';
  Command := AnsiChar(CommandCls) + GCReadBinary + AnsiChar(Offset div 256) + AnsiChar(Offset mod 256) + AnsiChar(Length mod 256);
  GetResponseFromCard(Command, Data, sw1, sw2);

  Result := (sw1 shl 8) + sw2;
end;

{ TReaderWatcher }

constructor TReaderWatcher.Create(AOwner: TPCSCConnector);
begin
  inherited Create(true);
  FOwner := AOwner;
end;

procedure TReaderWatcher.Execute;
var
  RetVar   : cardinal;
  RStates  : array[0..1] of SCARD_READERSTATEW;
  SelReader: String;
begin
  FreeOnTerminate := true;

  SelReader := FOwner.SelectedReaderName;

  ZeroMemory(@RStates[0], SizeOf(SCARD_READERSTATEW));
  RStates[0].szReader     := PChar(SelReader);
  RStates[0].pvUserData   := nil;
  RStates[0].dwEventState := FOwner.FActReaderState;

  while (not Terminated) and FOwner.IsReaderOpen do
  begin
    if (SCardIsValidContext(FOwner.FContext) <> SCARD_S_SUCCESS) then
    begin
      //RetVal := SCardEstablishContext(...);
      Exit;
    end;

    RetVar := SCardGetStatusChangeW(FOwner.FContext, 100, RStates, 1);

    if not Terminated then
    begin
      case RetVar of
        SCARD_E_TIMEOUT:;

        SCARD_S_SUCCESS:
          begin
           if ((RStates[0].dwEventState and SCARD_STATE_CHANGED) <> 0) then
           begin
            RStates[0].dwCurrentState := RStates[0].dwEventState xor SCARD_STATE_CHANGED;
            FOwner.FActReaderState := RStates[0].dwEventState;

            PostMessage(FOwner.FNotifyHandle, WM_CARDSTATE, RetVar, 0);
          end;
        end;
      end;
    end;
  end;
end;

end.

