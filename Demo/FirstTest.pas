unit FirstTest;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, WinSCard, WinSmCrd, SCardErr,
  StdCtrls, PCSCConnector;

type
  TForm1 = class(TForm)
    pcsc: TPCSCConnector;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    bt_Init: TButton;
    bt_Open: TButton;
    bt_Connect: TButton;
    bt_Close: TButton;
    bt_Disconnect: TButton;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    bt_Send: TButton;
    Memo1: TMemo;
    Label13: TLabel;
    Label14: TLabel;
    Button1: TButton;
    procedure pcscCardRemoved(Sender: TObject);
    procedure pcscError(Sender: TObject; ErrSource: TErrSource; ErrCode: Cardinal);
    procedure ShowData;
    procedure bt_InitClick(Sender: TObject);
    procedure bt_OpenClick(Sender: TObject);
    procedure bt_ConnectClick(Sender: TObject);
    procedure bt_CloseClick(Sender: TObject);
    procedure bt_DisconnectClick(Sender: TObject);
    procedure bt_SendClick(Sender: TObject);
    procedure pcscCardActive(Sender: TObject);
    procedure pcscCardInserted(Sender: TObject);
    procedure pcscCardInvalid(Sender: TObject);
    procedure pcscReaderConnect(Sender: TObject);
    procedure pcscReaderDisconnect(Sender: TObject);
    procedure pcscReaderListChange(Sender: TObject);
    procedure pcscReaderWaiting(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

const

HexChars    = '0123456789abcdefABCDEF';

function Hex2Bin(input: RawByteString): RawByteString;
var
  hex, output: RawByteString;
  loop: integer;
begin
 for loop := 1 to Length(input) do
 begin
  if Pos(input[loop], hexchars) > 0 then
    hex := hex + AnsiUpperCase(input[loop]);
 end;

 loop := 1;
 if Length(hex) > 0 then
 repeat
  output := output + AnsiChar(StrToInt('$'+Copy(hex,loop,2)));
  loop := loop + 2;
 until loop > Length(hex);

 Result := output;
end;

function Bin2HexExt(const input:string; const spaces, upcase: boolean): string;
var
   loop      : integer;
   hexresult : string;
begin
     hexresult := '';
     for loop := 1 to Length(input) do
        begin
        hexresult := hexresult + IntToHex(Ord(input[loop]),2);
        if spaces then hexresult := hexresult + ' ';
        end;
     if upcase then result := AnsiUpperCase(hexresult)
               else result := AnsiLowerCase(hexresult);
end;

procedure TForm1.ShowData;
begin
  label3.caption := IntToHex(pcsc.ReaderState,8);
  label4.caption := pcsc.AttrICCType;
  label5.caption := pcsc.AttrVendorName;
  label6.caption := pcsc.AttrVendorSerial;
  label14.caption := IntToHex(pcsc.AttrProtocol,8);
end;

procedure TForm1.pcscCardRemoved(Sender: TObject);
begin
memo1.Lines.Add('OnCardRemoved');
ShowData;
end;

procedure TForm1.pcscError(Sender: TObject; ErrSource: TErrSource; ErrCode: Cardinal);
begin
memo1.Lines.Add('OnError ' + IntToHex(ErrCode,8));
label1.caption := IntToHex(ErrCode,8);
ShowData;
end;

procedure TForm1.bt_InitClick(Sender: TObject);
begin
  pcsc.Init;
  pcsc.SelectedReaderIndex := 0;
end;

procedure TForm1.bt_OpenClick(Sender: TObject);
begin
if pcsc.Open then memo1.lines.add('OPEN: OK')
             else memo1.lines.add('OPEN: NOT OK');
end;

procedure TForm1.bt_ConnectClick(Sender: TObject);
begin
  pcsc.ConnectCard;

  if pcsc.CardConnected then memo1.lines.add('CONNECT to ''' + IntToStr(pcsc.SelectedReaderIndex) + ''' : OK')
                else memo1.lines.add('CONNECT to ''' + IntToStr(pcsc.SelectedReaderIndex) + ''' : NOT OK');
end;

procedure TForm1.bt_CloseClick(Sender: TObject);
begin
pcsc.Close;
end;

procedure TForm1.bt_DisconnectClick(Sender: TObject);
begin
  pcsc.DisconnectCard;
end;

procedure TForm1.bt_SendClick(Sender: TObject);
begin
label2.caption := Bin2HexExt(pcsc.GetResponseFromCard(Hex2Bin('a0f2000016')), true, true);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  test: RawByteString;
  OutStr: RawByteString;
begin
  pcsc.ConnectCard;
  try
    OutStr := '';

    if pcsc.SelectFile(RawByteString(#$2F#$06)) = CardStatusOK then
    begin
      pcsc.ReadBinary(0, 84, test);
      OutStr := OutStr + test;
    end;

  //  pcsc.GSMSelect(RawByteString('#$3f#$00'));

    if pcsc.SelectFile(RawByteString(#$2F#$07)) = CardStatusOK then
    begin
      pcsc.ReadBinary(0, 95, test);
      OutStr := OutStr + test;
    end;
  finally
    pcsc.DisconnectCard;
  end;

  ShowMessage(OutStr);
end;

procedure TForm1.pcscCardActive(Sender: TObject);
begin
memo1.Lines.Add('OnCardActive');
ShowData;
end;

procedure TForm1.pcscCardInserted(Sender: TObject);
begin
  memo1.Lines.Add('OnCardInserted');
  ShowData;

  Button1Click(nil);
end;

procedure TForm1.pcscCardInvalid(Sender: TObject);
begin
memo1.Lines.Add('OnCardInvalid');
ShowData;
end;

procedure TForm1.pcscReaderConnect(Sender: TObject);
begin
memo1.Lines.Add('OnReaderConnect');
ShowData;
end;

procedure TForm1.pcscReaderDisconnect(Sender: TObject);
begin
memo1.Lines.Add('OnReaderDisconnect');
ShowData;
end;

procedure TForm1.pcscReaderListChange(Sender: TObject);
begin
memo1.Lines.Add('OnReaderListChange');
end;

procedure TForm1.pcscReaderWaiting(Sender: TObject);
begin
memo1.Lines.Add('OnReaderWaiting');
end;

end.
