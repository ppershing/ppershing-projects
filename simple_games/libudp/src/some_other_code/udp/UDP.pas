unit UDP;

interface

uses
  Windows, Messages, SysUtils,  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, IdUDPClient, IdBaseComponent, IdComponent,
  IdUDPBase, IdUDPServer,
  IdStack;

type
  TUDPSearchEvent = Procedure(ResultIP, ResultName: String) of Object;
  TUDPSearchForm = class(TForm)
    RadioGroup1: TRadioGroup;
    Button1: TButton;
    IdUDPServer1: TIdUDPServer;
    IdUDPClient1: TIdUDPClient;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FLocalAddress: String;
    FActive: Boolean;
    FServerPort: Integer;
    FOnUDPRead: TUDPReadEvent;
    FHost: String;
    FOnException: TNotifyEvent;
    procedure SetActive(const Value: Boolean);
    procedure SetOnUDPRead(const Value: TUDPReadEvent);
    procedure SetHost(const Value: String);
    procedure ErrorDialog(Message: String; HelpContext: THelpContext);
    procedure SetOnException(const Value: TNotifyEvent);
  public
    AktIP: String;
    ReceiverForm2Event: TUDPSearchEvent;
    procedure SearchPartner;
    procedure Add(IP, Name: String);
    function WSGetHostByAddr(s: String) : String;
    function WSGetHostByName(s: String) : String;
    function ResolveHost(s: String) : String;
    function DoSend(var xt; Size, MaxSize: Integer): Integer;
    function PeerIP: String;
    property SearchEvent: TUDPSearchEvent read ReceiverForm2Event write ReceiverForm2Event;
    property LocalAddress : String read FLocalAddress;
    property Active: Boolean read FActive write SetActive;
    property Host: String read FHost write SetHost;
    property OnUDPRead: TUDPReadEvent read FOnUDPRead write SetOnUDPRead;
    property OnException: TNotifyEvent read FOnException write SetOnException;
  end;

var
  UDPSearchForm: TUDPSearchForm;

implementation

{$R *.dfm}
const
  UDPDefaultPort = 8760;

var
  MessageCount : Word;

procedure TUDPSearchForm.Add(IP, Name: String);
var
  s: String;
begin
   s := Name + '(' + IP + ')';
   RadioGroup1.Items.Add(s);
   Height := 50 + RadioGroup1.Items.Count * 13;
   if Name[1] = '<' then begin
     exit;
   end;
   if IP = AktIP then begin
     RadioGroup1.ItemIndex := RadioGroup1.Items.Count-1;
     exit
   end;
   if RadioGroup1.ItemIndex < 0 then begin
     RadioGroup1.ItemIndex := RadioGroup1.Items.Count-1;
     exit;
   end;
end;

procedure TUDPSearchForm.Button1Click(Sender: TObject);
var
  s, ResultIP, ResultName: String;
  p: Integer;
begin
  if RadioGroup1.Items.Count = 0 then begin
    Hide;
    Exit
  end;
  if RadioGroup1.ItemIndex < 0 then Exit;

  s := RadioGroup1.Items[RadioGroup1.ItemIndex];
  p := pos('(', s);
  ResultName := copy(s, 1, p-2);
  if p = 2 then exit;  // Error
  s := copy(s, p+1, High(Integer));
  p := pos(')', s);
  if p < 4 then exit;  // Error
  s := copy(s, 1, p-1);
  ResultIP := s;
  Hide;
  ReceiverForm2Event(ResultIP, ResultName);
end;

procedure TUDPSearchForm.SearchPartner;
var
  x: Array[0..3] of Byte;
begin
  RadioGroup1.Items.Clear;
  RadioGroup1.ItemIndex := -1;
  Height := 50+13;
  Top := Top;
  Left := left;
  Show;
  IdUDPClient1.Host := '255.255.255.255';
  IdUDPClient1.Active := true;
  IdUDPClient1.BroadcastEnabled := True;
  x[0] := $00; // Search Receivers
  x[1] := $00; // Search
  x[2] := MessageCount div $100;
  x[3] := MessageCount mod $100;
  inc(MessageCount);
  IdUDPClient1.Port := UDPDefaultPort;
  IdUDPClient1.SendBuffer(x, 4);
  IdUDPClient1.BroadcastEnabled := False;
end;

procedure TUDPSearchForm.FormCreate(Sender: TObject);
begin
  FLocalAddress := GStack.LocalAddress;
  IdUDPClient1.Port := UDPDefaultPort;
  IdUDPClient1.Binding.Port := UDPDefaultPort;     // Has no effect. why ?
  IdUDPServer1.DefaultPort := UDPDefaultPort;
end;

function TUDPSearchForm.WSGetHostByAddr(s: String): String;
begin
  Result := GStack.WSGetHostByAddr(s);
end;

function TUDPSearchForm.WSGetHostByName(s: String): String;
begin
  Result := GStack.WSGetHostByName(s);
end;

function TUDPSearchForm.ResolveHost(s: String): String;
begin
  Result := GStack.ResolveHost(s);
end;


function TUDPSearchForm.DoSend(var xt; Size, MaxSize: Integer): Integer;
var
  x: Array[0..3] of Byte absolute xt;
begin
  x[2] := MessageCount div $100;
  x[3] := MessageCount mod $100;
  inc(MessageCount);
  IdUDPClient1.SendBuffer(xt, Size);
  Result := Size;
end;

procedure TUDPSearchForm.ErrorDialog(Message: String; HelpContext: THelpContext);
begin
  MessageDlg(Message, mtError, [mbOK], HelpContext);
end;

procedure TUDPSearchForm.SetActive(const Value: Boolean);
begin
  FActive := Value;
  try
    IdUDPServer1.Active := Value;
   except
     on E: Exception do begin
       IdUDPServer1.Bindings.Clear;
       if assigned(FOnException) then FOnException(self);
       ErrorDialog(E.Message, E.HelpContext);
       Application.Terminate;
     end;  
  end;
  IdUDPServer1.BroadcastEnabled := true;
end;

procedure TUDPSearchForm.SetOnUDPRead(const Value: TUDPReadEvent);
begin
  FOnUDPRead := Value;
  IdUDPServer1.OnUDPRead := Value;
end;

procedure TUDPSearchForm.SetHost(const Value: String);
begin
  FHost := Value;
  IdUDPClient1.Host := Value;
end;


function TUDPSearchForm.PeerIP: String;
begin
   Result := IdUDPServer1.Binding.PeerIP;
end;

procedure TUDPSearchForm.SetOnException(const Value: TNotifyEvent);
begin
  FOnException := Value;
end;

end.
