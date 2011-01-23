unit chat1;

{

Chat application with Delphi source

http://delphi.about.com/library/weekly/aa101105a.htm

Full Delphi source to code to a simple Chat application.
Learn how UDP (User Datagram Protocol) broadcast can be
used to find partners with unknown IP addresses in the network.

Programming by Michael Schnell
Brought to you by Zarko Gajic

}

interface

uses
  Windows, Messages, SysUtils,  Classes, Graphics, Controls, Forms,
  Dialogs, IdSocketHandle, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Edit1: TEdit;
    Button2: TButton;
    Edit2: TEdit;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    Activated: Boolean;
    procedure SearchEvent(ResultIP, ResultName: String);
    procedure UDPRead(Sender: TObject; AData: TStream;
      ABinding: TIdSocketHandle);
    procedure UDPException(Sender: TObject);
  public
  end;

var
  Form1: TForm1;

implementation

uses UDP;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  UDPSearchForm.SearchEvent := SearchEvent;
  UDPSearchForm.Left := Left;
  UDPSearchForm.Top := Top;
  UDPSearchForm.AktIP := Edit1.Text;
  UDPSearchForm.SearchPartner;
end;

procedure TForm1.SearchEvent(ResultIP, ResultName: String);
begin
  Edit1.Text := ResultIP;
  Label1.Caption := ResultName;
end;

procedure TForm1.FormActivate(Sender: TObject);
var
  s, s2: String;
begin
  if Activated then exit;
  Memo1.Clear;
  Activated := true;
  UDPSearchForm.OnUDPRead := UDPRead;
  UDPSearchForm.OnException := UDPException;
  UDPSearchForm.Active := true;
  s := UDPSearchForm.LocalAddress;
  s2 := UDPSearchForm.WSGetHostByAddr(s);
  Memo1.Lines.Add('I''m (' + s + ') ' + s2);
end;

procedure TForm1.UDPRead(Sender: TObject; AData: TStream;
  ABinding: TIdSocketHandle);
var
  Buffer: Array [0..2047] of Byte;
  count: Integer;
  PeerIP: String;
  PeerPort: Integer;
  s, ss: String;
  i: Integer;
begin
  PeerIP := ABinding.PeerIP;
  PeerPort:= ABinding.PeerPort;
  count := AData.Size;
  if count > Length(Buffer) then begin
    exit;
  end;
  AData.Read(Buffer, count);
  if (Buffer[0] <> $00) and  (Buffer[0] <> $01) then begin  // not search
    Edit1.Text:= PeerIP;
  end;
  case Buffer[0] of
   $00: begin   // search request
    case count of
     4: begin
      case Buffer[1] of
       0: begin
        Buffer[0] := $01;
        UDPSearchForm.Host := PeerIP;
        UDPSearchForm.DoSend(Buffer, 4, Length(Buffer));
        Memo1.Lines.Add('Inquiry [' + UDPSearchForm.WSGetHostByAddr(PeerIP) + '(' + PeerIP + ')' +          ' Port: ' + IntToStr(PeerPort) +
          ']');
       end;
      end;
     end;
    end;
   end;
   $01: begin // Search Reply
    case count of
     4: begin
      case Buffer[1] of
       0: begin
        ss := UDPSearchForm.WSGetHostByAddr(PeerIP);
        s := '[' + ss + '(' + PeerIP + ')' +
          ' Client Port: ' + IntToStr(PeerPort) +
          ']';
        Memo1.Lines.Add('Inquiry Reply ' + s);
        if PeerIp = UDPSearchForm.LocalAddress then begin
          ss := '<myself>' + ss;
        end;
        UDPSearchForm.Add(PeerIP, ss);
       end;
      end;
     end;
    end;
   end;
   $10: begin // Text
    case Buffer[1] of
     0: begin
      s := '';
      for i := 4 to count-1 do begin
        s := s + char(Buffer[i]);
      end;
      Memo1.Lines.Add(PeerIP+'->' + s);
     end;
    end;
   end;
  end;
end;

procedure TForm1.UDPException(Sender: TObject);
begin
  //nothing
end;


procedure TForm1.Button2Click(Sender: TObject);
var
  x: Array[0..100] of Byte;
  i: Integer;
begin
  UDPSearchForm.Host := Edit1.Text;
  UDPSearchForm.Active := true;
  x[0] := $10; // Text
  x[1] := 0;   // Type 0
  for i := 1 to Length(Edit2.Text) do begin
    x[i+3] := Byte(Edit2.Text[i]);
  end;
  UDPSearchForm.DoSend(x, 4+Length(Edit2.Text), length(x));
end;

end.
