unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  libudp, StdCtrls, ExtCtrls,messagequeue;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    Memo1: TMemo;
    Button1: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Memo2: TMemo;
    Timer2: TTimer;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Edit2KeyPress(Sender: TObject; var Key: Char);
    procedure Timer2Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  tick:integer;
  udpcon:tudpconnection;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
begin
udpcon:=tudpconnection.create(edit1.text,4747);
timer1.enabled:=true;
timer2.enabled:=true;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var data:tudpdata;
    x:integer;
    s:string[200];
begin
inc(tick);

move(tick,data.data[0],4);
data.length:=4;
   if (not udpcon.send(data)) then
   memo1.lines.add(
    'FATAL - CANT SEND MESSAGE - MESSAGE QUEUE IS FULL');

while (not udpcon.received_data.empty) do
  begin
     data:=udpcon.received_data.front;
     udpcon.received_data.pop;
     move(data.data[0],x,4);

     memo1.lines.add('received   '+inttostr(x));
     if (data.length>4) then
       begin
        move(data.data[4],s,data.length);
        memo2.lines.add('<--('+
         inttostr(data.length)+'): '+s);
        beep;

       end;
  //   memo1.lines.add('incoming packet transfer');
  end;
end;

procedure TForm1.Edit2KeyPress(Sender: TObject; var Key: Char);
var data:tudpdata;
    x:integer;
    s:string[200];
begin
if (key=#13) then
  begin
   s:=edit2.text;


   data.length:=4+length(s)+1;
   x:=-1;
   move(x,data.data[0],4);
   memo1.lines.add('FF'+inttostr(integer(s[0])));
   move(s,data.data[4],length(s)+1);
   if (not udpcon.send(data)) then
   memo1.lines.add(
    'FATAL - CANT SEND MESSAGE - MESSAGE QUEUE IS FULL');


   edit2.text:='';
   memo2.lines.add('-->'+s);
  end;
end;

procedure TForm1.Timer2Timer(Sender: TObject);
begin
label1.caption:='Link quality: '+
 inttostr(udpcon.get_percent_acks)+' retransmissions:'+
 inttostr(udpcon.get_retransmission_count)+' waiting messages:'
 +inttostr(udpcon.get_queue_size);

end;

end.
