unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    Label1: TLabel;
    Image1: TImage;
    TrackBar1: TTrackBar;
    Label2: TLabel;
    procedure TrackBar1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

  const listov=1;
var   tm:longint;
      tick:longint;

implementation

{$R *.DFM}

procedure TForm1.TrackBar1Change(Sender: TObject);
begin

label2.caption:=inttostr(trackbar1.position);
image1.picture.bitmap.canvas.brush.color:=clwhite;
image1.picture.bitmap.canvas.Rectangle(0,0,100,100);

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
tick:=0;
image1.picture.bitmap.width:=100;
image1.picture.bitmap.height:=100;

end;

procedure TForm1.Timer1Timer(Sender: TObject);
var tn:longint;
    ts:ttimestamp;
    uhol:double;
    q:integer;
    t2:integer;
begin
inc(tick);
t2:=tick;
if (tick mod 10=0) then
  begin
ts:=DateTimeToTimeStamp(time);
 tn:=ts.Time;
 label1.caption:=floattostrF(10000/(tn-tm),ffgeneral,5,5);
 tm:=tn;
  end;


for q:=1 to listov do
  begin

     image1.picture.bitmap.canvas.Pen.color:=clblack;
   uhol:=(10*tick/trackbar1.position+(q/listov))*2*Pi;
   image1.Picture.bitmap.canvas.moveto(50,50);
   image1.picture.bitmap.canvas.lineto(50+trunc(sin(uhol)*50),50+trunc(cos(uhol)*50));
               image1.Repaint;
               sleep(5);
     image1.picture.bitmap.canvas.Pen.color:=clwhite;
     uhol:=(10*t2/trackbar1.position+(q/listov))*2*Pi;
   image1.Picture.bitmap.canvas.moveto(50,50);
   image1.picture.bitmap.canvas.lineto(50+trunc(sin(uhol)*50),50+trunc(cos(uhol)*50));

  end;

end;

end.
