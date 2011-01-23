unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls;

type
  TForm1 = class(TForm)
    Image1: TImage;
    Button1: TButton;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;
type tpoint=record
 x,y:double;
end;


type tulomok=record
 pos,v:tpoint;
 end;


const ULOMKOV=20;
const SIZE=600;
const F_konst=20;
const startx=400;
const starty=SIZE div 2;
const startv=0.7;
const DT=1;


var
  Form1: TForm1;
  ulomok:array[0..ULOMKOV] of tulomok;
  tick:longint;


implementation

{$R *.DFM}
function vector_minus(a,b:tpoint):tpoint;
var c:tpoint;
begin
c.x:=a.x-b.x;
c.y:=a.y-b.y;
vector_minus:=c;
end;

function vector_plus(a,b:tpoint):tpoint;
var c:tpoint;
begin
c.x:=a.x+b.x;
c.y:=a.y+b.y;
vector_plus:=c;
end;

function vector_mul(a:tpoint;k:double):tpoint;
var c:tpoint;
begin
c.x:=a.x*k;
c.y:=a.y*k;
vector_mul:=c;
end;

function vector_length(a:tpoint):double;
begin
 vector_length:=sqrt(a.x*a.x+a.y*a.y);
end;



procedure TForm1.FormCreate(Sender: TObject);
var q:integer;
    u:double;
begin
tick:=0;
ulomok[0].pos.x:=350;
ulomok[0].pos.y:=SIZE/2;


image1.picture.Bitmap.width:=SIZE;
image1.picture.Bitmap.height:=SIZE;
image1.picture.bitmap.canvas.ellipse(
trunc(ulomok[0].pos.x) -2,trunc(ulomok[0].pos.y) -2,
trunc(ulomok[0].pos.x) +2,trunc(ulomok[0].pos.y) +2);

FOR q:=1 to ULOMKOV do
  begin
    u:=(q*2*Pi)/ULOMKOV;
    ulomok[q].pos.x:=startx;
    ulomok[q].pos.y:=starty;
    ulomok[q].v.x:=sin(u)*startv;
    ulomok[q].v.y:=cos(u)*startv;
  end;

end;

procedure TForm1.Button1Click(Sender: TObject);
begin
timer1.enabled:=not timer1.enabled;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var q:longint;
    a:tpoint;

    function tqr(x:double):double;
    begin
     tqr:=x*x*x;
    end;
begin
inc(tick);

for q:=1 to ULOMKOV do
  begin
//      image1.Picture.Bitmap.canvas.pixels[trunc(ulomok[q].pos.x),
//    trunc(ulomok[q].pos.y)]:=clWhite;
    a:=vector_minus(ulomok[0].pos,ulomok[q].pos);
    ulomok[q].v:=vector_plus(ulomok[q].v,
    vector_mul(a,DT*F_konst/tqr(vector_length(a))));

    ulomok[q].pos:=vector_plus(ulomok[q].pos,
    vector_mul(ulomok[q].v,DT)
    );
    //if (tick mod 10=0) then
    image1.Picture.Bitmap.canvas.pixels[trunc(ulomok[q].pos.x),
    trunc(ulomok[q].pos.y)]:=(q)+(q*q*q)*256+(q*q*q*q*q)*65536;

  end;


end;

end.
