unit nit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls,math, ComCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Timer1: TTimer;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    ListBox1: TListBox;
    TrackBar1: TTrackBar;
    Label3: TLabel;
    CheckBox1: TCheckBox;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

const sirka=500;
const dlzka=800;
const usekov=30;
const hmotnost=1;
const usek_dlzka=dlzka/usekov;
const usek_hmotnost=hmotnost/usekov;

const gravitacia=1.5;
const zrychlenie=0.2;
const odpor_vzduchu=0.02;
const rate=1;


type tvector=record
x,y:extended;
end;

var
  Form1: TForm1;
  useky,tmpuseky:array[0..usekov] of tvector;
  useky_rychlost:array[0..usekov] of tvector;
  tick:longint;
  tuhost:extended;



implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
begin
timer1.enabled:=not timer1.enabled;
end;

procedure TForm1.FormCreate(Sender: TObject);
var q:integer;
begin
image1.Picture.Bitmap.Width:=dlzka+50;
image1.picture.bitmap.height:=dlzka+50;

for q:=0 to usekov do
  begin
   useky[q].x:=sirka/usekov*q+25;
   useky[q].y:=25+(q*sirka/usekov)*(sirka-q*sirka/usekov)/500;
   listbox1.Items.Add('###');
  end;
listbox1.items.add('---');

end;

function vector_minus(a,b:tvector):tvector;
var res:tvector;
begin
 res.x:=a.x-b.x;
 res.y:=a.y-b.y;
 vector_minus:=res;
end;

function vector_add(a,b:tvector):tvector;
var res:tvector;
begin
 res.x:=a.x+b.x;
 res.y:=a.y+b.y;
 vector_add:=res;
end;


function vector_length(v:tvector):extended;
begin
 vector_length:=power(v.x*v.x+v.y*v.y,0.5);
end;

function vector_mul(v:tvector;l:extended):tvector;
var res:tvector;
begin
  res.x:=v.x*l;
  res.y:=v.y*l;
  vector_mul:=res;
end;

function min(a,b:extended):extended;
begin
 if a<b then min:=a else min:=b;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var q:integer;
    sila:tvector;
    tmp:tvector;



  procedure useky_draw;
  var q:integer;
      sy,a:extended;
      qq:extended;
  begin
  qq:=1+trackbar1.Position/100;
  label3.caption:='krivka: x^'+floattostrf(qq,fffixed,10,2);
  image1.picture.bitmap.canvas.Brush.Color:=clWhite;
  image1.picture.bitmap.canvas.fillrect(rect(0,0,
   image1.picture.bitmap.width,image1.picture.bitmap.height));
   image1.picture.bitmap.canvas.pen.color:=clblack;
   for q:=0 to usekov-1 do
     begin
       image1.picture.Bitmap.canvas.ellipse(trunc(useky[q].x-3),
        trunc(useky[q].y-3),trunc(useky[q].x+3),trunc(useky[q].y+3));
       image1.picture.Bitmap.canvas.ellipse(trunc(useky[q+1].x-3),
        trunc(useky[q+1].y-3),trunc(useky[q+1].x+3),trunc(useky[q+1].y+3));
    image1.picture.bitmap.canvas.MoveTo(trunc(useky[q].x),trunc(useky[q].y));
    image1.picture.bitmap.canvas.lineTo(trunc(useky[q+1].x),trunc(useky[q+1].y));
     end;

    image1.picture.bitmap.canvas.pen.color:=clred;


    sy:=useky[usekov div 2].y;
    a:=(sy-25)/power(sirka/2,QQ);
    for q:=0 to 100 do
      begin
       image1.picture.bitmap.canvas.moveto(trunc(25+q*sirka/100),
       trunc(sy-power(abs(q*sirka/100-sirka/2),QQ)*a));
     image1.picture.bitmap.canvas.lineto(trunc(25+(q+1)*sirka/100),
       trunc(sy-power(abs((q+1)*sirka/100-sirka/2),QQ)*a));

      end;


  end;


begin
inc(tick);
tuhost:=min(power(1.01,tick)/10,3);
for q:=0 to usekov do tmpuseky[q]:=useky[q];



if (checkbox1.checked) then
 begin
 tmpuseky[0].x:=tmpuseky[1].x-usek_dlzka;
 tmpuseky[0].y:=tmpuseky[1].y;
 useky[0]:=tmpuseky[0];
 end;

for q:=1 to usekov-1 do
  begin
   sila.x:=0; sila.y:=usek_hmotnost*gravitacia; // gravitacia


   tmp:=vector_minus(tmpuseky[q-1],tmpuseky[q]);

   if (tick mod rate=0) then listbox1.items[q-1]:=  floattostrf(
    (vector_length(tmp)-usek_dlzka),fffixed,10,8);

   sila:=vector_add(sila,vector_mul(tmp,
    (vector_length(tmp)-usek_dlzka)*tuhost/usek_dlzka));

   tmp:=vector_minus(tmpuseky[q+1],tmpuseky[q]);

    if (tick mod rate=0) then listbox1.items[q]:=  floattostrf(
    (vector_length(tmp)-usek_dlzka),fffixed,10,8);

   sila:=vector_add(sila,vector_mul(tmp,
    (vector_length(tmp)-usek_dlzka)*tuhost/usek_dlzka));


    sila:=vector_mul(sila,zrychlenie);
    useky_rychlost[q]:=vector_mul(useky_rychlost[q],1-odpor_vzduchu);
    useky_rychlost[q]:=vector_add(useky_rychlost[q],sila);

   useky[q]:=vector_add(useky[q],useky_rychlost[q]);

  end;

if (tick mod rate=0) then
 begin
  label1.caption:='tuhost: '+floattostrf(tuhost,fffixed,10,4);
  label2.caption:='uhol: '+floattostrf(
   arctan(-tmp.x/tmp.y)*180/Pi,fffixed,10,4);
  useky_draw;
 end;
{
 if (tick mod 1000=0) then
  begin
   memo1.lines.add('sirka '+inttostr(
   trunc(useky[usekov].x-useky[0].x))+'  napetie '+floattostrf(
   vector_length(tmp)-usek_dlzka,fffixed,10,4)
    +' '+label2.caption);

    useky[usekov].x:=useky[usekov].x-10;
  end;
  }
end;

end.
