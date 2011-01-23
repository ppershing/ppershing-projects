unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    Image1: TImage;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

const SIZE=100;
const koef=1.5;

type ld=extended;

var
  Form1: TForm1;
  data:array[0..SIZE,0..SIZE] of ld;
  fixed:array[0..SIZE,0..SIZE] of boolean;



implementation

{$R *.DFM}

procedure tick;
var q,w:integer;
    tmp:ld;
    pocet:integer;
    p:integer;

begin

//  for q:=0 to SIZE do
// for w:=0 to SIZE do
for p:=0 to SIZE*SIZE do
 begin
  q:=random(SIZE);
  w:=random(SIZE);

 if not fixed[q,w] then
  begin
   pocet:=0;
   tmp:=0;
   if (q>0) then begin inc(pocet); tmp:=tmp+data[q-1,w]; end;
   if (q<SIZE-1) then begin inc(pocet); tmp:=tmp+data[q+1,w]; end;
   if (w>0) then begin inc(pocet); tmp:=tmp+data[q,w-1]; end;
   if (w<SIZE-1) then begin inc(pocet); tmp:=tmp+data[q,w+1]; end;
   tmp:=tmp/pocet;
   data[q,w]:=koef*tmp+(1-koef)*data[q,w];

  end;
 end;
end;

procedure zobraz;
var q,w:integer;
    b:tbitmap;
    p:pbytearray;
begin
 b:=tbitmap.create;
 b.width:=SIZE;
 b.height:=SIZE;
 b.PixelFormat:=pf24bit;

 for q:=0 to SIZE-1 do
  begin
   p:=b.scanline[q]; //form1.Image1.Picture.Bitmap.ScanLine[q];
   for w:=0 to SIZE-1 do
    begin
      p[w*3]:=trunc(data[q,w]);
      p[w*3+1]:=trunc(data[q,w]);
      p[w*3+2]:=trunc(data[q,w]);
//    form1.image1.picture.bitmap.Canvas.pixels[q,w]:=
//     (1+256+256*256)*trunc(data[q,w]);
    end;

  end;

  form1.image1.picture.bitmap.Canvas.stretchDraw(
   rect(0,0,SIZE*4,SIZE*4),b);
  b.free;

end;

procedure TForm1.Button1Click(Sender: TObject);
var q:integer;

begin
button1.enabled:=false;
q:=0;
repeat
tick;
inc(q);
if (q mod 500 = 0) then
 begin
 zobraz;
 application.ProcessMessages;
 end;
until false;
end;

procedure TForm1.FormCreate(Sender: TObject);
var q,w:integer;
begin
for q:=0 to SIZE do
 for w:=0 to SIZE do
  begin
  fixed[q,w]:=false;
  data[q,w]:=130;
  end;


for q:=SIZE div 3 to (SIZE div 3)*2 do
 begin
  fixed[q,30]:=true;
  data[q,30]:=255;
 end;

for q:=SIZE div 3 to (SIZE div 3)*2 do
 begin
  fixed[q,SIZE-30]:=true;
  data[q,SIZE-30]:=0;
 end;


for q:=0 to 5 do
 for w:=0 to 5 do
  begin
   fixed[q+30,w+50]:=true;
   data[q+30,w+50]:=255;

   fixed[q+70,w+50]:=true;
   data[q+70,w+50]:=0;


  end;

form1.image1.Picture.bitmap.Width:=SIZE*4;
form1.image1.Picture.bitmap.height:=SIZE*4;

end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  halt;
end;

end.
