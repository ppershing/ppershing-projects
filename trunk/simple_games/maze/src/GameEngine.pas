unit GameEngine;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs,Math,libudp,messagequeue;

type
  vector=record
     a,b,c: real;
    end;
TTransform = class(TObject)
private
    a : array[1..10,1..9] of real;
    aa: integer;
    constructor Create;
    function mv(x,y:real):vector; {vytvory vektor}
    function nvm(vek:vector;a,b,c,d,e,f,g,h,i:real):vector;
public
    procedure add(p1,p2,p3,p4,p5,p6,p7,p8,p9:real);
    procedure delete;
    function tran(x,y:integer):Tpoint;
  //  function otoc(x,y:longint;uhol:real):Tpoint;
end;

THrac = class(TObject)
  private
     tr : TTransform;
     x,y : integer;
     body : array[1..8] of Tpoint;
     bmp: TBitmap;
  public
    constructor Create(xx,yy:integer;cesta:string);
    destructor  Zmaz;
    procedure Kresli(c: TCanvas);
end;
THra = class
 private
    tr : TTransform;
    uhol : real;
    up,down,left,right : boolean;
    N,M,cx,cy : integer;
    mapa : array[1..1000,1..1000] of integer;
    kade : array[1..1000,1..1000] of integer;
    H: THrac;
    qx,qy : array[1..1000] of integer;
    qq,sx,sy : longint;
    cheat : boolean;
    bmpback: TBitmap;
    poc, pocfaz, skore: integer;
    function prienik(x,y:integer):boolean;
    procedure mlineto(c:TCanvas;z:Tpoint);
    procedure mmoveto(c:TCanvas;z:Tpoint);
  public
    maxscorelevels : longint;
    maxscoreCas : array[0..100] of TDateTime;
    maxscoreNick: array[0..100] of String;
    t1,t2,diff : TDateTime;
    stop: boolean;
    Level:string; {nazon suboru a zaroven urvne v ktorej sa nachadzam}
    c: TCanvas;
    procedure kresli;
    procedure LoadScore;
    constructor Create(llive,sscore:integer;cc: TCanvas);
    destructor  Zmaz;
    procedure LoadLevel(subor:string);
    procedure SetScore(ktore:longint;cas:TDateTime;nick:string);
    procedure tik(var ok: boolean);
    procedure stlac(aka:char;je:boolean);
    procedure reset_game;

  end;

const
  u45 = pi/4;
  hs = 2;

implementation
uses unit1;
{ THrac }


constructor TTransform.Create;begin aa:=0;end;
procedure TTransform.add(p1,p2,p3,p4,p5,p6,p7,p8,p9:real);begin
inc(aa);
a[aa,1]:=p1;a[aa,2]:=p2;a[aa,3]:=p3;
a[aa,4]:=p4;a[aa,5]:=p5;a[aa,6]:=p6;
a[aa,7]:=p7;a[aa,8]:=p8;a[aa,9]:=p9;
end;
procedure TTransform.delete;begin aa:=0;end;

function TTransform.mv(x,y:real):vector;begin mv.a:=x;mv.b:=y;mv.c:=1;end;
function TTransform.nvm(vek:vector;a,b,c, d,e,f,  g,h,i:real):vector;begin
 nvm.a:=vek.a*a+vek.b*d+vek.c*g;
 nvm.b:=vek.a*b+vek.b*e+vek.c*h;
 nvm.c:=vek.a*c+vek.b*f+vek.c*i;
end;
function TTransform.tran(x,y:integer):Tpoint;
var i:longint;
    pom : vector;
begin
  pom:=mv(x,y);
  for i:=1 to aa do pom:=nvm(pom,a[i,1],a[i,2],a[i,3],a[i,4],a[i,5],a[i,6],a[i,7],a[i,8],a[i,9]);
  tran.x:=round(pom.a);
  tran.y:=round(pom.b);
 // transform:=nvm(nvm(nvm(nvm(mv(x,y),1,0,0,0,1,0,-200,-200,1),cos(uhol),sin(uhol),0,-sin(uhol),cos(uhol),0,0,0,1),2,0,0,0,1,0,0,0,1),1,0,0,0,1,0,200,200,1);
end;
{
function TTransform.otoc(x,y:longint;uhol:real):Tpoint;
begin
  otoc.x:=round(transform(x,y,uhol).a);
  otoc.y:=round(transform(x,y,uhol).b);
end;
 }

procedure THra.mlineto(c:Tcanvas;z:tpoint);begin c.LineTo(z.X,z.y);end;
procedure THra.mmoveto(c:Tcanvas;z:tpoint);begin c.MoveTo(z.X,z.y);end;


constructor THrac.Create(xx,yy:Integer;cesta:string);
begin
  tr  :=TTransform.Create;
  tr.add(1,0,0,0,1,0,-200,-200,1);
  tr.add(cos(0),sin(0),0,-sin(0),cos(0),0,0,0,1);
  tr.add(2,0,0,0,1,0,0,0,1);
  tr.add(1,0,0,0,1,0,200,200,1);
  bmp := TBitmap.Create;
 { bmp.LoadFromFile(cesta);
  bmp.Transparent := true;}
  x:=xx;
  y:=yy;
  body[1].x:=round(200);body[1].y:=round(200-hs);
  body[3].x:=round(200+hs);body[3].y:=round(200);
  body[5].x:=round(200);body[5].y:=round(200+hs);
  body[7].x:=round(200-hs);body[7].y:=round(200);
  body[2].x:=round(200+Cos(u45)*hs);body[2].y:=round(200-Sin(u45)*hs);
  body[4].x:=round(200+Cos(u45)*hs);body[4].y:=round(200+Sin(u45)*hs);
  body[6].x:=round(200-Cos(u45)*hs);body[6].y:=round(200+Sin(u45)*hs);
  body[8].x:=round(200-Cos(u45)*hs);body[8].y:=round(200-Sin(u45)*hs);

end;

procedure THrac.Kresli(c: TCanvas);
var bodynew : array[1..8] of Tpoint;
    i: longint;
begin
 // c.Draw(x-10,y-10, bmp);
 c.Brush.Color := clBlue;
 c.pen.Color:=clBlue;
// c.Ellipse(x-hs,y-hs,x+hs,y+hs);
// c.Ellipse(200-hs,200-hs,200+hs,200+hs);
 for i:=1 to 8 do bodynew[i]:=tr.tran(body[i].x,body[i].y);
 c.Polygon(bodynew);
end;

destructor THrac.Zmaz;
begin
bmp.Free;
end;


{ THra }


procedure Thra.LoadLevel(subor:string);
var t:text;
    cesta,hraccesta:string;
    NN,i,j,somx,somy:integer;
    ch:char;
begin
  up:=false;
  down:=false;
  left:=false;
  right:=false;

  uhol:=0;
  Level:=subor;
  if Not(FileExists(Level+'.txt')) then Level:='level0';
  AssignFile(t, Level+'.txt');
  Reset(t);
  Readln(t,cesta);
  bmpback := TBitmap.Create;
  bmpback.LoadFromFile(cesta);
  readln(t, NN);
  readln(t, cesta);
  readln(t,hraccesta);
  readln(t,N); {pocet stlpcou}
  readln(t,M); {pocet riadkou}
  for i:=1 to M do begin
    for j:=1 to N do begin
      kade[j,i]:=0;
      if j<>n then read(t,ch) else readln(t,ch);
      mapa[j,i]:=0;
      case ch of
        'P' : begin H:=Thrac.Create((j-1)*20+10,(i-1)*20+10,hraccesta);mapa[j,i]:=0;end;
        'C' : begin cx:=j;cy:=i;mapa[j,i]:=0;end;
        '.' : mapa[j,i]:=0;
        'B' : mapa[j,i]:=1;
      end;
    end;
  end;
  qq:=1;
  qx[1]:=cx;qy[1]:=cy;
  while qq>0 do begin
    somx:=qx[qq];somy:=qy[qq];dec(qq);
    if (somx>1)and(mapa[somx-1,somy]=0)and(kade[somx-1,somy]=0) then begin
         inc(qq);qx[qq]:=somx-1;qy[qq]:=somy;kade[somx-1,somy]:=2;end;
    if (somx<N)and(mapa[somx+1,somy]=0)and(kade[somx+1,somy]=0) then begin
         inc(qq);qx[qq]:=somx+1;qy[qq]:=somy;kade[somx+1,somy]:=4;end;
    if (somy>1)and(mapa[somx,somy-1]=0)and(kade[somx,somy-1]=0) then begin
         inc(qq);qx[qq]:=somx;qy[qq]:=somy-1;kade[somx,somy-1]:=3;end;
    if (somy<M)and(mapa[somx,somy+1]=0)and(kade[somx,somy+1]=0) then begin
         inc(qq);qx[qq]:=somx;qy[qq]:=somy+1;kade[somx,somy+1]:=1;end;
  end;
  CloseFile(t);
  kresli;
end;
procedure Thra.LoadScore;var inn:text;i:longint;begin
cheat:=false;
assign(inn,'score.txt');reset(inn);readln(inn,maxscorelevels);
for i:=0 to maxscorelevels do begin
readln(inn,maxscoreCas[i]);
readln(inn,maxscoreNick[i]);
end;
close(inn);
end;
constructor THra.Create(llive,sscore:integer;cc: TCanvas);
begin
 uhol:=0;
 cheat:=false;
 h:=thrac.create(0,0,'');
 tr:=TTransform.Create;
 tr.add(1,0,0,0,1,0,-200,-200,1);
 tr.add(cos(uhol),sin(uhol),0,-sin(uhol),cos(uhol),0,0,0,1);
 tr.add(2,0,0,0,1,0,0,0,1);
 tr.add(1,0,0,0,1,0,200,200,1);
 c:=cc;
 stop:=true;
 up:=false;down:=false;left:=false;right:=false;
 LoadScore;
end;


destructor  THra.Zmaz;
var i:integer;
  begin
  H.Free;
  bmpback.Free;
end;



function Thra.prienik(x,y:Integer):boolean;
var r:integer;
    je : boolean;
function stvorec(xx,yy:integer):integer;
begin
stvorec:=mapa[(xx div 20)+1,(yy div 20)+1];
end;
begin
//  (x div 20)+1,((y-6-kolko) div 20)+1]
   je:=false;
   if stvorec(x+hs,y)=1 then je:=true;
   if stvorec(x-hs,y)=1 then je:=true;
   if stvorec(x,y+hs)=1 then je:=true;
   if stvorec(x,y-hs)=1 then je:=true;
   if stvorec(Round(x+(hs*Cos(u45))),Round(y+(hs*Sin(u45))))=1 then je:=true;
   if stvorec(Round(x-(hs*Cos(u45))),Round(y+(hs*Sin(u45))))=1 then je:=true;
   if stvorec(Round(x+(hs*Cos(u45))),Round(y-(hs*Sin(u45))))=1 then je:=true;
   if stvorec(Round(x-(hs*Cos(u45))),Round(y-(hs*Sin(u45))))=1 then je:=true;
   prienik:=je;
end;

procedure THra.kresli;
var i,j,xx,yy,vx,vy: integer;
 body : array[1..4] of Tpoint;
 tm : vector;
// q : real;
begin
 // q:=uhol;
  c.Brush.Color := clWhite;
  c.FillRect(Rect(0, 0, 400, 400));
  c.Brush.Color := clYellow;
  c.Pen.Color := clYellow;
  c.Draw(0,0,bmpback);

   c.Brush.Color := clBlack;
   xx:=(H.x div 20)+1;
   yy:=(H.y div 20)+1;
   vx:=200-H.x;
   vy:=200-H.y;

   tr.delete;tr.add(1,0,0,0,1,0,-200,-200,1);tr.add(cos(uhol),sin(uhol),0,-sin(uhol),cos(uhol),0,0,0,1);
   tr.add(2,0,0,0,1,0,0,0,1);tr.add(1,0,0,0,1,0,200,200,1);

   for i:=1 to M do
     for j:=1 to N do
       if (abs(j-xx)<=15)and(abs(i-yy)<15) then
       if mapa[j,i]=1 then begin
          body[1]:=tr.tran((j-1)*20+vx,(i-1)*20+vy);
          body[2]:=tr.tran((j)*20+vx,(i-1)*20+vy);
          body[3]:=tr.tran((j)*20+vx,(i)*20+vy);
          body[4]:=tr.tran((j-1)*20+vx,(i)*20+vy);
          c.Brush.Color := (j-xx)*10+128+ +256*128+256*((i-yy)*10);
          c.Pen.Color   :=   clYellow;
          c.Polygon(body);
         //c.FillRect(Rect((j-1)*20+vx,(i-1)*20+vy,(j)*20+vx,(i)*20+vy));
       end;
c.Brush.Color := clBlack;
c.Pen.Color := clBlack;
if cheat and (not cheat) then
for i:=1 to M do
  for j:=1 to N do if mapa[j,i]=0 then begin
    case kade[j,i] of
      1: begin
           mmoveto(c,tr.tran((j-1)*20+vx+10,(i-1)*20+vy+15));
           mlineto(c,tr.tran((j-1)*20+vx+10,(i-1)*20+vy+5));
           mlineto(c,tr.tran((j-1)*20+vx+7,(i-1)*20+vy+8));
           mmoveto(c,tr.tran((j-1)*20+vx+10,(i-1)*20+vy+5));
           mlineto(c,tr.tran((j-1)*20+vx+13,(i-1)*20+vy+8));
         end;
      2: begin
           mmoveto(c,tr.tran((j-1)*20+vx+5,(i-1)*20+vy+10));
           mlineto(c,tr.tran((j-1)*20+vx+15,(i-1)*20+vy+10));
           mlineto(c,tr.tran((j-1)*20+vx+12,(i-1)*20+vy+7));
           mmoveto(c,tr.tran((j-1)*20+vx+15,(i-1)*20+vy+10));
           mlineto(c,tr.tran((j-1)*20+vx+12,(i-1)*20+vy+13));
         end;
      3: begin
           mmoveto(c,tr.tran((j-1)*20+vx+10,(i-1)*20+vy+5));
           mlineto(c,tr.tran((j-1)*20+vx+10,(i-1)*20+vy+15));
           mlineto(c,tr.tran((j-1)*20+vx+7,(i-1)*20+vy+12));
           mmoveto(c,tr.tran((j-1)*20+vx+10,(i-1)*20+vy+15));
           mlineto(c,tr.tran((j-1)*20+vx+13,(i-1)*20+vy+12));
         end;
      4: begin
           mmoveto(c,tr.tran((j-1)*20+vx+15,(i-1)*20+vy+10));
           mlineto(c,tr.tran((j-1)*20+vx+5,(i-1)*20+vy+10));
           mlineto(c,tr.tran((j-1)*20+vx+8,(i-1)*20+vy+7));
           mmoveto(c,tr.tran((j-1)*20+vx+5,(i-1)*20+vy+10));
           mlineto(c,tr.tran((j-1)*20+vx+8,(i-1)*20+vy+13));
         end;
    end;
end;
 if H<>nil then H.kresli(c);
 { for i :=0 to high(F) do
    F[i].kresli(c);       }
end;

procedure THra.SetScore(ktore:longint;cas:TDateTime;nick:string);
var inn:text;
    i : longint;
begin
 if ktore>maxscorelevels then begin
    maxscorelevels:=ktore;
    maxscoreCas[ktore]:=cas;
    maxscoreNick[ktore]:=nick;
 end;
if  maxscoreCas[ktore]>cas then begin
    maxscoreCas[ktore]:=cas;
    maxscoreNick[ktore]:=nick;
 end;
 assign(inn,'score.txt');
 rewrite(inn);
 writeln(inn,maxscorelevels);
 for i:=0 to maxscorelevels do begin
    writeln(inn,maxscoreCas[i]);
    writeln(inn,maxscoreNick[i]);
 end;
 close(inn);
end;

procedure THra.tik(var ok: boolean);
var
  i,kolko,kmax,levelnum:integer;
  subor,s:string;
  data:tudpdata;
begin if not stop then begin
kmax:=1;
kolko:=0;while (kolko<kmax)and(up)and(not(prienik(H.x,H.y-kolko-1))) do inc(kolko);H.y:=H.y-kolko;
kolko:=0;while (kolko<kmax)and(down)and(not(prienik(H.x,H.y+kolko+1))) do inc(kolko);H.y:=H.y+kolko;
kolko:=0;while (kolko<kmax)and(left)and(not(prienik(H.x-kolko-1,H.y))) do inc(kolko);H.x:=H.x-kolko;
kolko:=0;while (kolko<kmax)and(right)and(not(prienik(H.x+kolko+1,H.y))) do inc(kolko);H.x:=H.x+kolko;
uhol:=uhol+0.005;
kresli;
if ((H.x div 20)+1=cx)and((H.y div 20)+1=cy) then begin
         stop:=true;
         T2:=now;
         diff:=T2-T1;
    {ShowMessage('Level Completed!!!');}
         up:=false;down:=false;left:=false;right:=false;
         Subor:=copy(Level,6,Length(Level));   {Nastavenie nazvu suboru}
         levelnum:=strtoint(subor)+1;
         Subor:='level'+inttostr(levelnum);
         LoadLevel(Subor);
         if ((diff)<maxScoreCas[levelnum-1])or(levelnum-1>maxscorelevels)  then S:='Podarilo sa ti prekonat rekord :-)'+#13 else s:='';{S:='Nepodarilo sa ti prekonat rekord :-('+#13;}
         SetScore(levelnum-1,diff,'Maty');
         s:=s+'Level '+inttostr(levelnum-1)+' si presiel za '+FormatDateTime('hh:nn:ss', diff)+#13+'Spustit dalsi?';
         data.length:=2;
         data.data[0]:=99;
         data.data[1]:=levelnum-1;
         unit1.udpcon.send(data);
         if MessageDlg(s, mtConfirmation, [mbYes, mbNo], 0) = mrYes then reset_game;


end;
end;end;

procedure THra.stlac(aka:char;je:boolean);
begin
 case aka of
   'W','w' :  up:=je;
   'S','s' :  down:=je;
   'A','a' :  left:=je;
   'D','d' :  right:=je;
   'C','c' :  cheat:=je;
 end;
end;

procedure THra.reset_game;
begin
 T1:=now;
 up:=false;down:=false;left:=false;right:=false;
 stop:=true;
 LoadLevel(Level);
 stop:=false;
end;

end.
