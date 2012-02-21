unit MainForm;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,math, ExtCtrls, ComCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    load_button: TButton;
    save_button: TButton;
    file_edit: TEdit;
    sk_edit: TEdit;
    en_edit: TEdit;
    add_button: TButton;
    slovka_list: TListBox;
    slovko_label: TLabel;
    answer: TEdit;
    Memo1: TMemo;
    edit_button: TButton;
    Timer1: TTimer;
    image: TImage;
    delete_button: TButton;
    sort_opt: TRadioGroup;
    randomness: TTrackBar;
    sort_box: TCheckBox;
    OpenDialog1: TOpenDialog;
    new_button: TButton;
    clear_button: TButton;
    clear_history_button: TButton;
    load_merge: TCheckBox;
    zobraz_listbox: TCheckBox;
    delete_duplicates: TButton;
    merge_add_text: TEdit;
    procedure add_buttonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure save_buttonClick(Sender: TObject);
    procedure sk_editKeyPress(Sender: TObject; var Key: Char);
    procedure naplnlistbox;
    procedure en_editKeyPress(Sender: TObject; var Key: Char);
    procedure load_buttonClick(Sender: TObject);
    procedure skusaj;
    procedure answerKeyPress(Sender: TObject; var Key: Char);
    procedure slovka_listClick(Sender: TObject);
    procedure edit_buttonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Timer1Timer(Sender: TObject);
    procedure delete_buttonClick(Sender: TObject);
    procedure sort_optClick(Sender: TObject);
    procedure en_editChange(Sender: TObject);
    procedure file_editDblClick(Sender: TObject);
    procedure new_buttonClick(Sender: TObject);
    procedure clear_buttonClick(Sender: TObject);
    procedure clear_history_buttonClick(Sender: TObject);
    procedure delete_duplicatesClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

type tword=record
  sk,en:string;
  id:longint;
  stat_ok,stat_all,stat_last:longint;
  stat_hist:extended;
  rating:longint;
  pravdepodobnost:extended;
end;


const MAX_SLOVIEK=100000;
      MAX_SKUSAJ_POKUSOV=10;
      history_const=0.3;
      INI_FILE='aj.ini';
var
  Form1: TForm1;
  slovka:array[1..MAX_SLOVIEK] of tword;
  sloviek:integer;
  f:textfile;
  skusane_slovko:integer;
  skusanie:longint;

implementation

{$R *.lfm}

function get_ok(slovko:tword):real;
begin
  get_ok := slovko.stat_hist;
end;

function get_rating(slovko:tword):real;
var exponent:real;
begin
  exponent :=  1.5 + sqrt(skusanie - slovko.stat_last) / 15;
  get_rating := power(get_ok(slovko), exponent);
end;

procedure save_ini;
var f:textfile;
    s:string;
    i:integer;
begin
  assignfile(f,INI_FILE);
  rewrite(f);
  writeln(f,form1.file_edit.text);
  writeln(f,form1.sort_opt.ItemIndex);
  writeln(f,form1.randomness.Position);
  writeln(f,integer(form1.sort_box.checked));
  writeln(f,integer(form1.zobraz_listbox.checked));
  writeln(f,integer(form1.load_merge.checked));

  closefile(f);
end;

procedure load_ini;
var f:textfile;
    s:string;
    i:integer;
begin
  assignfile(f, INI_FILE);
  reset(f);

  readln(f,s);
  form1.file_edit.Text := s;

  readln(f,i);
  form1.sort_opt.ItemIndex := i;

  readln(f,i);
  form1.randomness.Position := i;

  readln(f,i);
  form1.sort_box.checked := bool(i);

  readln(f,i);
  form1.zobraz_listbox.checked := bool(i);

  readln(f,i);
  form1.load_merge.checked := bool(i);

  closefile(f);
end;

function skusaj_slovko_accept(pokus:integer):extended;
var    koef:extended;
begin
 koef:=exp(form1.randomness.position/10)+1;
 if (pokus>=MAX_SKUSAJ_POKUSOV) then result:=10000
   // maximalny pocet pokusov -akceptujeme kazde slovko
  else
 result:=power(koef,pokus)-1; // vrati maximalny akceptovatelny rating
end;

procedure tform1.skusaj;
var q:integer;
    mam:boolean;
    pokus:integer;
    oldq:integer;
begin
if (sloviek=0) then exit;
q:=-1;
mam:=false;
pokus:=1;
while (not mam) do
  begin
    q:=random(sloviek)+1;
    if ( get_rating(slovka[q])<=skusaj_slovko_accept(pokus)) then
      begin
       mam:=true;
      end;

     inc(pokus);
  end;
//  oldq:=skusane_slovko;
skusane_slovko:=q;
slovko_label.caption:=slovka[q].sk;
answer.text:='';
answer.setfocus;

memo1.lines.add(inttostr(skusanie)+' slovko '+slovka[q].sk+' -> pokusov: '
+inttostr(pokus)+' (limit '+
inttostr(slovka[q].rating)+' ( '+inttostr(trunc(slovka[q].stat_hist*100))+' ) , '+
inttostr(trunc(1000*skusaj_slovko_accept(pokus)))+')');
image.Picture.bitmap.Canvas.Pen.color:=cllime;
image.picture.bitmap.canvas.MoveTo(trunc(q/sloviek*image.picture.bitmap.Width),0);
image.picture.bitmap.canvas.lineTo(trunc(q/sloviek*image.picture.bitmap.width),image.picture.bitmap.height);
{
image.Picture.bitmap.Canvas.Pen.color:=clgray;
image.picture.bitmap.canvas.MoveTo(trunc(oldq/sloviek*image.picture.bitmap.Width),0);
image.picture.bitmap.canvas.lineTo(trunc(oldq/sloviek*image.picture.bitmap.width),image.picture.bitmap.height);
 }

end;

function compare(a,b:tword):integer;
var cmp:integer;
begin
 case form1.sort_opt.itemindex of
  0: if (a.en<b.en) then cmp:=-1 else if (a.en=b.en) then cmp:=0 else cmp:=1;
  1: if (a.sk<b.sk) then cmp:=-1 else if (a.sk=b.sk) then cmp:=0 else cmp:=1;
  2: if (a.rating<b.rating) then cmp:=-1 else  if (a.rating=b.rating) then cmp:=0 else cmp:=1;
  3: if (get_ok(a)<get_ok(b)) then cmp:=-1 else
  if (get_ok(a)=get_ok(b)) then cmp:=0 else
  cmp:=1;
  4: if (a.stat_last<b.stat_last) then cmp:=-1 else  if (a.stat_last=b.stat_last) then cmp:=0 else cmp:=1;
 end;

if (cmp=0) then
 begin
  if (a.id<b.id) then cmp:=-1 else if (a.id=b.id) then cmp:=0 else cmp:=1;
 end;

compare:=cmp;

end;

procedure slovka_sort(l,r:integer);
var i,j:integer;
     med:tword;
     tmp:tword;
begin
 if (not form1.sort_box.checked) then exit;
i:=l;
j:=r;
med:=slovka[(l+r)div 2];
//med:=slovka[l];
repeat
 while (compare(slovka[i],med)<0) do inc(i);
 while (compare(slovka[j],med)>0) do dec(j);
 if (i<=j) then
  begin
   tmp:=slovka[i]; slovka[i]:=slovka[j]; slovka[j]:=tmp;
   inc(i);
   dec(j);
  end;
until i>j;
if (l<j) then slovka_sort(l,j);
if (i<r) then slovka_sort(i,r);
end;

procedure zrataj_pravdepodobnosti;
var q,w,e:integer;
    data:array[1..MAX_SKUSAJ_POKUSOV] of integer;
    pokusov:integer;
    rat:extended;
    p,pp:extended;
begin

pokusov:=0;
repeat
 inc(pokusov);
 data[pokusov]:=0;
for q:=1 to sloviek do
 begin
  rat:=get_rating(slovka[q]);
 if (rat<=skusaj_slovko_accept(pokusov))
  then
   begin
    inc(data[pokusov]);
   end;
 end;
until data[pokusov]=sloviek;

for q:=1 to sloviek do
 begin
  w:=1;
    rat:=get_rating(slovka[q]);

  while (rat>skusaj_slovko_accept(w)) do inc(w);
  p:=1;
  for e:=1 to w-1 do p:=p*(1-data[e]/sloviek);
  pp:=0;
  for e:=w to pokusov do
   begin
    pp:=pp+p*data[e]/sloviek;
    p:=p*(1-data[e]/sloviek);
   end;
  slovka[q].pravdepodobnost:=pp;


 end;

end;


procedure tform1.naplnlistbox;
var q,w,w2:integer;
    bmp:tbitmap;
    width,height:integer;
    sumrating,tmp:extended;
    max_all:integer;
    max_pravdep:extended;
begin
 sumrating:=0;
 max_pravdep:=0.0000001;// aby sme nedelili nulou
 max_all:=1;

 bmp:=image.picture.bitmap;
 bmp.canvas.brush.color:=clwhite;
 bmp.canvas.pen.color:=clwhite;
 width:=bmp.width;
 height:=bmp.height;
 bmp.canvas.Rectangle(0,0,width,height);
 bmp.canvas.pen.color:=clnavy;
 bmp.Canvas.Brush.Style:=bssolid;
 bmp.canvas.brush.color:=clnavy;

 if (sloviek=0) then exit;

  for q:=1 to sloviek do
   begin
    tmp:=get_rating(slovka[q]);
    sumrating:=sumrating+tmp;
    slovka[q].rating:=trunc(1000*tmp);
   end;

 slovka_sort(1,sloviek);
  memo1.lines.add(format('priemerny rating : %.2f ',[sumrating*1000/(sloviek+0.01)]));

 slovka_list.Items.Clear;
 if (zobraz_listbox.checked) then
  begin
  for q:=1 to sloviek do
  begin

   slovka_list.items.Add(format('%4d %.3d / %2d%% , %3d , %2d/%2d, %10s - %10s',[q,slovka[q].rating,
   trunc(100*get_ok(slovka[q])),
   skusanie-slovka[q].stat_last,
   slovka[q].stat_ok,slovka[q].stat_all,slovka[q].en,slovka[q].sk]));
  end;
 end;


 zrataj_pravdepodobnosti;
   // spocita pravdepodobnosti sloviek

 for q:=1 to sloviek do
  begin
  if (max_all<slovka[q].stat_all) then max_all:=slovka[q].stat_all;
  if (max_pravdep<slovka[q].pravdepodobnost) then max_pravdep:=slovka[q].pravdepodobnost;
  end;

 for q:=1 to sloviek  do // rating
   begin
    w:=trunc((q-1)*width/sloviek);

    w2:=max(trunc((q-1)*width/sloviek)+1,
         trunc(q*width/sloviek));



    bmp.canvas.pen.color:=clnavy;
    bmp.canvas.Brush.color:=clnavy;

    bmp.canvas.Rectangle(w,height div 2,
         w2,height div 2-(slovka[q].rating*height div 2000));

//    bmp.canvas.pen.color:=cllime;
//    bmp.canvas.Brush.color:=cllime;
    bmp.canvas.pixels[w2,height div 2-trunc(slovka[q].pravdepodobnost/
          max_pravdep*height/2)]:=cllime;

//    Rectangle(w,height div 2,
//         w2,height div 2-trunc(slovka[q].pravdepodobnost/
//          max_pravdep*height/2));


   bmp.canvas.pen.color:=clgray;
   bmp.canvas.Brush.color:=clgray;

    bmp.canvas.Rectangle(w,height div 2,
      w2,trunc(height/2*(1+slovka[q].stat_all/max_all)));

   bmp.canvas.pen.color:=clred;
   bmp.canvas.Brush.color:=clred;

    bmp.canvas.Rectangle(w,height div 2,
      w2,trunc(height/2*(1-log10(1.06-get_ok(slovka[q])))));
   end;

   image.picture.bitmap:=bmp;

end;


procedure TForm1.add_buttonClick(Sender: TObject);
begin
inc(sloviek);
with slovka[sloviek] do
 begin
en:=en_edit.text;
sk:=sk_edit.text;
id:=sloviek;
stat_ok:=0;
stat_all:=0;
stat_last:=0;
stat_hist:=0;
rating:=0;
end;
en_edit.text:='';
sk_edit.text:='';


naplnlistbox;
skusaj;
sk_edit.setfocus;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  load_ini();
  image.Picture.Bitmap.width := image.Width;
  image.picture.bitmap.height := image.height;

  sloviek:=0;
  randomize();
end;

procedure TForm1.save_buttonClick(Sender: TObject);
var q:integer;
begin
assignfile(f,file_edit.text);
rewrite(f);
writeln(f,'ver.2.0');
writeln(f,sloviek);
 for q:=1 to sloviek do
   begin
     writeln(f,slovka[q].en);
     writeln(f,slovka[q].sk);
     writeln(f,slovka[q].id,' ',slovka[q].stat_ok,' ',slovka[q].stat_all,' ',
      slovka[q].stat_last,' ',slovka[q].rating);
     writeln(f,slovka[q].stat_hist:5:5);
     writeln(f);
   end;
closefile(f);
memo1.lines.add('SAVE >>>>>>>>>');
memo1.lines.add('<<<<<<<<<');

end;

procedure TForm1.sk_editKeyPress(Sender: TObject; var Key: Char);
begin
  if key=chr(13) then
  begin
    en_edit.setfocus();
  end;
end;

procedure TForm1.en_editKeyPress(Sender: TObject; var Key: Char);
begin
  if key=chr(13) then
  begin
    add_button.setfocus();
  end;
end;

procedure TForm1.load_buttonClick(Sender: TObject);
var q:integer;
    s:string;
    pocet:integer;
begin
if (not load_merge.checked) then skusanie:=0;

assignfile(f,file_edit.text);
reset(f);
readln(f,s);

if (not load_merge.checked) then sloviek:=0;
readln(f,pocet);
 for q:=1+sloviek to pocet+sloviek do
   begin
     readln(f,slovka[q].en);
     readln(f,slovka[q].sk);
     if (load_merge.checked) then
       begin
        slovka[q].sk:=slovka[q].sk+merge_add_text.Text;
       end;
       
     readln(f,slovka[q].id,slovka[q].stat_ok,slovka[q].stat_all,
      slovka[q].stat_last,slovka[q].rating);
      if (skusanie<slovka[q].stat_last) then skusanie:=slovka[q].stat_last;
     if (s>='ver.2.0') then readln(f,slovka[q].stat_hist) else
      slovka[q].stat_hist:=(slovka[q].stat_ok)/(slovka[q].stat_all+1);
     if (s>='ver.2.0') then readln(f);
   end;

 inc(sloviek,pocet);
 for q:=1 to sloviek do
        slovka[q].rating:=trunc(999*get_rating(slovka[q]));

closefile(f);
memo1.lines.add('LOAD >>>>>>>>');
memo1.lines.add('load complete - '+inttostr(sloviek)+' sloviek '+inttostr(skusanie)+' start');
memo1.lines.add('<<<<<<<<<');
naplnlistbox;
skusaj;
end;

procedure TForm1.answerKeyPress(Sender: TObject; var Key: Char);
var q:integer;
    c:tcolor;
begin
if (key <>chr(13)) then
 begin
  exit;
 end;

 inc(skusanie);

memo1.Lines.add(slovka[skusane_slovko].sk +' '+slovka[skusane_slovko].en);
if (answer.text=slovka[skusane_slovko].en) then
  begin
      memo1.lines.add('OK');
      c:=clgreen;
      inc(slovka[skusane_slovko].stat_ok);
      slovka[skusane_slovko].stat_hist:=
      (1-history_const)* slovka[skusane_slovko].stat_hist+history_const;
  end else
   begin
     c:=clred;
      memo1.lines.add('WRONG');
      slovka[skusane_slovko].stat_hist:=
      (1-history_const)* slovka[skusane_slovko].stat_hist;
   end;
   slovko_label.caption:=slovka[skusane_slovko].en+' - ' +slovka[skusane_slovko].sk;
   slovko_label.font.color:=c;
   slovko_label.refresh;
   application.processmessages;
   sleep(00+2000*integer(c=clred));
   slovko_label.font.color:=clblack;

   memo1.lines.add('');
 inc(slovka[skusane_slovko].stat_all);
 slovka[skusane_slovko].stat_last:=skusanie;



 naplnlistbox;


 skusaj;
//end;
end;

procedure TForm1.slovka_listClick(Sender: TObject);
begin
skusane_slovko:=slovka_list.ItemIndex+1;
slovko_label.caption:=slovka[skusane_slovko].sk;
sk_edit.text:=slovka[skusane_slovko].sk;
en_edit.text:=slovka[skusane_slovko].en;

end;

procedure TForm1.edit_buttonClick(Sender: TObject);
begin

with slovka[skusane_slovko] do
 begin
en:=en_edit.text;
sk:=sk_edit.text;
id:=sloviek;
stat_ok:=0;
stat_all:=0;
stat_last:=0;
stat_hist:=0;
rating:=0;
end;
en_edit.text:='';
sk_edit.text:='';


naplnlistbox;
skusaj;
sk_edit.setfocus;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
save_ini;
save_button.click;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
timer1.enabled:=false;
load_button.click;
end;

procedure TForm1.delete_buttonClick(Sender: TObject);
var q:integer;
begin
dec(sloviek);
for q:=slovka_list.ItemIndex+1 to sloviek do slovka[q]:=slovka[q+1];
en_edit.text:='';
sk_edit.text:='';

naplnlistbox;
skusaj;
end;

procedure TForm1.sort_optClick(Sender: TObject);
begin
naplnlistbox;
skusaj;
end;

procedure TForm1.en_editChange(Sender: TObject);
var best:integer;
    b:string;
    q:integer;
begin
if (not zobraz_listbox.enabled) then exit; // preskakujeme
//if (sender<>nil) then exit;
 // lebo nemame co vyhladavat
 
b:=#255;
best:=0;
for q:=1 to sloviek do
 begin
   if (copy(slovka[q].en,1,length(en_edit.text))=en_edit.text)then
     begin
       b:=slovka[q].en;
       best:=q;
     end;
 end;
 if (length(en_edit.text)=0) then best:=0;
slovka_list.ItemIndex:=best-1;


end;

procedure TForm1.file_editDblClick(Sender: TObject);
begin
//   save_buttonclick(self);
   opendialog1.filename:=file_edit.text;
  if (opendialog1.Execute) then
   begin
   file_edit.text:=opendialog1.FileName;
   load_buttonclick(self);
   end;
end;

procedure TForm1.new_buttonClick(Sender: TObject);
begin
sloviek:=0;
skusanie:=0;
memo1.lines.add('NEW >>>>>>>>>');
memo1.lines.add('<<<<<<<<');
naplnlistbox;
skusaj;
end;

procedure TForm1.clear_buttonClick(Sender: TObject);
begin
memo1.lines.Clear;
end;

procedure TForm1.clear_history_buttonClick(Sender: TObject);
var q:integer;
begin
  for q:=1 to sloviek do
  begin
    slovka[q].stat_ok := 0;
    slovka[q].stat_all := 0;
    slovka[q].stat_last := 0;
    slovka[q].stat_hist := 0;
  end;
  skusanie := 0;
end;

procedure TForm1.delete_duplicatesClick(Sender: TObject);
var q:integer;
 procedure delete(x:integer);
 var q:integer;
 begin
 dec(sloviek);
 for q:=x to sloviek do slovka[q]:=slovka[q+1];
 end;

begin
sort_opt.ItemIndex:=1;
slovka_sort(1,sloviek);
q:=2;
while (q<=sloviek) do
 begin

  if (slovka[q-1].en=slovka[q].en) and
     (slovka[q-1].sk=slovka[q].sk) then
      begin
       if (slovka[q-1].rating>slovka[q].rating) then
        begin
         delete(q)
        end else
        begin
         delete(q-1);
        end;
      end
      else inc(q);
      application.processmessages;
 end;

end;


end.