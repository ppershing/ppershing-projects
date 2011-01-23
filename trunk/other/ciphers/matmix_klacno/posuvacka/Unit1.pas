unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    Button1: TButton;
    Edit3: TEdit;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
var key_size:integer;
    q:integer;
    pos:integer;
    c:char;
    cc:integer;
begin
edit3.text:='';
key_size:=length(edit2.text);
pos:=0;
for q:=1 to length(edit1.text) do
  begin
   c:=edit1.text[q];
   if (c>='a') and (c<='z') then
     begin
   cc:=ord(c);
   cc:=cc-ord('a');
   cc:=cc-(1+ord(edit2.text[(pos mod key_size)+1])-ord('a'));
   memo1.lines.add(inttostr(cc));
   cc:=cc+26*100;
   cc:=cc mod 26;
   edit3.text:=edit3.text+char(cc+ord('a'));
   pos:=pos+1;
     end
     else
     edit3.text:=edit3.text+c;

  end;


end;

end.
