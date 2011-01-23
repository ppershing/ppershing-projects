unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,_ping;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    Memo1: TMemo;
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  time:integer;

implementation

{$R *.DFM}

procedure TForm1.Timer1Timer(Sender: TObject);
begin
inc(time);
if not _ping.ping('192.168.0.2') then
 begin memo1.lines.add(inttostr(time)+'NOOOO'); beep; end
 else
 begin
  memo1.lines.add(inttostr(time)+'yess');

 end;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
time:=0;
end;

end.
