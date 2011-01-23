unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, GameEngine, Buttons,libudp,messagequeue;

type
  TForm1 = class(TForm)
    New_Game: TButton;
    plocha: TImage;
    Level: TLabel;
    start: TTimer;
    stlacenebolo: TLabel;
    Resetbut: TBitBtn;
    About: TBitBtn;
    High_score: TBitBtn;
    nazov: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    exit: TBitBtn;
    radio: TRadioGroup;
    Edit1: TEdit;
    Label3: TLabel;
    Timer1: TTimer;
    Timer2: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure New_GameKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure High_scoreKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure New_GameKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure High_scoreKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure New_GameClick(Sender: TObject);
    procedure startTimer(Sender: TObject);
    procedure ResetbutClick(Sender: TObject);
    procedure ResetbutKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ResetbutKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure AboutKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure AboutKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure AboutClick(Sender: TObject);
    procedure exitKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure exitKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure exitClick(Sender: TObject);
    procedure High_scoreClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


var Form1: TForm1;
    Hra: THra;
    udpcon:tudpconnection;
implementation

{$R *.dfm}

procedure init(subor: string);{NASTAVY CASOVACE A NAHRA LEVEL ZO SUBORU zivoty a score ostava z predosla}
begin
//  sndPlaySound('ding.wav',SND_ASYNC);
  With Form1 do begin
    Start.Enabled:=false;
    Hra.LoadLevel(subor);
//    Lives.Text:=Inttostr(Hra.Live);
  end;
end;

procedure uvod;{VYKRESLI PEKNU UVODNU OBRAZOVKU}
begin
   With Form1 do begin
    Level.Caption:='Mazze game by Maty';
    Start.Enabled:=false;
    Hra.LoadLevel('level0');
    Hra.stop:=true;
    Start.Enabled:=True;
   end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin

   DoubleBuffered := true;
   randomize;
   if hra<>nil then FreeAndNil(Hra);
   Hra:=Thra.Create(0,0,Form1.plocha.Canvas);{vytvory prazdnu hru}
   uvod;{zobrazi uvodnu obrazovku}

end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var data:tudpdata;
begin
 data.length:=2;
 data.data[0]:=42;
 data.data[1]:=byte(char(key));
if (udpcon<>nil) then udpcon.send(data);

//Hra.stlac(chr(Key),true);
//stlacenebolo.Caption:=InttoStr(Key)+' '+chr(Key);
//   case chr(Key) of
//     'W' : stlacenebolo.Caption:='w';
//     'S' : stlacenebolo.Caption:='s';
//     'A' : stlacenebolo.Caption:='a';
//     'D' : stlacenebolo.Caption:='d';
//   end;
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var data:tudpdata;
begin
 data.length:=2;
 data.data[0]:=43;
 data.data[1]:=byte(char(key));
if (udpcon<>nil) then udpcon.send(data);
//Hra.stlac(chr(Key),false);

end;

procedure TForm1.New_GameKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
   Form1.FormKeyDown(Sender,Key,Shift);
end;

procedure TForm1.High_scoreKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
   Form1.FormKeyDown(Sender,Key,Shift);
end;

procedure TForm1.New_GameKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
      Form1.FormKeyUp(Sender,Key,Shift);
end;

procedure TForm1.High_scoreKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
     Form1.FormKeyUp(Sender,Key,Shift);
end;

procedure TForm1.New_GameClick(Sender: TObject);
begin
   udpcon:=tudpconnection.create(edit1.text,4321);
   Hra.reset_game;
   edit1.enabled:=false;
   if (radio.itemindex>0) then
    plocha.visible:=false;
   {zapne pocitadlo casu}
end;

procedure TForm1.startTimer(Sender: TObject);
var ok:boolean;
    data:tudpdata;

begin
while (udpcon<>nil) and (not udpcon.received_data.empty) do
  begin
    data:=udpcon.received_data.front;
    udpcon.received_data.pop;
    if (data.data[0]=47) then close;
    if (data.data[0]=99) then
     begin
      messagedlg('Level '+inttostr(data.data[1])+' completed',
      mtinformation,[mbok],0);

     end;
    if (data.data[0]=42) then
     begin
      hra.stlac(char(data.data[1]),true);
      beep;
     end;
    if (data.data[0]=43) then
     begin
      hra.stlac(char(data.data[1]),false);
       beep;
     end;
  end;


SetFocus;
Form1.Nazov.Caption:='Level '+copy(Hra.Level,6,Length(Hra.Level));;
if Hra.stop then hra.t1:=now;
Label2.Caption:=FormatDateTime('hh:nn:ss', now - Hra.T1);
Hra.tik(ok);

end;

procedure TForm1.ResetbutClick(Sender: TObject);
begin
 Hra.Reset_game;
end;

procedure TForm1.ResetbutKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
   Form1.FormKeyUp(Sender,Key,Shift);
end;

procedure TForm1.ResetbutKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
    Form1.FormKeyDown(Sender,Key,Shift);
end;

procedure TForm1.AboutKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
     Form1.FormKeyUp(Sender,Key,Shift);
end;

procedure TForm1.AboutKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
   Form1.FormKeyDown(Sender,Key,Shift);
end;

procedure TForm1.AboutClick(Sender: TObject);
begin
   ShowMessage('Maze game by Maty '+#13+#13+'O hre: V hre ide prejdenie bludiska za co'+#13
                                           +'najkratsi cas. Aby to nebolo prilis jednoduche'+#13+
                                            'tak sa bludisko toci okolo hraca. Smer hraca'+#13+
                                            'ked ide hore sa ale nemeni.'+#13+
                                            'Ovladanie: Na ovladanie sluzia klavesy W,S,A,D'+#13+
                                            'Created by Maty');
end;

procedure TForm1.exitKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
   Form1.FormKeyUp(Sender,Key,Shift);
end;

procedure TForm1.exitKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
    Form1.FormKeyDown(Sender,Key,Shift);
end;

procedure TForm1.exitClick(Sender: TObject);
begin
   Close;
end;

procedure TForm1.High_scoreClick(Sender: TObject);
var s : WideString;
 i: longint;
begin
  s:='High Score';
  for i:=0 to hra.maxscorelevels do
  s:=s+#13+'Level '+inttoStr(i)+':  '+Hra.maxscoreNick[i]+' '+FormatDateTime('hh:nn:ss', Hra.maxscorecas[i]);
  showMessage(s);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var data:tudpdata;
begin
data.length:=1;
data.data[0]:=1; // false packet
if (udpcon<>nil) then udpcon.send(data);

end;

procedure TForm1.Timer2Timer(Sender: TObject);
begin
if (udpcon<>nil) then 
 label3.caption:='Link quality: '+
 inttostr(udpcon.get_percent_acks)+' retransmissions:'+
 inttostr(udpcon.get_retransmission_count)+' waiting messages:'
 +inttostr(udpcon.get_queue_size);

end;

end.
