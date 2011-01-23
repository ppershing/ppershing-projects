unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, WaveIO, WaveOut, WavePlayers,math;

type
  TForm1 = class(TForm)
    audio: TLiveAudioPlayer;
    Button1: TButton;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    function audioData(Sender: TObject; const Buffer: Pointer;
      BufferSize: Cardinal; var NumLoops: Cardinal): Cardinal;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

const samplefreq=44100;
const call:array[0..13] of integer =(0,9,0,8,6,2,6,4,0,4,-1,-1,-1,-2);

var
  Form1: TForm1;
  freq1,freq2:integer;
  cycle:integer;
  subcycle:integer;
  phase:integer;

implementation

{$R *.DFM}

function getfreq1(x:integer):integer;
begin
if (x=-1) then result:=500;
if (x=-2) then result:=0;

if (x=0) then result:=941;
if (x>=1) and (x<=3) then result:=697;
if (x>=4) and (x<=6) then result:=770;
if (x>=7) and (x<=9) then result:=852;
end;

function getfreq2(x:integer):integer;
begin
if (x=-1) then result:=250;
if (x=-2) then result:=0;
if (x=0) then begin result:=1336; exit; end;
if (x mod 3=0) then result:=1477;
if (x mod 3=1) then result:=1209;
if (x mod 3=2) then result:=1336;

end;


procedure advance;
begin
inc(phase);
if (phase>cycle div 5+5) then
  begin
   phase:=- cycle div 5-1;
   inc(subcycle);
   if (subcycle>=length(call)) then
     begin
      subcycle:=0;
      inc(cycle);
     end;

  end;
if (phase<0) then
 begin
 freq1:=0; freq2:=0; end
  else
  begin
  freq1:=getfreq1(call[subcycle]);
  freq2:=getfreq2(call[subcycle]);

 end;


end;

procedure TForm1.Button1Click(Sender: TObject);
begin
audio.Active:=not audio.active;

end;

function TForm1.audioData(Sender: TObject; const Buffer: Pointer;
  BufferSize: Cardinal; var NumLoops: Cardinal): Cardinal;
var q,qq:integer;
    buf:pbytearray;
begin

advance;
label1.caption:=inttostr(cycle)+' '+inttostr(subcycle)+' '+
 inttostr(phase)+',  '+inttostr(freq1)+' '+inttostr(freq2)+
 '      '+inttostr(buffersize);

buf:=buffer;
for q:=0 to 4410-1 do
 begin
  qq:=q+4410*phase;
  buf[q]:=128+trunc(60*(sin (2 * pi * qq/samplefreq * freq1) +
    sin (2 * pi * qq/samplefreq * freq2)) );
 end;

  result:=4410;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
cycle:=1;
phase:=1000;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
audio.active:=false;
end;

end.
