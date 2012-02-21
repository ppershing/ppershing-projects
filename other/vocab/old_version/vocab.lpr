program vocab;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainForm
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='Vocabluary training';
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
