program chat;

uses
  Forms,
  chat1 in 'chat1.pas' {Form1},
  UDP in 'UDP.pas' {UDPSearchForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TUDPSearchForm, UDPSearchForm);
  Application.Run;
end.
