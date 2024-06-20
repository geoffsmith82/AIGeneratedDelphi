program Clock;

uses
  Vcl.Forms,
  ClockForm in 'ClockForm.pas' {Form1};

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
