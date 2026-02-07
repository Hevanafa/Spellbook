program project;

{$Mode ObjFPC}
{$H+}

uses
  Interfaces, { This includes the LCL widgetset }
  Forms, Unit1;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='Spellbook - By Hevanafa (Feb 2026)';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

