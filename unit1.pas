unit Unit1;

{$Mode ObjFPC}
{$J-}
{$H-}

interface

uses
  Classes, SysUtils, Forms, Controls,
  Graphics, Dialogs, StdCtrls,
  LCLType;

type

  { TForm1 }

  TForm1 = class(TForm)
    SearchButton: TButton;
    InputEdit: TEdit;
    ResultMemo: TMemo;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure InputEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    rawWordlist: TStringList;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.InputEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if key = VK_RETURN then ResultMemo.Text := InputEdit.text;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  f: text;
  line: string;
begin
  rawWordlist := TStringList.create;

  if not FileExists('TWL06.txt') then begin
    ResultMemo.Text := 'Couldn''t find the dictionary file TWL06.txt!';
    ResultMemo.Enabled := false;
    exit
  end;

  AssignFile(f, 'TWL06.txt');
  {$I-} Reset(f); {$I+}

  while not EOF(f) do
  begin
    readln(f, line);
    rawWordlist.Add(line);
  end;

  closeFile(f);

  { TODO: Process the frequency list }
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  rawWordlist.Clear;
  FreeAndNil(rawWordlist);
end;

end.

