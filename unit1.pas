unit Unit1;

{$Mode ObjFPC}
{$J-}
{$H-}
{$Notes OFF}

interface

uses
  Classes, SysUtils, Forms, Controls,
  Graphics, Dialogs, StdCtrls,
  LCLType, FGL;

type
  TLetterFreqMap = specialize TFPGMap<char, smallint>;

  { TForm1 }

  TForm1 = class(TForm)
    SearchButton: TButton;
    InputEdit: TEdit;
    ResultMemo: TMemo;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure InputEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SearchButtonClick(Sender: TObject);

  private
    rawWordlist: TStringList;
    frequencyMap: specialize TFPGMap<string, TLetterFreqMap>;

    procedure performSearch;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.performSearch;
begin
  ResultMemo.Text := UpperCase(InputEdit.text)
end;

procedure TForm1.InputEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if key = VK_RETURN then
    performSearch;
end;

procedure TForm1.SearchButtonClick(Sender: TObject);
begin
  performSearch
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  f: text;
  line, term: string;
  c: char;
  newFreqMapEntry: TLetterFreqMap;
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

  ResultMemo.Text := 'Loaded ' + inttostr(rawWordlist.count) + ' words';

  { Process the frequency list }
  frequencyMap := specialize TFPGMap<string, TLetterFreqMap>.create;
  for term in rawWordlist do begin
    newFreqMapEntry := TLetterFreqMap.create;

    for c in term do
      if newFreqMapEntry.indexOf(c) >= 0 then begin
        newFreqMapEntry[c] := newFreqMapEntry[c] + 1
      end else
        newFreqMapEntry.Add(c, 1);

    frequencyMap.Add(term, newFreqMapEntry);
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  rawWordlist.Clear;
  FreeAndNil(rawWordlist);
end;

end.

