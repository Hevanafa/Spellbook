unit Unit1;

{$Mode ObjFPC}
{$J-}
{$H+}
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
    dictFrequencyMap: specialize TFPGMap<string, TLetterFreqMap>;

    function makeFrequencyMap(const word: string): TLetterFreqMap;
    procedure performSearch;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

function TForm1.makeFrequencyMap(const word: string): TLetterFreqMap;
var
  c: char;
  count: smallint;
begin
  result := TLetterFreqMap.create;
  count := 0;

  for c in word do
    if result.trygetdata(c, count) then begin
      result[c] := count + 1
    end else
      result.Add(c, 1);
end;

function canMakeWord(const inputFreq, wordFreq: TLetterFreqMap): boolean;
var
  a: SmallInt;
  c: char;
  wordCount, inputCount: smallint;
begin
  result := true;

  for a:=0 to wordFreq.count - 1 do begin
    c := wordFreq.keys[a];
    wordCount := wordFreq[c];

    if not inputFreq.TryGetData(c, inputCount) then begin
      result := false; exit
    end;

    if inputCount < wordCount then begin
      result := false; exit
    end;
  end;
end;

procedure TForm1.performSearch;
var
  inputFreq: TLetterFreqMap;
  idx: longword;
  term: string;
  resultWordlist: TStringList;
begin
  inputFreq := makeFrequencyMap(uppercase(InputEdit.text));
  resultWordlist := TStringList.create;

  { ResultMemo.Text := UpperCase(InputEdit.text) }

  for idx:=0 to dictFrequencyMap.Count - 1 do begin
    term := dictFrequencyMap.keys[idx];

    if canMakeWord(inputFreq, dictFrequencyMap[term]) then
      resultWordlist.Add(term);
  end;

  ResultMemo.Text := resultWordlist.text;

  FreeAndNil(inputFreq);
  FreeAndNil(resultWordlist);
end;

procedure TForm1.InputEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if key = VK_RETURN then performSearch;
end;

procedure TForm1.SearchButtonClick(Sender: TObject);
begin
  performSearch
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  f: text;
  line, term: string;
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
  dictFrequencyMap := specialize TFPGMap<string, TLetterFreqMap>.create;
  for term in rawWordlist do
    dictFrequencyMap.add(term, makeFrequencyMap(term));

  ResultMemo.Lines.Add('Dict count: ' + IntToStr(dictFrequencyMap.Count));
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  rawWordlist.Clear;
  FreeAndNil(rawWordlist);
end;

end.

