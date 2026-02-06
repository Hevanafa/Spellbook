unit Unit1;

{$Mode ObjFPC}
{$J-}
{$H+}
{$Notes OFF}

interface

uses
  Classes, SysUtils, Forms, Controls,
  Graphics, Dialogs, StdCtrls,
  DateUtils, LCLType, FGL;

type
  TLetterFreq = array['A'..'Z'] of byte;
  TDictFrequencyMap = specialize TFPGMap<string, TLetterFreq>;

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
    dictFrequencyMap: TDictFrequencyMap;

    function makeFrequencyMap(const word: string): TLetterFreq;
    procedure performSearch;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

function TForm1.makeFrequencyMap(const word: string): TLetterFreq;
var
  c: char;
begin
  fillchar(result, sizeof(result), 0);

  for c in word do
    if c in ['A'..'Z'] then
      inc(result[c]);
end;

function canMakeWord(const inputFreq, wordFreq: TLetterFreq): boolean;
var
  c: char;
begin
  result := true;

  for c:='A' to 'Z' do begin
    if wordFreq[c] > inputFreq[c] then begin
      result := false; exit
    end;
  end;
end;

function cmpLengthDesc(list: TStringList; a, b: longint): longint;
begin
  result := length(list[b]) - length(list[a]);

  if result = 0 then
    result := compareText(list[a], list[b]);
end;

procedure TForm1.performSearch;
var
  inputFreq: TLetterFreq;
  idx: longword;
  resultWordlist: TStringList;
  startTime: TDateTime;
begin
  startTime := now;
  ResultMemo.Lines.add('Starting search...');
  ResultMemo.Lines.Add('Dict count: ' + IntToStr(dictFrequencyMap.Count));
  application.ProcessMessages;  { Force UI update }

  inputFreq := makeFrequencyMap(uppercase(InputEdit.text));
  { ResultMemo.lines.add('Created inputFreq, count: ' + inttostr(inputFreq.count)); }
  application.ProcessMessages;

  resultWordlist := TStringList.create;

  for idx:=0 to dictFrequencyMap.Count - 1 do begin
    { if idx and $3FF = 0 then begin
      ResultMemo.lines.add('Progress: ' + inttostr(idx));
      application.ProcessMessages
    end; }

    { term := dictFrequencyMap.keys[idx]; }
    if canMakeWord(inputFreq, dictFrequencyMap.data[idx]) then
      resultWordlist.Add(dictFrequencyMap.keys[idx]);
  end;

  ResultMemo.lines.clear;
  ResultMemo.Lines.add('Search time: ' + inttostr(MilliSecondsBetween(now, startTime)) + 'ms');
  ResultMemo.lines.add('');

  startTime := now;
  resultWordlist.CustomSort(@cmpLengthDesc);
  ResultMemo.Lines.AddStrings(resultWordlist.text);
  ResultMemo.lines.add('Render time: ' + IntToStr(MilliSecondsBetween(now, startTime)) + 'ms');

  { FreeAndNil(inputFreq); }
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
  dictFrequencyMap := TDictFrequencyMap.create;
  for term in rawWordlist do
    dictFrequencyMap.add(term, makeFrequencyMap(term));
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FreeAndNil(rawWordlist);
end;

end.

