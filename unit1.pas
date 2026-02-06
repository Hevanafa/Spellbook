unit Unit1;

{$Mode ObjFPC}
{$J-}
{$H+}
{$Notes OFF}

interface

uses
  Classes, SysUtils, Forms, Controls,
  Graphics, Dialogs, StdCtrls,
  DateUtils, LCLType, LCLIntf,
  RichMemo, FGL,
  BCButton, BCTypes, BCListBox;

type
  TLetterFreq = array['A'..'Z'] of byte;
  TDictFrequencyMap = specialize TFPGMap<string, TLetterFreq>;

  { TForm1 }

  TForm1 = class(TForm)
    SearchButton: TBCButton;
    BenchmarkLabel: TLabel;
    ResultMemo: TRichMemo;
    InputEdit: TEdit;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure InputEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SearchButtonClick(Sender: TObject);

  private
    rawWordlist: TStringList;
    dictFrequencyMap: TDictFrequencyMap;

    procedure loadFontFromResource(const resname: string);

    procedure appendHeading(const txt: string);
    procedure appendText(const txt: string);

    function makeFrequencyMap(const word: string): TLetterFreq;
    procedure performSearch;
    function filterByLength(const s: string): boolean;

  public

  end;

var
  Form1: TForm1;

implementation

uses Windows;

function AddFontMemResourceEx(pbFont: pointer; cbFont: DWORD; pdv: pointer; pcFonts: PDWORD): THANDLE; stdcall; external 'gdi32.dll';

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

var
  filterLen: SmallInt;
{ This must be attached to an object }
function TForm1.filterByLength(const s: string): boolean;
begin
  Result := length(s) = filterLen
end;

procedure TForm1.appendHeading(const txt: string);
var
  startPos, textLen: longint;
begin
  ResultMemo.SelStart := ResultMemo.GetTextLen;
  ResultMemo.SelLength := 0;

  ResultMemo.SelText := txt + LineEnding;

  ResultMemo.SetRangeParams(
    ResultMemo.SelStart, ResultMemo.SelLength,
    [tmm_Styles, tmm_Color],
    '', 0, TColor($9C5A00),
    [fsBold], []
  );

  ResultMemo.SelStart := ResultMemo.GetTextLen;
  ResultMemo.SelLength := 0;
end;

procedure TForm1.appendText(const txt: string);
begin
  ResultMemo.SelStart := ResultMemo.GetTextLen;
  ResultMemo.SelLength := 0;

  ResultMemo.SelText := txt + LineEnding;

  ResultMemo.SetRangeColor(ResultMemo.SelStart, ResultMemo.SelLength, clBlack);

  ResultMemo.SelStart := ResultMemo.GetTextLen;
  ResultMemo.SelLength := 0
end;

procedure TForm1.performSearch;
var
  inputFreq: TLetterFreq;
  idx: longword;
  len: word;
  resultWordlist: TStringList;
  startTime: TDateTime;
  searchTime, renderTime: longword;  { in milliseconds }
  shortest, longest: smallint;
  filtered: TStrings;
begin
  startTime := now;

  ResultMemo.lines.clear;
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

  searchTime := MilliSecondsBetween(now, startTime);

  ResultMemo.lines.clear;
  { ResultMemo.Lines.AddStrings(resultWordlist.text); }

  startTime := now;
  resultWordlist.CustomSort(@cmpLengthDesc);

  longest := length(resultWordlist[0]);
  shortest := length(resultWordlist[resultWordlist.count - 1]);

  for len:=longest downto shortest do begin
    filterLen := len;
    filtered := resultWordlist.filter(@filterByLength);

    if filtered.count > 0 then begin
      appendHeading(format('%d Letters:', [len]));
      appendText(trimRight(filtered.text));
      appendText('')
    end;

    filtered.free
  end;

  ResultMemo.SelStart := 0;
  ResultMemo.SelLength := 0;

  renderTime := MilliSecondsBetween(now, startTime);

  BenchmarkLabel.Caption := format(
    'Search time: %dms'#13#10'Render time: %dms'#13#10'Results: %d',
    [searchTime, renderTime, resultWordlist.count]);

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

procedure TForm1.loadFontFromResource(const resname: string);
var
  resStream: TResourceStream;
  bytes: TBytes;
  fontHandle: THandle;
  numFonts: DWORD;
begin
  resStream := TResourceStream.create(HINSTANCE, resname, RT_RCDATA);

  try
    SetLength(bytes, resStream.size);
    resStream.ReadBuffer(bytes[0], resStream.size);

{$ifdef windows}
    fontHandle := AddFontMemResourceEx(@bytes[0], length(bytes), nil, @numFonts);
{$endif}
  finally
    resStream.free
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  f: text;
  line, term: string;
begin
  { Load resources }
  loadFontFromResource('DROIDSANS');
  loadFontFromResource('DROIDSANS-BOLD');

  font.name := 'Droid Sans';
  font.size := 10;

  { Init app state }
  rawWordlist := TStringList.create;
  ResultMemo.lines.clear;

  if not FileExists('TWL06.txt') then begin
    appendText('Couldn''t find the dictionary file TWL06.txt!');
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

  appendText('Loaded ' + inttostr(rawWordlist.count) + ' words');

  { ResultMemo.rtf := 'Lorem ipsum!'; }

  { Process the frequency list }
  dictFrequencyMap := TDictFrequencyMap.create;
  for term in rawWordlist do
    dictFrequencyMap.add(term, makeFrequencyMap(term));
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FreeAndNil(rawWordlist);
  freeandnil(dictFrequencyMap);
end;

end.

