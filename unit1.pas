unit Unit1;

{$Mode ObjFPC}
{$J-}
{$H+}
{$Notes OFF}

interface

uses
  Classes, SysUtils, Forms, Controls,
  Graphics, Dialogs, StdCtrls,
  DateUtils, LCLType, LCLIntf, ExtCtrls,
  RichMemo, FGL,
  BCButton, BCTypes, BCListBox, BCMaterialEdit, BGRACustomDrawn;

type
  TLetterFreq = array['A'..'Z'] of byte;
  TDictFrequencyMap = specialize TFPGMap<string, TLetterFreq>;

  { TForm1 }

  TForm1 = class(TForm)
    InputEdit: TBCMaterialEdit;
    ContentPanel: TPanel;
    BackgroundPanel: TPanel;
    SearchButton: TBCButton;
    BenchmarkLabel: TLabel;
    ResultMemo: TRichMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure InputEditChange(Sender: TObject);
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

  for c:='A' to 'Z' do
    if wordFreq[c] > inputFreq[c] then begin
      result := false; exit
    end;
end;

{ Compare length, descending, used with result word list }
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
    '', 0, TColor($69F0A7),
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

  ResultMemo.SetRangeColor(
    ResultMemo.SelStart, ResultMemo.SelLength,
    clWhite);

  ResultMemo.SelStart := ResultMemo.GetTextLen;
  ResultMemo.SelLength := 0
end;

function makeColumns(const words: TStrings; columns: SmallInt = 1): string;
var
  a: word;
begin
  if columns < 1 then columns := 1;

  result := '';

  for a:=0 to words.count - 1 do begin
    result := result + words[a];

    if a < words.count - 1 then begin
      if (a + 1) mod columns <> 0 then
        { This can use either the figure space #$2007, or a tab (#9) }
        result := result + '   '
      else
        result := result + LineEnding;
    end;
  end;

  result := trimRight(result)
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
  bufferStr: string;
  a: word;
begin
  if trim(InputEdit.text) = '' then exit;

  { This procedure uses figure spaces (#$2007) as the column delimiter }

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
      appendHeading(format('%d Letters', [len]));

      case len of
        2..5:
          appendText(makeColumns(filtered, 4));
        6, 7, 8:
          appendText(makeColumns(filtered, 3));
        else
          { trimRight is necessary because a newline is appended implicitly }
          appendText(trimRight(filtered.text))
      end;

      { appendText(trimRight(
        StringReplace(filtered.text, LineEnding, #9, [rfReplaceAll])
      )); }

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

procedure TForm1.InputEditChange(Sender: TObject);
var
  a: smallint;
  cleaned: string;
  lastEditPos: smallint;
begin
  if InputEdit.text = '' then exit;

  cleaned := '';

  lastEditPos := InputEdit.Edit.SelStart;

  for a:=1 to length(InputEdit.text) do
    if InputEdit.text[a] in ['a'..'z', 'A'..'Z'] then
      cleaned := cleaned + InputEdit.text[a];

  if cleaned <> InputEdit.text then begin
    InputEdit.text := cleaned;
    InputEdit.edit.SelStart := lastEditPos
  end;
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

  { Load the dictionary }
  rawWordlist := TStringList.create;

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

  { Build the frequency list }
  dictFrequencyMap := TDictFrequencyMap.create;
  for term in rawWordlist do
    dictFrequencyMap.add(term, makeFrequencyMap(term));
end;

procedure TForm1.FormResize(Sender: TObject);
var
  origWidth, origHeight: smallint;
  targetWidth, targetHeight: smallint;
  ar: double;  { aspect ratio }
begin
  ar := 5 / 7;

  origWidth := BackgroundPanel.width;
  origHeight := BackgroundPanel.height;

  if origWidth / origHeight > ar then begin
    targetHeight := origHeight;
    targetWidth := round(targetHeight * ar)
  end else begin
    targetWidth := origWidth;
    targetHeight := round(targetWidth / ar)
  end;

  with ContentPanel do begin
    width := targetWidth;
    height := targetHeight;
    left := (origWidth - targetWidth) div 2;
    top := (origHeight - targetHeight) div 2
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  { Init UI elements }
  font.name := 'Droid Sans';
  font.size := 10;

  BenchmarkLabel.font.name := font.name;
  BenchmarkLabel.font.size := font.size;

  SearchButton.StateNormal.FontEx.name := font.name;
  { This should follow the ScaleBy procedure call }
  SearchButton.StateNormal.FontEx.Height := round(font.height * 1.2);

  { Apply the same font properties as StateNormal }
  with SearchButton.StateNormal.FontEx do begin
    SearchButton.StateClicked.FontEx.name := name;
    SearchButton.StateClicked.FontEx.height := height;
    SearchButton.StateHover.FontEx.name := name;
    SearchButton.StateHover.FontEx.height := height;
  end;

  ScaleBy(120, 100);

  InputEdit.Text := '';
  ResultMemo.lines.clear;

  BenchmarkLabel.Caption := 'Welcome!';
  appendText('Loaded ' + inttostr(rawWordlist.count) + ' words');

  { Debug font height }
  { appendText(inttostr(SearchButton.StateNormal.FontEx.Height) + ' ' + inttostr(font.height)); }

  { Manual positioning }
  {
  self.left := screen.PrimaryMonitor.left;
  self.top := screen.PrimaryMonitor.top;
  }
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FreeAndNil(rawWordlist);
  freeandnil(dictFrequencyMap);
end;

end.

