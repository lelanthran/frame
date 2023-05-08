unit mainWin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, PairSplitter, ComCtrls, StrUtils, Types, CTypes, Cmem,
  FrameWrapper;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    bbtnQuit: TBitBtn;
    bbtnHelp: TBitBtn;
    bbtnSearchHistory: TBitBtn;
    edtSearchTerm: TEdit;
    lvHistory: TListView;
    Memo1: TMemo;
    PairSplitter1: TPairSplitter;
    PairSplitter2: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    PairSplitterSide3: TPairSplitterSide;
    PairSplitterSide4: TPairSplitterSide;
    Panel1: TPanel;
    Panel2: TScrollBox;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    StaticText3: TStaticText;
    TreeView1: TTreeView;
    procedure bbtnQuitClick(Sender: TObject);
    procedure bbtnSearchHistoryClick(Sender: TObject);
  private

  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.bbtnQuitClick(Sender: TObject);
begin
  frmMain.Close;
end;

procedure TfrmMain.bbtnSearchHistoryClick(Sender: TObject);
var
  frame_history: PChar;
  nl: Char;
  i: csize_t;
  strItems: TStringDynArray;
  listItem: TListItem;

begin
  frame_history := frm_history(frame_var, csize_t($ffffffffffffffff));
  nl := Char($0a);
  strItems := SplitString(frame_history, #$0a);

  for i:=0 to Length(strItems) do
  begin
    listItem := frmMain.lvHistory.Items.Add();
    listItem.Caption := Copy(strItems[i], 1, Length(strItems[i]));
  end;
  frm_mem_free(frame_history);
end;

end.

