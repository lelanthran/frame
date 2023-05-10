unit mainWin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, PairSplitter, Types, ComCtrls, TreeFilterEdit, CTypes, Cmem,
  FrameWrapper;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    bbtnQuit: TBitBtn;
    bbtnHelp: TBitBtn;
    edtSearchTerm: TEdit;
    lvHistory: TListView;
    memoNotes: TMemo;
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
    edtCurrentFrame: TEdit;
    tvFrames: TTreeView;
    procedure bbtnQuitClick(Sender: TObject);
    procedure edtSearchTermChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tvFramesSelectionChanged(Sender: TObject);
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


procedure TfrmMain.edtSearchTermChange(Sender: TObject);
var
  sterm: String;
begin
  sterm := frmMain.edtSearchTerm.Caption;
  frame_history_populate (sterm, frmMain.lvHistory);
end;

procedure TfrmMain.FormActivate(Sender: TObject);
begin
  frame_var := frm_init('/home/lelanthran/.framedb');
  frame_history_populate('', frmMain.lvHistory);
  frame_current_populate(frmMain.edtCurrentFrame);
  frame_notes_populate(frmMain.memoNotes);
  frame_frames_populate(frmMain.tvFrames);
  frame_set_frames_selected(frmMain.tvFrames, frmMain.edtCurrentFrame);
end;


procedure TfrmMain.FormCreate(Sender: TObject);
begin

end;

function TreeNodeFpath(node: TTreeNode): String;
var
  parent: String;
begin
  if node = nil then
  begin
    Exit('');
  end;

  parent := TreeNodeFpath(node.Parent);
  if Length(parent) > 1 then
  begin
    parent := parent + '/';
  end;
  Exit(parent + node.Text);
end;

procedure TfrmMain.tvFramesSelectionChanged(Sender: TObject);
var
  fpath: String;
  ttnode: TTreeNode;
begin
  ttnode := frmMain.tvFrames.Selected;
  if ttnode <> nil then
  begin
    fpath := TreeNodeFpath(frmMain.tvFrames.Selected);
    Writeln('Clicked: ', fpath);
    frm_switch(frame_var, PChar(fpath));
    frame_history_populate('', frmMain.lvHistory);
    frame_current_populate(frmMain.edtCurrentFrame);
    frame_notes_populate(frmMain.memoNotes);
  end;
end;

end.

