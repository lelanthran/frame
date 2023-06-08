unit mainWin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, PairSplitter, Types, ComCtrls, TreeFilterEdit, CTypes, Cmem,
  LCLType, Menus,
  FrameWrapper;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    bbtnQuit: TBitBtn;
    bbtnHelp: TBitBtn;
    edtSearchTerm: TEdit;
    lvHistory: TListView;
    memoNotes: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    PairSplitter1: TPairSplitter;
    PairSplitter2: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    PairSplitterSide3: TPairSplitterSide;
    PairSplitterSide4: TPairSplitterSide;
    Panel1: TPanel;
    Panel2: TScrollBox;
    ctxMenuCurrentFrame: TPopupMenu;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    StaticText3: TStaticText;
    edtCurrentFrame: TEdit;
    sbarStatus: TStatusBar;
    stxtStatusChanged: TStaticText;
    tvFrames: TTreeView;
    procedure bbtnQuitClick(Sender: TObject);
    procedure edtSearchTermChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lvHistorySelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure memoNotesChange(Sender: TObject);
    procedure memoNotesEditingDone(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure tvFramesSelectionChanged(Sender: TObject);
  private
    notesChanged: Boolean;
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

procedure FrameReopen();
begin
  frm_close(frame_var);
  frame_var := frm_init('/home/lelanthran/.framedb');
  frame_history_populate('', frmMain.lvHistory);
  frame_current_populate(frmMain.edtCurrentFrame);
  frame_notes_populate(frmMain.memoNotes);
  frame_frames_populate(frmMain.tvFrames);
  frame_set_frames_selected(frmMain.tvFrames, frmMain.edtCurrentFrame);
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

procedure TfrmMain.lvHistorySelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  fpath: String;
  ttnode: TListItem;
begin
  ttnode := frmMain.lvHistory.Selected;
  if ttnode <> nil then
  begin
    fpath := ttnode.Caption;
    frm_switch_direct(frame_var, PChar(fpath));
    frame_current_populate(frmMain.edtCurrentFrame);
    frame_notes_populate(frmMain.memoNotes);
    frame_frames_populate(frmMain.tvFrames);
    frame_set_frames_selected(frmMain.tvFrames, frmMain.edtCurrentFrame);
  end;
end;

procedure TfrmMain.memoNotesChange(Sender: TObject);
begin
  frmMain.notesChanged:=true;
  frmMain.stxtStatusChanged.Caption:='Frame notes changed';
end;

procedure Alert(message: String);
begin
   ShowMessage(message);
end;

procedure TfrmMain.memoNotesEditingDone(Sender: TObject);
var
 reply, boxStyle: Integer;
begin
  if frmMain.notesChanged = false then
     Exit;

  boxStyle := MB_ICONQUESTION + MB_YESNO;
  reply := Application.MessageBox('Frame contents changed. Save Changes?', 'Contents Changed', BoxStyle);
  if reply = IDYES then
  begin
    frm_payload_replace(PChar(frmMain.memoNotes.Text));
  end;
end;

procedure TfrmMain.MenuItem1Click(Sender: TObject);
begin
  frmMain.sbarStatus.SimpleText:='Pushing new frame';
  if frm_push(frame_var, 'New Frame', 'Enter Contents for new frame') <> true then
  begin
    Alert('failed to push new frame');
  end else
  begin
    FrameReopen();
  end;
end;

procedure TfrmMain.MenuItem2Click(Sender: TObject);
begin
  frmMain.sbarStatus.SimpleText:='Popping current frame';
  if frm_pop(frame_var, false) <> true then
  begin
       Alert('failed to pop frame');
  end else
  begin
    FrameReopen();
  end;
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
    frm_switch_direct(frame_var, PChar(fpath));
    frame_history_populate('', frmMain.lvHistory);
    frame_current_populate(frmMain.edtCurrentFrame);
    frame_notes_populate(frmMain.memoNotes);
    frmMain.notesChanged:=false;
    frmMain.stxtStatusChanged.Caption:= '';
  end;
end;

end.

