unit mainWin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, PairSplitter, Types, ComCtrls, TreeFilterEdit, SynEdit, CTypes,
  Cmem, LCLType, Menus, FrameWrapper;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    bbtnQuit: TBitBtn;
    bbtnHelp: TBitBtn;
    edtSearchTerm: TEdit;
    lvHistory: TListView;
    memoNotes: TSynEdit;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    PairSplitter1: TPairSplitter;
    PairSplitter2: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    PairSplitterSide3: TPairSplitterSide;
    PairSplitterSide4: TPairSplitterSide;
    Panel1: TPanel;
    Panel2: TScrollBox;
    ctxMenuCurrentFrame: TPopupMenu;
    Separator1: TMenuItem;
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
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure tvFramesEditingEnd(Sender: TObject; Node: TTreeNode;
      Cancel: Boolean);
    procedure tvFramesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
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


procedure Alert(message: String);
var
  tmp: String;
begin
   ShowMessage(message);
   tmp := frm_lastmsg(frame_var);
   frmMain.sbarStatus.SimpleText:=frm_lastmsg(frame_var);
end;

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

function FrameInit (path: String): Boolean;
begin
  frm_close(frame_var);
  frame_var := frm_init(Pchar(path));
  if frame_var = nil then
  begin
    Alert('Failed to open Frame Database at [' + path + ']'
                  + sLineBreak
                  + 'Aborting');
    Exit(false);
  end;
  Exit(true);
end;

procedure FrameReopen();
begin
  FrameInit(frm_homepath() + '/.framedb');
  frame_history_populate('', frmMain.lvHistory);
  frame_current_populate(frmMain.edtCurrentFrame);
  frame_notes_populate(frmMain.memoNotes);
  frame_frames_populate(frmMain.tvFrames);
  frame_set_frames_selected(frmMain.tvFrames, frmMain.edtCurrentFrame);
end;

procedure TfrmMain.FormActivate(Sender: TObject);
begin
  if FrameInit(frm_homepath() + '/.framedb') <> true then
  begin
    frmMain.Close;
    Exit;
  end;
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
  frmMain.notesChanged:=false;
  frmMain.stxtStatusChanged.Caption:='';
end;

procedure TfrmMain.MenuItem1Click(Sender: TObject);
begin
  frmMain.sbarStatus.SimpleText:='Pushing new frame';
  if frm_push(frame_var, 'New Frame', 'Enter Contents for new frame') <> true then
  begin
    Alert('failed to push new frame:' + sLineBreak + frm_lastmsg(frame_var));
  end else
  begin
    FrameReopen();
    frmMain.tvFrames.Selected := frmMain.tvFrames.Items.FindNodeWithTextPath(frmMain.edtCurrentFrame.Caption);
    frmMain.tvFrames.Selected.EditText;
  end;
end;

procedure TfrmMain.MenuItem2Click(Sender: TObject);
begin
  frmMain.sbarStatus.SimpleText:='Popping current frame';
  if frm_pop(frame_var, false) <> true then
  begin
       Alert('failed to pop frame:' + sLineBreak + frm_lastmsg(frame_var));
  end else
  begin
    FrameReopen();
  end;
end;

procedure TfrmMain.MenuItem3Click(Sender: TObject);
var
 node: TTreeNode;
begin
  frmMain.sbarStatus.SimpleText:='Renaming current frame';
  node := frmMain.tvFrames.Selected;
  node.EditText();
end;

procedure TfrmMain.MenuItem4Click(Sender: TObject);
begin
  frmMain.sbarStatus.SimpleText:='Recursively popping current frame';
  if frm_pop(frame_var, true) <> true then
  begin
       Alert('failed to force pop frame:' + sLineBreak + frm_lastmsg(frame_var));
  end else
  begin
    FrameReopen();
  end;
end;

procedure TfrmMain.tvFramesEditingEnd(Sender: TObject; Node: TTreeNode;
  Cancel: Boolean);
var
  newname: String;
begin
  if Cancel = true then
     Exit();

  newname := Node.Text;
  if frm_rename(frame_var, PChar(newname)) <> true then
  begin
    Alert('Failed to rename node:' + sLineBreak + frm_lastmsg(frame_var));
  end else
  begin
    FrameReopen();
  end;
end;

procedure TfrmMain.tvFramesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = VK_F2 then
  begin
    frmMain.tvFrames.Selected.EditText;
  end;

  if key = VK_LCL_ALT then
  begin
    frmMain.ctxMenuCurrentFrame.PopUp;
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

