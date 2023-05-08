unit mainWin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, PairSplitter, Types, ComCtrls, CTypes, Cmem,
  FrameWrapper;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    bbtnQuit: TBitBtn;
    bbtnHelp: TBitBtn;
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
    procedure edtSearchTermChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin

end;

end.

