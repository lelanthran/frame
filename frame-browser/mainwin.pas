unit mainWin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, PairSplitter, ComCtrls;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    bbtnQuit: TBitBtn;
    bbtnHelp: TBitBtn;
    bbtnSearchHistory: TBitBtn;
    edtSearchTerm: TEdit;
    lbHistory: TListBox;
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
  private

  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

end.

