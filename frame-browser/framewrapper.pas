unit FrameWrapper;

{$mode ObjFPC}{$H+}

interface
uses
  Classes, SysUtils, Ctypes, ComCtrls, Types, StrUtils,
  StdCtrls;

type
  va_list = pointer;

  frm_t = pointer;
  frm_node_t = Pointer;

var frame_var: frm_t;


    function frm_create(dbpath: PAnsiChar): frm_t; cdecl; external 'frame';
    function frm_init(dbpath: PAnsiChar): frm_t; cdecl; external 'frame';
    procedure frm_close(frm: frm_t); cdecl; external 'frame';

    function frm_history(frm: frm_t; count: csize_t): PAnsiChar; cdecl; external 'frame';
    function frm_current(frm: frm_t): PAnsiChar; cdecl; external 'frame';
    function frm_payload: PAnsiChar; cdecl; external 'frame';
    function frm_date_epoch: UInt64; cdecl; external 'frame';
    function frm_date_str: PAnsiChar; cdecl; external 'frame';

    function frm_new(frm: frm_t; name, message: PAnsiChar): LongBool; cdecl; external 'frame';
    function frm_push(frm: frm_t; name, message: PAnsiChar): LongBool; cdecl; external 'frame';
    function frm_payload_replace(message: PAnsiChar): LongBool; cdecl; external 'frame';
    function frm_payload_append(message: PAnsiChar): LongBool; cdecl; external 'frame';
    function frm_payload_fname: PAnsiChar; cdecl; external 'frame';

    function frm_top(frm: frm_t): LongBool; cdecl; external 'frame';
    function frm_up(frm: frm_t): LongBool; cdecl; external 'frame';
    function frm_down(frm: frm_t; target: PAnsiChar): LongBool; cdecl; external 'frame';
    function frm_switch(frm: frm_t; target: PAnsiChar): LongBool; cdecl; external 'frame';
    function frm_back(frm: frm_t; index: csize_t): LongBool; cdecl; external 'frame';
    function frm_delete(frm: frm_t; target: PAnsiChar): LongBool; cdecl; external 'frame';
    function frm_pop(frm: frm_t; force: LongBool): LongBool; cdecl; external 'frame';

    function frm_list(frm: frm_t; from: PAnsiChar): PPAnsiChar; cdecl; external 'frame';
    function frm_match(frm: frm_t; sterm: PAnsiChar; flags: LongWord): PPAnsiChar; cdecl; external 'frame';
    function frm_match_from_root(frm: frm_t; sterm: PAnsiChar; flags: LongWord): PPAnsiChar; cdecl; external 'frame';

    procedure frm_mem_free(ptr: Pointer); cdecl; external 'frame';
    procedure frm_strarray_free(array_: PPAnsiChar); cdecl; external 'frame';

    function frm_readfile(fname: PAnsiChar): PAnsiChar; cdecl; external 'frame';
    function frm_vwritefile(fname: PAnsiChar; data: PAnsiChar; ap: Pointer): LongBool; cdecl; external 'frame';
    function frm_writefile(fname: PAnsiChar; data: PAnsiChar; args: array of const): LongBool; cdecl; varargs; external 'frame';

    function frm_node_create(frm: frm_t): frm_node_t; cdecl; external 'frame';
    procedure frm_node_free(rootnode: frm_node_t); cdecl; external 'frame';

function frm_node_name(node: frm_node_t): PAnsiChar; cdecl; external 'frame';
function frm_node_date(node: frm_node_t): cuint64; cdecl; external 'frame';
function frm_node_fpath(node: frm_node_t): PChar; cdecl; external 'frame';
function frm_node_nchildren(node: frm_node_t): csize_t; cdecl; external 'frame';
function frm_node_child(node: frm_node_t; index: csize_t): frm_node_t; cdecl; external 'frame';
function frm_node_parent(node: frm_node_t): frm_node_t; cdecl; external 'frame';
function frm_node_root(node: frm_node_t): frm_node_t; cdecl; external 'frame';
function frm_node_find(node: frm_node_t; fpath: PChar): frm_node_t; cdecl; external 'frame';



procedure frame_history_populate(searchTerm: String; tlView: TListView);
procedure frame_frames_populate(tv: TTreeView);
procedure frame_current_populate(stxt: TStaticText);
procedure frame_notes_populate(memo: TMemo);

implementation

procedure frame_history_populate(searchTerm: String; tlView: TListView);
var
  frame_history: PChar;
  i: csize_t;
  strItems: TStringDynArray;
  listItem: TListItem;

begin
  frame_history := frm_history(frame_var, csize_t($ffffffffffffffff));
  strItems := SplitString(frame_history, #$0a);

  tlView.Items.Clear();
  Writeln('nitems: ', Length(strItems));
  for i:=0 to (Length(strItems) - 1) do
  begin
    if (Length(searchTerm) > 2) and (PosEx(searchTerm, strItems[i]) <= 0) then
    begin
      continue;
    end;
    listItem := tlView.Items.Add();
    listItem.Caption := Copy(strItems[i], 1, Length(strItems[i]));
  end;
  frm_mem_free(frame_history);
end;


procedure addChild (tv: TTreeView; parent: TTreeNode; frmNode: frm_node_t);
var
  name: String;
  nchildren: csize_t;
  child: frm_node_t;
  i: csize_t;
  current: TTreeNode;

begin
  name := frm_node_name(frmNode);
  current := tv.Items.AddChild(parent, name);

  nchildren := frm_node_nchildren(frmNode);
  i := 0;

  while i < nchildren do
  begin
    child := frm_node_child(frmNode, i);
    addChild (tv, current, child);
    Inc(i);
  end;
end;

procedure frame_frames_populate(tv: TTreeView);
var
  root: pointer;
begin
  tv.Items.Clear;
  root := frm_node_create(frame_var);
  addChild (tv, nil, root);
  frm_node_free(root);
end;

procedure frame_current_populate(stxt: TStaticText);
begin
  stxt.Caption := frm_current(frame_var);
end;

procedure frame_notes_populate(memo: TMemo);
begin
  memo.Append(frm_payload());
end;

end.

