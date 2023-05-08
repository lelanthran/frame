unit FrameWrapper;

{$mode ObjFPC}{$H+}

interface
uses
  Classes, SysUtils, Ctypes, ComCtrls, Types, StrUtils;

type
  va_list = pointer;

  frm_t = pointer;

var frame_var: frm_t;

procedure frm_mem_free(ptr: Pointer); cdecl; external 'frame';
function frm_readfile(fname: PChar): PChar; cdecl; external 'frame';
function frm_vwritefile(fname: PChar; data: PChar; ap: va_list): LongBool; cdecl; external 'frame';
function frm_writefile(fname: PChar; data: PChar; Args: array of const): LongBool; cdecl; external 'frame';
function frm_create(dbpath: PChar): frm_t; cdecl; external 'frame';
function frm_init(dbpath: PChar): frm_t; cdecl; external 'frame';
procedure frm_close(frm: frm_t); cdecl; external 'frame';
function frm_history(frm: frm_t; count: csize_t): PChar; cdecl; external 'frame';
function frm_current(frm: frm_t): PChar; cdecl; external 'frame';
function frm_payload(frm: frm_t): PChar; cdecl; external 'frame';
function frm_date_epoch(frm: frm_t): cuint64; cdecl; external 'frame';
function frm_date_str(frm: frm_t): PChar; cdecl; external 'frame';
function frm_push(frm: frm_t; name, message: PChar): LongBool; cdecl; external 'frame';
function frm_payload_replace(frm: frm_t; message: PChar): LongBool; cdecl; external 'frame';
function frm_payload_append(frm: frm_t; message: PChar): LongBool; cdecl; external 'frame';
function frm_payload_fname(frm: frm_t): PChar; cdecl; external 'frame';
function frm_top(frm: frm_t): LongBool; cdecl; external 'frame';
function frm_up(frm: frm_t): LongBool; cdecl; external 'frame';
function frm_down(frm: frm_t; target: PChar): LongBool; cdecl; external 'frame';
function frm_switch(frm: frm_t; target: PChar): LongBool; cdecl; external 'frame';
function frm_back(frm: frm_t; index: csize_t): LongBool; cdecl; external 'frame';
function frm_delete(frm: frm_t; target: PChar): LongBool; cdecl; external 'frame';
function frm_pop(frm: frm_t; force: LongBool): LongBool; cdecl; external 'frame';
function frm_list(frm: frm_t; from_: PChar): PPChar; cdecl; external 'frame';
function frm_match(frm: frm_t; sterm: PChar; flags: cuint32): PPChar; cdecl; external 'frame';
function frm_match_from_root(frm: frm_t; sterm: PChar; flags: cuint32): PPChar; cdecl; external 'frame';

procedure frame_history_populate(searchTerm: String; tlView: TListView);

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
  for i:=0 to Length(strItems) do
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

end.

