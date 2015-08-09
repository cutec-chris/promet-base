unit uvisualwlxmodule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dynlibs,
  uWlxPrototypes, WlxPlugin,uWlxModule
  ,LCLProc, LCLType
  {$IFDEF LCLWIN32}
  ,Windows
  {$ENDIF}
  {$IFDEF LCLGTK}
  , gtk, glib, gdk, gtkproc
  {$ENDIF}
  {$IFDEF LCLGTK2}
  , gtk2, glib2, gtk2proc
  {$ENDIF}
  {$IFDEF LCLQT}
  , qt4, qtwidgets
  // The Qt widgetset must be used to load plugins on qt
  {$ENDIF}
  ;

implementation

{$IF DEFINED(LCLWIN32)}
var
  WindowProcAtom: PWideChar;

function PluginProc(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  WindowProc: WNDPROC;
begin
  if Msg = WM_KEYDOWN then
  begin
    PostMessage(GetParent(hWnd), Msg, wParam, lParam);
  end;
  WindowProc := WNDPROC(GetPropW(hWnd, WindowProcAtom));
  if Assigned(WindowProc) then
    Result := CallWindowProc(WindowProc, hWnd, Msg, wParam, lParam)
  else
    Result := DefWindowProc(hWnd, Msg, wParam, lParam);
end;
{$ENDIF}

procedure WlxPrepareContainer(var ParentWin: HWND);
begin
{$IF DEFINED(LCLGTK) or DEFINED(LCLGTK2)}
  ParentWin := HWND(GetFixedWidget(Pointer(ParentWin)));
{$ELSEIF DEFINED(LCLQT)}
  ParentWin := HWND(TQtWidget(ParentWin).GetContainerWidget);
{$ENDIF}
end;


end.

