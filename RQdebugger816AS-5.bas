$INCLUDE "RAPIDQ.INC"

$INCLUDE "QXpTheme.INC"
$INCLUDE "QButtonXP.inc"
$INCLUDE "ConvertToTruePath.inc"
$Include "NewWndProc.inc"   ' or $Include "CallBackAll.Inc"
$Include "QtabControlEx.inc"
$include "QColorButton.inc"

'cls

ver$="b816AS Jul 2023"
Declare Sub TabPopUpMenuReloadFile
Declare Sub OpenCurrPrjClick
Declare Sub TabListCmboxChange
Declare Sub ArdLogFileButnonclick
Declare sub SetTab25length

DECLARE SUB ListBoxDrawItem(Index AS INTEGER, State AS BYTE, Rect AS QRECT)
DECLARE SUB ListBoxMeasureItem(Index AS INTEGER, Height AS INTEGER)
DelWinListBoxClr=clm
Declare Sub FindDisabledFilesOnClick

'!!! ---- $INCLUDE ------ lIsFusion
Declare Function CreateDirIfNotExists (DirName$) as   long

Declare Sub SplitterV3Moved
Declare Sub LogBtnRightOnClick
Declare Sub LogBtnCntrOnClick
Declare Sub LogBtnLeftOnClick
Declare Sub ArdCliHelpFormShow
Declare Sub ArdCliOnOnSetEditText(Col%, Row%, Value$, Sender AS QSTRINGGRID)
Declare Sub ArdCliOnListDropDown (Col%, Row%, BYREF S AS STRING, Sender AS QSTRINGGRID)
Declare Sub ArdCliHelpFormClose
Declare Sub ArdCliCmdHelpComboChange

Declare Sub LibMngFormShow
Declare Sub StopItOnClick
Declare Sub LibMngListGridSelectCell(Col%, Row%, CanSelect%, Sender as QStringGrid)
Declare Sub LoadJson
Declare Sub Tab25AddTab
Declare Sub Tab25DelTab


Declare Sub ArduinoToolsCmboxOnChange
Declare Sub ArduinoCmdCmboxOnChange
Declare Sub sketchbookPathBtnonclick
Declare Sub Tab25Change

Application.HintPause=100
Application.HintColor =&H7AFF59'' &H00FF00
'Application.HintColor =0 '&H7AFF59'' &H00FF00
'Application.HintTextColor=clr

darkcolor=&H272727 '&H5A5654

'$APPTYPE gui
$INCLUDE "QFILEDIALOG.INC"

$INCLUDE "RichEdPopUp.inc"
$INCLUDE "QColorDialog.inc"
$Include "constants.inc"
'$include "QColorButton.inc"
$include "Object\QINI.inc" 
$include "Object\QStatusBarEx.inc" 
$include "Form.inc"

$define WM_SETFOCUS  &H7
$define  WM_KILLFOCUS  &H8

RQversion$="RQ Advanced Search v1.05-12INC"
RQversionDate$="15 Jul 2023"

noletter$=cr+lf+",./\| !@#$&^&*()+=-?><~`;:[]{}"+chr$(39)+ht+qt
FileNameToCopy$=""
dim TmpFileSLtextCP$ as string
dim TmpFileSLtext$ as string

TmpFileSLtextCP$="---" ' копия для текста файла для последующих перекодировок
TmpFileSLtext$="++++++"
AlreadyFound=0 ' уже найден, не надо искать в других кодировках этого файла
NoSearch=0 ' не искать в данной кодировке

'defint SelGridClr,OldSelGridClr
SelGridClr=&h80ffff 'clGreen'

SDNum=0
SDGAlka=1
SDPath=2
SDName=3 '2
SDSize=4
SDDate=5
SDTime=6
SDColor=7 '6

OldSubDir$=""
defint GridClr
GridClr=&HD9FFCE

codP$="Win"

declare function ansi2koi lib "fbsv.dll" alias "ANSI2KOI@4"( src as string) as long 'string
declare function koi2ansi lib "fbsv.dll" alias "KOI2ANSI@4"( src as string) as long ' as string
declare Function  ConvertCP lib "fbsv.dll" alias "CONVERTCP@16"_
( byref strSrc As String, nFromCP As Long, nToCP As Long,  byref strOut As string) as long  


'declare function ansi2koi lib "FBKoi.dll" alias "ANSI2KOI@4"( src as string) as long 'string



$IFNDEF strl1$
defint _crlf=2

'!*****************************************
FUNCTION STRL1$ (value AS long) AS STRING
tmp$=STRF$(value, ffFixed, 11, 18)
if instr(tmp$,".")>0 then decSep$="."
if instr(tmp$,",")>0 then decSep$=","

STRL1$ = field$(STRF$(value, ffFixed, 11, 18),decSep$,1)

END FUNCTION
$ENDIF


$OPTION ICON "./resource/buggs.ico"

declare function GetMatches lib "fb_regex.dll" alias "GETMATCHES@16"_
( byref regex as string, byref SrcText as string, byref ptrArrBeg as integer, byref ptrArrEnd as integer ) as integer

Declare Function WinExec Lib "kernel32" Alias "WinExec" (ByVal lpCmdLine As String, ByVal nCmdShow As Long) As Long


Declare Function ShellExecute Lib "shell32.dll" Alias "ShellExecuteA"_
(ByVal hwnd As Long, ByVal lpOperation As String, ByVal lpFile As String, ByVal lpParameters As String, ByVal lpDirectory As String, ByVal nShowCmd As Long) As Long

'print  ' "curdir$=";curdir$
'curdir1$=curdir$
'print"curdir1$=";curdir1$
'print 12," ",timer
'Declare Sub TimerOnceOver

declare Function ConvertCodePage(SourceString As String, inPage As long, outPage As long) As string

DECLARE FUNCTION SetParent Lib "user32" Alias "SetParent" (hWndChild As Long,hWndNewParent As Long) As Long


Declare Function GetConsoleCP Lib "kernel32" Alias "GetConsoleCP" () As Long
Declare Function SetConsoleCP Lib "kernel32" Alias "SetConsoleCP" (ByVal wCodePageID As Long) As Long
Declare Function GetConsoleOutputCP Lib "kernel32" Alias "GetConsoleOutputCP" () As Long
Declare Function SetConsoleOutputCP Lib "kernel32" Alias "SetConsoleOutputCP" (ByVal wCodePageID As Long) As Long

'print"GetConsoleCP=";GetConsoleCP
'print"GetConsoleOutputCP=";GetConsoleOutputCP
'SetConsoleCP(1251)
'SetConsoleOutputCP(1251)

'print"консоль GetConsoleCP=";GetConsoleCP
'print"консоль GetConsoleOutputCP=";GetConsoleOutputCP



dim  ArdBoardList as QStringlist


PortName$=""
BoardName$=""
FQBN$=""
Core$=""



'!!! ---- Resource -----
$RESOURCE sOpen AS "./resource/sOpen.bmp"
$RESOURCE Run_rsc as ".\resource\Run.bmp"
$RESOURCE Bug2s as "resource\Bug2s.bmp"
$RESOURCE AddWatch_rsc as  "resource\AddWatch.bmp"
$RESOURCE DelWatch_rsc as "resource\DelWatch.bmp"
$RESOURCE DelAllWatch_rsc as "resource\DelAllWatch.bmp"
$RESOURCE ClearAllBrP_rsc as "resource\ClearAllBrP.bmp"
'$RESOURCE CloseDbg_rsc as "resource\2775.bmp"
'print 22," ",timer

'$INCLUDE "resource.inc" 'c:\BAS\RAPIDQ\RQ IDE\
Declare Sub ViewTplFileMng
$RESOURCE SubNext_BMP AS "resource\Subnext.bmp"
$RESOURCE SubPrev_BMP AS "resource\Subprev.bmp"
'$RESOURCE Compile_BMP AS "resource\Compile.bmp"
$RESOURCE Copy_BMP AS "resource\Copy.bmp"
$RESOURCE Cut_BMP AS "resource\Cut.bmp"
'$RESOURCE Delete_BMP AS "resource\Delete.bmp"
$RESOURCE New_BMP AS "resource\New.bmp"
$RESOURCE Open_BMP AS "resource\Open.bmp"
$RESOURCE OpenHEX_BMP AS "resource\OpenHEX.bmp"
$RESOURCE Paste_BMP AS "resource\Paste.bmp"
$RESOURCE Save_BMP AS "resource\Save.bmp"
'$RESOURCE SelectAll_BMP AS "resource\Selectall.bmp"
$RESOURCE Undo_BMP AS "resource\Undo.bmp"
$RESOURCE FileMng_BMP as "resource\OutlineGrey.bmp" ' components\filelistbox.bmp"
'$RESOURCE SavePrj_BMP as "resource\Prj1.BMP"
$RESOURCE SavePrj_BMP as "resource\SAVEALL.bmp"
'H:\RQIDE\resource\stringgrid.bmp H:\RQIDE\resource\SAVEALL.bmp
$RESOURCE FBLogo_bmp as "resource\FBsslogo1.BMP" 
$RESOURCE  SubList_bmp as "resource\listbox.bmp" ' "SubList.BMP" 
$RESOURCE  DirTree_bmp as "resource\ObjTree.bmp"
$RESOURCE  IncTree_bmp as "resource\IncTree.bmp"
'----------------------------------------------
const MF_BYCOMMAND = 0
const MF_BYPOSITION = &H400


Declare Function SetMenuItemBitmaps Lib "user32" Alias "SetMenuItemBitmaps" (hMenu As Long, _
nPosition As Long, wFlags As Long, _
hBitmapUnchecked As long, hBitmapChecked As long) As Long

CONST LR_LOADFROMFILE = &H10
CONST LR_CREATEDIBSECTION = &H2000

DECLARE FUNCTION LoadImage Lib "user32" ALIAS "LoadImageA" (hInst AS LONG,lpsz AS STRING, dwImageType AS LONG,dwDesiredWidth AS LONG, dwDesiredHeight AS LONG,dwFlags AS LONG) AS LONG

function LoadMP( BMPFileNAme as string) as long
result =  LoadImage(0,  BMPFileNAme,0,0,0,LR_LOADFROMFILE OR LR_CREATEDIBSECTION)
end function

hsOpen_BMP =LoadMP("resource\obj\Open.bmp")

hCopy_BMP =LoadMP("resource\Copy.bmp")
hCut_BMP =LoadMP("resource\Cut.bmp")
hDelete_BMP =LoadMP("resource\Delete.bmp")
hNew_BMP =LoadMP("resource\New.bmp")
hOpen_BMP =LoadMP("resource\Open.bmp")
hOpenHEX_BMP =LoadMP("resource\OpenHEX.bmp")
hPaste_BMP =LoadMP("resource\Paste.bmp")
hSave_BMP =LoadMP("resource\Save.bmp")
hSelectAll_BMP =LoadMP( "resource\Selectall.bmp")
hUndo_BMP =LoadMP("resource\Undo.bmp")



'$include "QDirListView.inc"

Const GWL_STYLE=-16
Const TVS_NOTOOLTIPS=&H80
Declare Function SetWindowLong Lib "user32" Alias "SetWindowLongA" (hwnd As Long, nIndex As Long, dwNewLong As Long) As Long
Declare Function GetWindowLong Lib "user32" Alias "GetWindowLongA" (hwnd As Long, nIndex As Long) As Long

codp$="win"

'-------------------------------------
DIM ColorDialog AS QColorDialog
ColorDialog.customer(1)=&HFF0000
ColorDialog.customer(2)=&H00FF00
ColorDialog.customer(3) = &H0000FF
ColorDialog.customer(4) = &HFF00FF
ColorDialog.customer(5) = &H00FFFF
ColorDialog.customer(6) = &HFFFFFF
ColorDialog.customer(7) = &H559911
ColorDialog.customer(8) = &HEE44BB
ColorDialog.customer(9) = &HBB44EE
ColorDialog.customer(10) = &H115599
ColorDialog.customer(11) = &H333333
ColorDialog.customer(12) = &H666666
ColorDialog.customer(13) = &H999999
ColorDialog.customer(14) = &HABABAB
ColorDialog.customer(15) = &HDDDDDD
ColorDialog.customer(16) = &H550000

defint ClrDlgOpen 
'defint SelGridClr ''цвет подсветки ячейки
defint clrTok        ' цвет текущих токенов 
defint RowClr,OldRow=1      'номер подсвеченной строки 
defint NumParStyle=11 '-1' число строк описания стиля оформления.
defint fontcolor  ', bgcolor, bordercolor

defstr ClrSchComBoxText$
Dim StrFontA as QFont




defint hdc '-- device handle'

DIM M AS QMEMORYSTREAM
defint FIRSTVISIBLELINE,LastVISIBLELINE

defint FirstChar

'SortLabelCount   =64 '---
'dim SortLabel(SortLabelCount) as QPanel

defint BegSub ' позиция начала текущей процедуры
defint NoReload ' флаг разрешения перезагрузки файла в новое окно
'--------------------------------------
ScreenWidth=Screen.Width
ScreenHeight=Screen.Height

'!!!----- Memory status -------
Type T_MEMORYSTATUS
dwLength As Long
dwMemoryLoad As Long
dwTotalPhys As Long
dwAvailPhys As Long
dwTotalPageFile As Long
dwAvailPageFile As Long
dwTotalVirtual As Long
dwAvailVirtual As Long
End Type

Declare Sub GlobalMemoryStatus Lib "kernel32" Alias "GlobalMemoryStatus" (lpBuffer As T_MEMORYSTATUS)
Dim tMemInfo As T_MEMORYSTATUS
Declare Sub ShowFreePhysicalMemory

'-----------------------------------------
defint KeyDowned, KeyUpped, bline,eline,numtab ' при нажатии отступ
OldClipBrd$="--"

defint ListGridReady=0
defint lineNumber=0
SubDelim$="'!******************************************"

dim SEarchList as QStringList
FNtmp$ ="" ' имя файла открытого через файл менеджер или через inc tree

'!!!----- Formatting ----
defint PrevSubNamePos,SubNamePos,NextSubNamePos,SubLineIdx,FuncNamePos

'IndentWords$="For-Next;With-End With;Select case-case-select else-end select;If-elseif-else-end if;Create-end create;While-wend;Do-Loop"

'SubCount=0
'SubCountOver=0
'  !!!-----language ---------
scanLngCol=3
numLine=0 '5
'!!!-----language Pack ----
'dim LangVar$ (2048) as string
SrcLang=3 ' English - номер столбца в верхней таблице


'--- Declarations --- !!!
Declare Sub DelWinBtnOnClick
Declare Sub CancelDelWinBtnOnClick
Declare Sub DelWinListOnClick
Declare Sub CloseTabBnOnClick
Declare Sub TabPopUpMenuAdd
Declare Sub TabPopUpMenuDel
Declare sub SetTab25Index

Declare Sub DelSchemBtnOnClick
Declare Sub RQFormonresize
Declare Sub RQFormonshow
Declare Sub SearchListFormClose
Declare Sub ArduinoflgCmboxOnChange
Declare Sub CodeSectBtnOnClick
Declare Sub BoardCmbBoxOnChange
Declare Sub BoardCmbBoxOnChange1
Declare Sub TabRightChange
Declare Sub CliFileButnonclick
Declare Sub ArdBasTabChange
Declare Sub ArduinoPrefOnClick
Declare Sub UploadArduinoSketch
Declare Sub VerifyArduinoSketch
Declare Sub CheckPrjOnClick
Declare Sub ViewPrjFileMng
Declare Sub SyntaxHLBoxOnClick
Declare Sub ClrSchEditOnKeyPress(Key AS BYTE)
Declare Sub AddSchemBtnOnClick
Declare Sub ThemeChange

Declare Sub SaveIDEOptions
Declare Sub WinBtnOnmove (X%, Y%, Shift%, sender as QLabel)

Declare Sub FileHEXLoad
Declare Sub DElEmptyRowsOnClick
Declare Sub DElRowOnClick
Declare Sub SrcLangEditOnKeyUp(Key AS Word, Shift AS INTEGER)
Declare Sub H2IncOnClick
Declare Sub SetDefLng

'--- lang declares -----!!!
Declare Sub SaveLangOnClick
Declare Sub ClearLangGrids
Declare Sub CreateLangPack
Declare Sub LangGrid2SetEditText (Col%, Row%, Value$)
Declare Sub LangGrid1SelectCell  (Col% , Row%, CanSelect%, Sender AS QStringGrid) 
Declare Sub LangGrid1DrawCell (Col%, Row%, State%, Rect AS QRect, Sender AS QStringGrid)
Declare Sub LangGrid2SelectCell  (Col% , Row%, CanSelect%, Sender AS QStringGrid) 
Declare Sub LangGrid2DrawCell (Col%, Row%, State%, Rect AS QRect, Sender AS QStringGrid)
Declare Sub ScanSrcLang
Declare Sub CreateSectionList
Declare Sub SectionOnClick
Declare Sub RQFormOnMouseMove(X%, Y%, Shift%)
Declare Sub DelLangOnClick
Declare Sub AddLangOnClick1
Declare Sub LangMngFormResize
'--- end lang declares

Declare Sub ReFormatC
Declare Sub FindAll
Declare Sub DOS2Win
declare Sub CreateINCfilesTree
declare Sub ReFormatRTF
declare Sub ReFormatHTml

declare function  UCaseR$ (Cases$) as string
declare function  LCaseR$ (Cases$) as string

Declare Sub CodPChange  (sender as QButtonXP)
Declare Sub QPDecode
Declare Sub UTF8Decode

Declare Sub LoadGridBtnOnClick
Declare Sub SwapcellBtnOnClick
Declare Sub DelDupBtnOnClick
Declare Sub SortBtnOnClick
Declare Sub SortLabelOnClick (Sender as QLabel)
Declare Sub Str2ChrOnClick
Declare SUB HLDrawCell (Col%, Row%, State%, Rect AS QRect, Sender AS QStringGrid)
Declare SUB HLSelectCell ( Col%, Row%, CanSelect%) 
Declare Sub HLNewTypeNAme  (Col%, Row%, Value$)
Declare Sub IncTreeViewDblClick
Declare Sub ObjTreeViewDblClick
Declare Sub MinRightPanelOnClick
Declare Sub ExpandRightPanelOnClick
Declare Sub CLoseRightPanelOnClick
Declare Sub IncFilesMnuOnClick
Declare Sub CreateINCfileslist
Declare Sub IncTreeViewClick
Declare Sub ObjTreeViewClick
Declare Sub BuildObjTree
Declare Sub TreeViewMouseMove(X AS INTEGER, Y AS INTEGER, Shift AS INTEGER, Sender AS QTREEVIEW)
Declare Sub TreeViewChange(Node AS INTEGER, AllowChange AS INTEGER, Sender AS QTREEVIEW)


Declare Sub ObjTreeOnClick
Declare Sub CheckEmptyWinOnClick
Declare Sub DelEmptyWinOnClick
Declare Sub SplitterV2Moved
Declare Sub WindowsOnClick
Declare Sub DisRemBtnOnClick
Declare Sub HelpRus
Declare Sub HelpEn

Declare Sub PrevBookMark
Declare Sub NextBookMark
Declare Sub PrevSub
Declare Sub NextSub
Declare Sub ReFormatHotTabsCurSub
Declare Sub DelEmptyLinesOnClick
Declare Sub DelCommentsOnClick
Declare sub ReFormatHotTabs
Declare Sub DelHotTabs
Declare Sub DeleteLedSpaces '(Sender as QMenuItem)
Declare Sub ListGridDrawCell (Col%, Row%, State%, R AS QRect)
Declare Sub RichEditSelectAll
Declare Sub RichEditDelete
Declare Sub RichEditPaste
Declare Sub RichEditCopy
Declare Sub RichEditCut
Declare Sub RichEditUndo
DECLARE SUB btnClick(sender as QButtonXP)

declare sub FileListBox1OnChange (Sender as QFileListBox)
declare sub FileListDblClick

declare sub FileClick
declare  SUB Check(key AS BYTE)               

declare  SUB Check1
DECLARE   SUB ChangeDirectory
DECLARE   SUB ChangeDirectory1 
DECLARE   SUB ChangeDirectory2 
'DECLARE   SUB MaskChange
'DECLARE   SUB MaskDel
'DECLARE   SUB MaskSave

declare sub RefreshDirTree
declare sub FileOnlyOnClick
declare sub AddExtOnClick
declare sub OnDirPopUp

Declare Sub ViewFileMng 'Sender as QMenuItem)
Declare Sub ListSelectCell(Col% , Row%, CanSelect%, Sender AS QStringGrid)  
Declare Sub ListChBoxClick
Declare Sub ClearBMark
Declare Sub LoadRqTpl
Declare Sub RqTplChange
'Declare Sub IncFilesChoose (Sender as QMenuItem)

Declare Sub VerifyEventOnClick
Declare Sub PasteDateOnClick
Declare Sub PasteCurFileNameOnClick
Declare Sub PasteSubDelimiter

Declare Sub CMDLParamOnClick
defint ddlflg
Declare Sub FBDllOnClick
Declare Sub FBEXEOnClick
Declare Sub FBCompileOnClick
Declare Sub SavePrjAsOnClick
declare sub GoToLine (LineNumber as int)
declare sub ReplCheckClick              
'declare sub ProjectsChoose
declare sub clearWin_bm
declare sub OpenStFiles (Sender as QMenuItem)

declare sub CreatePrjOnClick
DECLARE SUB MainModulePathClick (Sender AS QCOOLBTN)

declare sub RQFormShow
declare sub SortBMark
declare sub SetBMark
declare sub GoToLineError
declare sub ChooseIcon

declare sub SrcEditOnKeyDown (Key AS WORD, Shift AS INTEGER)
declare sub SrcEditOnKeyUp (Key AS WORD, Shift AS INTEGER)

'defbyte SEdKeyDFlg
'defbyte SEdKeyDFlgRep 
'defbyte SEdKeyDFlgRep1 

declare sub LogValue
declare sub PrintValue
declare sub AddDeclaration (Sender AS QMenuItem)

declare sub FileLoad1
declare function ParseString (StrLine$ as string, VAlMPos as long) as string
setbmf=0

dim RapidQTplItem$(1) as string

'OnceFlg=0
'SetX=600
'SetY=80

defint FMngOpenFlg 
' флаг открытие файла производится из менеджера и он не
' должен включаться в список окон проекта

EditFlg=0 ' тип файла в редакторе
'!!! 0 - файл в редакторе находится в списке окон (файл из состава проекта) 
'!!! 1 - файл в редакторе не находится в списке окон (файл НЕ из состава проекта)
'!!! 2 - include$ файл открыт через дерево inc файлов 

'позиции окон 
DEFWORD WLTopM=80: DEFWORD WLLeftM=500 : DEFWORD OnceFlgW=0
DEFWORD SLTopM=80: DEFWORD SLLeftM=600: DEFWORD OnceFlgS=0
DEFWORD ObjInTopM=80: DEFWORD ObjInLeftM=400:DEFWORD OnceFlgObj=0

DEFWORD PropSGridH
DEFWORD PropSGridW
'!!! -- FreeBAsic -----
defstr FBCompParam$

'!!!----- Debugger ------
'------------- constant part DebugForm etc. -------
'Dim WatchedVars(1) as string

Dim ~Op(1) as string
dim ~OpPos(1) as integer
dim ~OpLen(1) as integer
dim VarN$(100) as string, VarType$(100) as string
defstr SrcFilePath$ ' путь текущего файла

LangFilePath$=SrcFilePath$+"language\" '---
'call  AddClrString ("9141:SrcFilePath$="+(SrcFilePath$), clred, LogEdit)
'MKSubDir (LangFilePath$)


'-------- end of constant part -------------------

DEFWORD dbgflg=0
DEFWORD NumPer=64'128 ' всего может наблюдаться 128 переменных и в том числе что-то

dim DebugStr(NumPer) as string '- это массив строк, по чиcлу переменных, который будут наблюдаться
'Эти строки будут вставляться в программу

dim VAlScope(NumPer) as string ' Global or SubName
dim VAlName(NumPer) as string ' имя  переменной
dim SubVAlName(NumPer) as string ' имя  переменной
dim VAlType(NumPer) as byte ' тип переменной 0 - не строковая 1 - строковая

DEFWORD AddWatchIndex ' номер текущей добавляемой переменной в WatchEdit

' номер оператора,  его позиция, длина оператора для подсветки. 1l
DEFWORD OpersCount  ' число наблюдаемых операторов 
'DEFWORD OperPos(4096) ' (8192)  ' позиция
'DEFWORD OperLen(4096) '  (8192) ' длина 
'DEFWORD OperLine(4096) '  (8192) ' номер строки на которой находится оператор.

dim DBGSUbLstPos as QSTRINGLIST 


'!!!--------------
defint selB=1, selst=1'art
'------------------------
Dim MsgList as QSTRINGLIST 
DEFWORD errlineNumb
DEFWORD OldFirstVisLine

DEFWORD DirTag=0 ' номер кнопки устаноки директории в сетапе.
DEFWORD DelPrjFlg=0 ' признак удаления проекта

defstr OpenDialogFilter,OpenDialogInitialDir

dim RQdbini as QINI
dim ProjectIni as QINI

DEFWORD reload

declare sub Splitter2Moved
declare sub LoadFile2Window 
RQdbiniflg=0
declare sub RichEditOnMouseDown
declare sub  R2Change
declare sub CloseWinOnClick
declare sub LastWindClick

declare sub RefreshBMark
declare sub LastBMarkClick
declare sub DelBMark
declare sub AddBMark
declare sub BMarkChoose (Sender as qmenuitem)
declare sub OpenFileCurs
declare sub SAveLog    'сохранить содержание логедита
declare sub SaveMainLog

'$TypeCheck On

Public Const FR_DOWN = &H1
Public Const FR_WHOLEWORD = &H2
Public Const FR_MATCHCASE = &H4
Public Const FR_FINDNEXT = &H8
Public Const FR_REPLACE = &H10
Public Const FR_REPLACEALL = &H20
'- -------------------------'

'!!!- -- обмен данными между окнами ---- '
'CONST WM_COPYDATA = &H4A
TYPE COPYDATASTRUCT
dwData AS LONG
cbData AS LONG
lpData AS LONG
END TYPE
DIM DataStruct AS COPYDATASTRUCT
DEFSTR strSend, strRecv, AppName

declare sub FrmClose
DECLARE SUB SendData '(AppName, strSend)
'DECLARE SUB FormWndProc (Hwnd&, uMsg&, wParam&, lParam&)
DECLARE FUNCTION FindWindow LIB "user32" ALIAS "FindWindowA" (ByVal lpClassName AS String, ByVal lpWindowName AS String) AS LONG
'- -------------------------------------------------------

'!!! ----- HiLight -----
Declare Sub HtHLBoxClick
dim HotTAbHL as long
HotTAbHL=1
'declare sub bas2
'-- messages
CONST WM_DESTORY = 2
CONST WM_PAINT = &HF

'-- Edit Control Messages

'-- EDITWORDBREAKPROC code values
Public Const WB_LEFT = 0
Public Const WB_RIGHT = 1
Public Const WB_ISDELIMITER = 2

Type Point
Left As Long
Top As Long
End Type

CONST GWL_WNDPROC = (-4)
CONST GWL_HWNDPARENT = (-8)

'dim ScrollPoint as Point

'DECLARE FUNCTION SetWindowLongAPI LIB "user32" ALIAS "SetWindowLongA" _
'(ByVal hWnd AS LONG, ByVal nIndex AS LONG, ByVal dwNewLong AS LONG) AS LONG

Private Declare Function HideCaret Lib "user32" ALIAS "HideCaret" (ByVal hwnd As Long) As Long
Private Declare Function ShowCaret Lib "user32" ALIAS  "ShowCaret" (ByVal hwnd As Long) As Long

'Declare Function CallWindowProc Lib "user32" Alias "CallWindowProcA" _
'(ByVal lpPrevWndFunc As Long, ByVal hWnd As Long, ByVal Msg As Long, ByVal wParam As Long, _
'ByVal lParam As Long) As Long

'Declare Function CallWindowProc Lib "user32" Alias "CallWindowProcA" _
'(lpPrevWndFunc As Long,  hWnd As Long, Msg As Long,  wParam As Long,  lParam As Long) As Long

'DECLARE SUB FormWndProc (Handle AS INTEGER, uMsg AS LONG, wParam AS LONG, lParam AS LONG)

DECLARE FUNCTION SendMessageApi LIB "user32.dll" ALIAS "SendMessageA" (hWnd AS LONG, Msg AS LONG, wParam AS LONG, lParam AS LONG) AS LONG

Declare Function GetDC Lib "user32" Alias "GetDC"(ByVal hWnd As Long) As Long
Declare Function ReleaseDC Lib "user32" Alias "ReleaseDC" _
(ByVal hwnd As Long, ByVal hdc As Long) As Long

Declare Function SetBkMode Lib "gdi32" Alias "SetBkMode" (ByVal hdc As Long, ByVal nBkMode As Long) As Long
Declare Function TextOut Lib "gdi32" Alias "TextOutA" (ByVal hdc As Long, ByVal x As Long, ByVal y As Long, ByVal lpString As String, ByVal nCount As Long) As Long

Declare Function SelectObject Lib "gdi32" Alias "SelectObject" (ByVal hdc As Long, ByVal hObject As Long) As Long
Declare Function SetTextColor Lib "gdi32" Alias "SetTextColor" (ByVal hdc As Long, ByVal crColor As Long) As Long

'Declare Function CreateRectRgn Lib "gdi32" (ByVal X1 As Long, ByVal Y1 As Long, ByVal X2 As Long, ByVal Y2 As Long) As Long


declare function HiLight lib "HiLightDllv66-4.dll" alias "HiLight@20"_
(byval RhWnd as long, byval HiLiteFontHandle as long, byref ptrQBColor as integer,  ptrKeyList as long, hthl as long ) as long 



'!! ---- RichEditWndProc -----
declare FUNCTION RichEditWndProc (hWnd AS LONG, uMsg AS LONG, wParam AS LONG, lParam AS LONG) AS LONG
declare FUNCTION StringGridWndProc (hWnd AS LONG, uMsg AS LONG, wParam AS LONG, lParam AS LONG) AS LONG

declare Function fbTally lib "fbFunctions.dll" alias "FBTALLY@8"  ( searchstr as string,  matchstr as string) as long 
declare Function fbFIELD lib "fbFunctions.dll" alias "FBFIELD@12"  ( byref  Srcstring as string, delimstring as string, fieldNumber as integer) as  long 'string 



'declare FUNCTION IncEditWndProc (hWnd AS LONG, uMsg AS LONG, wParam AS LONG, lParam AS LONG) AS LONG

Declare Function FillRect Lib "user32" Alias "FillRect" _ 
(ByVal hdc As Long, lpRect As QRect, ByVal hBrush As Long) As Long

Declare Function GetBkColor Lib "gdi32" Alias "GetBkColor" (ByVal hdc As Long) As Long

' Background Modes
Public Const TRANSPARENT = 1
Public Const OPAQUE = 2
Public Const BKMODE_LAST = 2

dim VisRect as QRect
dim VisPoint as Point
dim VisPoint1 as Point
Dim hBrush As Long
dim bgcolor As Long


dim HiLiteFont as QFont
dim HiLiteFont1 as QFont
HiLiteFont.bold=0
HiLiteFont.Name="FixedSys"


dim ClrSchComBoxList as QStringList 'должен быть глобальный



'!!! ----- цвета подсветки ---
defint HLColor (0 TO 15):  for i=0 to  15 : HLColor(i)=0: next i
HLColor(0)=&HB5E6EC ' желтоватый
HLColor(1)=&HFF0000 ' синий 
HLColor(2)=&HFF0082
HLColor(3)=&HFF0000
HLColor(4)=&HFF0000
HLColor(5)=&HFF0000
HLColor(6)=&H00BE00
HLColor(7)=&H0077F9
HLColor(8)=&H0000FF ' красный
HLColor(9)=0
HLColor(10)=&HF9E7FF
HLColor(11)=clBlue
HLColor(12)=&HFFF2F1
HLColor(13)=&HE2ECf8
HLColor(14)=&HE2ECB8


'!!! ----- цвета темы ---
defint TmColor (0 TO 15):  for i=0 to  15 : TmColor(i)=0: next i
TmColor(0)=&HB5E6EC
TmColor(1)=&HFF0000
TmColor(2)=&HFF0082
TmColor(3)=&HFF0000
TmColor(4)=&HFF0000
TmColor(5)=&HFF0000
TmColor(6)=&H00BE00
TmColor(7)=&H0077F9
TmColor(8)=&H0000FF
TmColor(9)=0
TmColor(10)=&HF9E7FF
TmColor(11)=clDBlue
TmColor(12)=&HFFF2F1
TmColor(13)=&HE2ECf8
TmColor(14)=&HE2ECB8

'!!! -----end  HiLight ----------

DECLARE FUNCTION GetFocus Lib "user32" Alias "GetFocus"() AS LONG
DECLARE FUNCTION Setfocus Lib "user32" Alias "SetFocus"(hwnd As Long) AS LONG

defint SrcEditLineCount ' число строк в редакторе

'!!! ----- popup menu -------
defint MaskBoxHdl, SearchEditHdl

declare sub SearchBlock '(Sender as QRichEdit)
declare sub ClearAllRich ' (Sender as QRichEdit)
declare sub ToLoverCAse '(Sender as QRichEdit)
declare sub ToUpperCAse '(Sender as QRichEdit)


'declare SUB FindOnClick 
declare SUB FindText (Sender as QButtonXP)'

declare sub ChangeFontBtnClick

declare sub AddSubs
declare sub GoSubs
NOPAINT=0
NOPAINT1=0

defint SrcEditLen, Modiflg, NeedSubRefresh 'размер текста изменился

'DECLARE SUB TabChange
declare sub ObjTabChange
declare sub PropertTabChange

declare sub SearchItOnClick
declare sub RunItOnClick
declare sub CompileOnClick
declare sub CompDebugOnClick            
declare sub CompDebugOnClick1            
declare sub CompByteOnClick
declare sub CMDLParamOnClick

declare sub RunOnClick
declare sub PauseOnClick
declare sub ContinueOnClick
declare sub StepOverOnClick
declare sub StepInToOnClick
declare sub RunToCursOnClick
declare sub AddWatchOnClick
declare sub DelWatchOnClick

declare sub DelAllWatchOnClick

declare sub ToggleBreakPointOnClick
declare sub ClearAllBreakPointsOnClick
declare sub  FormCloseOnClick
declare sub EvalModifOnClick


declare sub WatchListFormResize
declare sub SubListFormResize
declare sub ObjInspectorFormResize
Declare Sub LangManagerOnClick


declare sub CreateFileOnClick
declare sub SaveFileOnClick
declare sub SaveAsOnClick
declare sub SaveAs
declare sub SaveFile

declare sub DeleteFileOnClick
declare sub SavePrjFileOnClick
declare sub SavePrjAsOnClick
declare sub DeletePrjOnClick

declare sub exitProgOnClick
declare sub OpenPrjSrcOnClick
declare sub  OpenDebugSrcOnClick
declare sub OpenPrjFrmOnClick
declare sub ObjInspectorOnClick
declare sub SubsListOnClick
declare sub WatchListOnClick
declare sub DirTreeOnClick (Sender as QButtonXP)


declare sub Help1
declare sub Help2
Declare Sub ArduinoCLIHelpClick
'declare  SUB Check(key AS BYTE)               

declare sub exitProg
declare sub DeleteFile
'declare sub LoadIni
declare sub OpenProject
declare sub OptionsPrj
Declare Sub UtilOnClick
'declare sub ClearGrd 
declare sub  SaveIni



declare sub DirTreeFormResize
declare sub DirTreeFormOnClose
declare sub CheckDir (Key as Byte)
declare sub GotoHotDir
Declare Sub DirTreeFormShow
declare sub BtnDelHotDirClick
declare sub BtnAddHotDirClick
'DECLARE   SUB MaskChange
'DECLARE   SUB MaskDel
'DECLARE   SUB MaskSave
DECLARE   SUB FileLoad

'declare FUNCTION vidnum$ (Num, dig%) as string
declare sub PopUpMe
'declare sub StopS
'declare function TimeString (TimSec as integer) as string

declare SUB RichShowXY
'declare sub R2Change

defstr filname$

'!!! menu  ---------'
dim Del AS QMENUITEM
dim BackSpace AS QMENUITEM
dim mnuSep31 AS QMENUITEM
mnuSep31.Caption = "-"

dim AddWatchMnu AS QMENUITEM
dim mnuSep3 AS QMENUITEM
mnuSep3.Caption = "-"

defstr RichRow1,  RichCol1, RichRow2,  RichCol2, RichRow3,  RichCol3

dim StrFont1 as QFont
StrFont1.name="FixedSys"

dim G2Font as QFont
G2Font.Size=18
G2Font.color=clRed
'StrFont.DelStyles(fsBold)
G2Font.name="FixedSys"'"MS Sans qwerty


dim StrFont as QFont
StrFont.Size=8
'StrFont.color=clRed
'StrFont.DelStyles(fsBold)
StrFont.name="FixedSys"'"MS Sans qwerty
DelWinFileName$=""
defstr SrcFileName, DebugString, FrmWndProc, AddInText, StartDir, ext
defstr IncFilesName$
StartPath$ = COMMAND$(0)-Application.ExeName    
IdePath$=StartPath$

StartDir=StartPath$ 'curdir$+"\"
mkdir StartPath$+"lang\"


'DIM OpenDialog AS QOpenDialog 
DIM OpenDialog AS QFILEDIALOG
OpenDialog.FileName=""

'rem 0
OpenDialog.Filter ="1.All Files *.*|*.*|_
2.Bas Files  *.bas;*.rqb;*.vbs|*.bas;*.rqb;*.vbs|_
3.Arduino Files *.ino;*.pde;*.h;*.c;*.cpp|*.ino;*.pde;*.h;*.c;*.cpp|_
4.Config Files *.yml;*.json|*.yml;*.json|_
5.Ini Files *.ini;*.prj|*.ini;*.prj|_
6.Inс Files *.inc;*.tpl|*.inc;*.tpl|_
7.Log Files *.log|*.log|_
8.Txt Files *.txt|*.txt|_
9.Html Files *.*htm*;*.js;*.vbs;*.css|*.*htm*;*.js;*.vbs;*.css|_
10.Hex Files *.Hex|*.Hex|_
11.Graphics Files *.ico;*.bmp|*.ico;*.bmp|_
12.Media Files *.*wav;*.mid;*.mp3|*.*wav;*.mid;*.mp3|" 
'erem

OpenDialog.FilterIndex = 1 
'12
OpenDialog.Caption = "QfileDialog"
OpenDialog.FileName= ""
OpenDialog.InitialDir = StartDir



DIM SaveDialog AS QSaveDialog 
'DIM SaveDialog  AS QFILEDIALOG

DIM FontDialog AS QFontDialog
FontDialog.name="FixedSys"



defint pauseOn, ButtonSizeH, ButtonSize, NumMsg, WatchList_Visible




brem 0
'!!!================
DIM fDialog as qfiledialog

with  fDialog
	.Filter ="All Files *.*|*.*|_
	Bas Files  *.bas;*.rqb;*.vbs|*.bas;*.rqb;*.vbs|_
	Arduino Files *.ino;*.pde;*.h;*.c;*.cpp|*.ino;*.pde;*.h;*.c;*.cpp|_
	Config Files *.yml;*.json|*.yml;*.json|_
	Ini Files *.ini;*.prj|*.ini;*.prj|_
	Inс Files *.inc;*tpl|*.inc;*tpl|_
	Log Files *.log|*.log|_
	Txt Files *.txt|*.txt|_
	Html Files *.*htm*;*.js;*.vbs;*.css|*.*htm*;*.js;*.vbs;*.css|_
	Hex Files *.Hex|*.Hex|_
	Graphics Files *.ico;*.bmp|*.ico;*.bmp|_
	Media Files *.*wav;*.mid;*.mp3|*.*wav;*.mid;*.mp3|" '_
	.FilterIndex = 1 
	
	fDialog.Caption = "QfileDialog test"
	fDialog.FileName= "test.txt"
	fDialog.InitialDir = StartDir
	IF fDialog.Execute THEN
		ShowMessage "file name & path =" + fDialog.FileName
		ShowMessage "file name no path =" + fDialog.FileTitle
	END IF
end with

'!!! =====
erem





ButtonSize=25
ButtonSizeH=25

dim dateR as string

'DIM FileIni AS QFileStream ' файл инициализации

'DIM FileBak AS QFileStream:
'DIM FileName AS QFileStream: ' текущий файл

dateR=mid$(date$, 4,3)+ mid$(date$, 1,3)+mid$(date$, 7,4)

DIM Timer1 AS QTimer

declare sub TimerOver 
Timer1.Interval = 400 
Timer1.Enabled = 0 'True 
Timer1.OnTimer = TimerOver 

DIM TimerOnce AS QTimer

declare sub TimerOnceOver 
TimerOnce.Interval = 400 
TimerOnce.Enabled = 1 'True 
TimerOnce.OnTimer = TimerOnceOver 



DECLARE SUB btnClick(sender as QButtonXP)
'DECLARE SUB Main

DECLARE SUB FrmMinimized (Sender as QForm)

LangBtnWidth=35


$Include  "RQSV105b2023-13Inc-1.inc"


CREATE LibMngForm AS QFORM
	Width = 800
	Height = 600
	Caption = "Library Manager"
	Center
	'FormStyle = fsStayOnTop
	BorderStyle=bsSizeToolWin 
	OnShow=LibMngFormShow
	
	
	CREATE LibMngLeftPanel AS QPANEL
		Left = 0
		Top = 2
		Width = 330
		Height = 28
		Color =&HA3C06F ' &HFFBBFF
		align=alleft
		
		CREATE LibMngListGrid AS QSTRINGGRID
			
			TabOrder = 8
			FixedRows = 1 
			FixedCols = 0 
			ColCount = 3 
			RowCount = 500 
			Align = alClient
			Height = 130
			'ColumnStyle(2) =gcsList '' gcsEllipsis †
			
			'ColumnList(2) = TypeList
			
			Separator=chr$(160)
			'AddOptions=goThumbTracking, goDrawFocusSelected, goColSizing  ,goEditing ' goRowMoving , goColMoving
			cell(0,0) = "#"
			cell(1,0) = "                Library name":
			'cell(1,0) = "From line":
			colWidths(1)=33' номер
			colWidths(1)=194' номер
			colWidths(2)=60' номер
			'OnDrawCell = LangGrid1DrawCell
			OnSelectCell = LibMngListGridSelectCell
			
			
		END CREATE
		
		
		
	END CREATE
	
	CREATE LibMngLogEdit AS QRICHEDIT
		align=albottom
		height=150
		
	END CREATE
	
	
	CREATE LibMngRichEd AS QRICHEDIT
		Font=StrFont
		Left = 5
		Top = 44
		Width = 59
		Height = 39
		Color =&HA3FFFA ' &HFFBBFF
		align=alclient
		plaintext=1
		Wordwrap=false
		HideSelection=false
		WantTabs=1
		'Font.Name="FixedSys"
		BorderStyle=1
		'ScrollBars = 3
		
		
	END CREATE
	
	
	CREATE LibMngPanel AS QPANEL
		Left = 0
		Top = 2
		Width = 667
		Height = 28
		Color =&HA3CFFF ' &HFFBBFF
		align=altop
		
		CREATE LibMngLabel1 AS QLABEL
			Left = 0
			Top = 0
			Width = 63
			Height = 400
			Align = alLeft
			Alignment = taCenter
			Caption = "Type"
		END CREATE
		CREATE LibMngTypeCombo AS QCOMBOBOX
			Left = 67
			Top = 1
			Height = 21
			Text = "Combo1"
			AddItems "udatable"
		END CREATE
		CREATE LibMngLabel2 AS QLABEL
			Left = 229
			Top = 0
			Width = 63
			Alignment = taCenter
			Caption = "Topic"
		END CREATE
		CREATE LibMngTopicCombo AS QCOMBOBOX
			Left = 297
			Top = 2
			Width = 105
			Height = 21
			Text = "Combo2"
		END CREATE
		CREATE LibMngSearchEdit AS QEDIT
			Left = 417
			Top = 2
			Width = 230
			Height = 20
			Text = "Filter your search"
		END CREATE
		
		CREATE StopItButton AS QBUTTON
			Caption = "StopIt !!!"
			Left = 660
			Top = 2
			OnClick=StopItOnClick
		END CREATE
		
		
	END CREATE
END CREATE

'DefInt OldWndProc1, OldWndProc2, OldWndProc3, OldWndProc4, OldWndProc5, OldWndProc6
'DefInt iTmpBind, iTmpBind1
'
'Bind iTmpBind To StringGridWndProc 
'OldWndProc3 = SetNewWndProc (LibMngListGrid.Handle, iTmpBind)
'print"OldWndProc3=";OldWndProc3

dim timerJ as qtimer
timerJ.Interval=1500
timerJ.ontimer=LoadJson
timerJ.enabled=0

dim JFakeList as QStringList
braceCount=0
jsontxt$=""
CurRow=1

flgSTOP=0
istatic=51

addr&=0



clicmd$=""
clicmdflg$=""
crow=1


CREATE ArdCliHelpForm AS QFORM
	Width = 480 
	Height = 550
	Caption = "Arduino Cli Help"
	'Center
	top=100
	left=1400
	FormStyle = fsStayOnTop
	
	onshow=ArdCliHelpFormShow
	'onclose=ArdCliHelpFormClose
	
	CREATE ArdCliHlpTopPanel AS QPANEL
		Left = 1
		Top = 1
		TabOrder = 8
		align=alTop
		color=clg
		height=70
		
		CREATE ArdCliCmdHelpLabel AS QLABEL
			Left = 10
			Top = 14
			Width = 108
			Caption = "Arduino Cli commands"
		END CREATE
		CREATE ArdCliCmdHelpCombo AS QCOMBOBOX
			Left = 124
			Top = 12
			Width = 300
			Height = 21
			font.name="JetBrains Mono"
			Text = "Select cli commmand..."
			AddItems _ 
			"board           Arduino board commands", _
			"burn-bootloader Upload the bootloader", _
			"cache           Arduino cache commands", _
			"compile         Compiles Arduino sketches", _
			"completion      Generates completion scripts", _
			"config          Arduino configuration commands", _
			"core            Arduino core operations", _
			"daemon          Run as a daemon on port 50051", _
			"debug           Debug Arduino sketches", _
			"help            Help about any command", _
			"lib             Arduino commands about libraries", _
			"outdated        Lists cores and libraries that can be upgraded", _
			"sketch          Arduino CLI sketch commands", _
			"update          Updates the index of cores and libraries", _
			"upgrade         Upgrades installed cores and libraries", _
			"upload          Upload Arduino sketches", _
			"version         Shows version number of Arduino CLI" 
			
			OnChange=ArdCliCmdHelpComboChange
		END CREATE
		
		CREATE ArdCliFlgHelpLabel AS QLABEL
			Left = 10
			Top = 42
			Width = 107
			Height = 22
			Caption = "Arduino Cli Flags"
			
		END CREATE
		CREATE ArdCliFlgHelpCombo AS QCOMBOBOX
			Left = 124
			Top = 40
			Width = 300
			Height = 28
			Text = "Select cli commmand flag..."
		END CREATE
	END CREATE
	
	CREATE ArdCliHlpGridPanel AS QPANEL
		Left = 1
		Top = 200
		height=200
		TabOrder = 8
		align=alTop 'client
		color=cly
		
		CREATE ArdCliHlpGrid AS QSTRINGGRID
			align=alclient
			TabOrder = 8
			FixedRows = 1 
			FixedCols = 1 
			ColCount = 5 
			RowCount = 5 
			Height = 130
			ColumnStyle(1) =gcsList '' gcsEllipsis
			ColumnList(1) ="" 
			ColumnStyle(2) =gcsList '' gcsEllipsis
			ColumnList(2) ="" 
			color=cla
			
			Separator=chr$(160)
			AddOptions=goThumbTracking, goDrawFocusSelected, goColSizing  ,goEditing ' goRowMoving , goColMoving
			
			cell(0,0) = "Command":
			cell(1,0) = "Child command":
			cell(2,0) = "Flags":
			'cell(3,0) = "":
			'cell(4,0) = "":
			colWidths(0)=80' номер
			colWidths(1)=160' номер
			colWidths(2)=160' номер
			colWidths(3)=-1' номер
			colWidths(4)=-1' номер
			'OnDrawCell = LangGrid1DrawCell
			'OnSelectCell = LangGrid1SelectCell
			OnListDropDown=ArdCliOnListDropDown
			OnSetEditText=ArdCliOnOnSetEditText
			
			
		END CREATE
		
	END CREATE
	Create splitter  as QSplitter
		align=altop 'bottom
		color=clr
		width=6
	END CREATE
	
	CREATE ArdCliHelpRichEd AS QRICHEDIT
		'CREATE LogEdit AS QRICHEDIT
		Align = alclient
		
		Top=200
		Height = 300
		'color=&HFFCFFF
	END CREATE
END CREATE






CREATE LangMngForm AS QFORM
	Caption = "Language manager"
	Width = 640
	Height = 480
	Center
	OnResize=LangMngFormResize
	AutoScroll=false
	OnClose=FrmMinimized
	CREATE LangMngPanel AS QPANEL
		align=alTop
		height=24
		showhint=1
		Create AddLang AS QCOOLBTN
			Align = 3
			caption="L+"
			'BMPHandle = New_BMP
			'Flat = 1
			Hint = "Add language"
			OnClick = AddLangOnClick1
			Width = LangBtnWidth
			visible=0
		End Create
		
		Create DelLang AS QCOOLBTN
			Align = 3
			'BMPHandle = Open_BMP
			caption="L-"
			'Flat = 1
			Hint = "Delete language"
			OnClick =DelLangOnClick
			Width = LangBtnWidth
			visible=0
		End Create
		
		Create SaveLang AS QCOOLBTN
			Align = 3
			BMPHandle = Save_BMP
			'Flat = 1
			Hint = "Create languge files"
			OnClick =SaveLangOnClick 
			Width = LangBtnWidth
		End Create
		
		Create BarL0 AS QCOOLBTN
			Align = 3
			Enabled = 0
			Flat = 0
			Width = 3
		End Create
		Create ScanSrcLangBtn AS QCOOLBTN
			Align = 3
			'BMPHandle = Save_BMP
			caption="Scan"
			'Flat = 1
			Hint = "Scan source for strings"
			OnClick =ScanSrcLang
			Width = LangBtnWidth
		End Create
		Create CreateLangBtn AS QCOOLBTN
			Align = 3
			'BMPHandle = Save_BMP
			caption="Create"
			'Flat = 1
			Hint = "Create language pack"
			OnClick =CreateLangPack
			Width = LangBtnWidth
		End Create
		Create ClearLangBtn AS QCOOLBTN
			Align = 3
			'BMPHandle = Save_BMP
			caption="Clear"
			'Flat = 1
			Hint = "Clear grids"
			OnClick =ClearLangGrids
			Width = LangBtnWidth
		End Create
		
		Create SwapColBtn as QCOOLBTN
			Align = 3
			'BMPHandle = Save_BMP
			caption="Swap Eng"
			'Flat = 1
			Hint = "Swap  cell"
			OnClick =SwapcellBtnOnClick
			Width = LangBtnWidth+20
		End Create
		Create LoadGridBtn as QCOOLBTN
			Align = 3
			'BMPHandle = Save_BMP
			caption="Load Grid"
			'Flat = 1
			Hint = "Load Grid"
			OnClick =LoadGridBtnOnClick
			Width = LangBtnWidth+20
		End Create
		
		Create DElRowGridBtn as QCOOLBTN
			Align = 3
			'BMPHandle = Save_BMP
			caption="Del Row"
			'Flat = 1
			Hint = "Del Row"
			OnClick =DElRowOnClick
			Width = LangBtnWidth+20
		End Create
		
		Create DElEmptyRowsBtn as QCOOLBTN
			Align = 3
			'BMPHandle = Save_BMP
			caption="Del EmptyRow"
			'Flat = 1
			Hint = "Del EmptyRow"
			OnClick =DElEmptyRowsOnClick
			Width = LangBtnWidth+20
		End Create
		Create EmptyLbl AS QLabel
			Align = 3
			Width = 5
		End Create
		
		Create SortBtn AS QCheckBox
			Align = 3
			caption="Sort"
			Hint = "Sort by current column"
			checked=1
			OnClick =SortBtnOnClick
			Width = LangBtnWidth+20
		End Create
		
		Create DelDupBtn AS QCheckBox 'QCOOLBTN
			Align = 3
			caption="Del Dup"
			Hint = "Delete duplicates"
			checked=1
			OnClick =DelDupBtnOnClick
			Width = 60
		End Create
		
		Create SrcLangEdit AS QEdit 'panel 'Label
			Align = alRight
			'caption="English"
			text="English"
			font.bold=1
			left=101
			color=cly
			width=56
			OnKeyUp=SrcLangEditOnKeyUp
		End Create
		
		Create LangNameCBox AS QButtonXP 'CheckBox 'QComboBox
			caption="Set Source"
			Align = alRight
			OnClick =SetDefLng
			left=100
			width=65
			'Hint = "Undo"
			XP=1
		End Create
		
		
	END CREATE
	CREATE LangPanel AS QPANEL
		Align = alClient
		
		
		CREATE LangMngGridPanel AS QPANEL
			Align = alClient
			CREATE LangGrid1 AS QSTRINGGRID
				top=5
				Align = alClient 
				FixedRows = 1 
				FixedCols = 0 
				ColCount = 5 '9 
				RowCount = 5
				DefaultColWidth = 220
				DefaultRowHeight = 18
				enabled=true
				Separator=chr$(160)
				AddOptions=goThumbTracking, goDrawFocusSelected, goColSizing, goRowMoving , goColMoving' ,goEditing
				
				cell(0,0) = "Use":
				cell(1,0) = "From line":
				cell(2,0) = "Value#":
				cell(3,0) = "English":
				cell(4,0) = "Russian":
				'cell(5,0) = "French":
				'cell(6,0) = "Italian":
				'cell(7,0) = "Germany":
				'cell(8,0) = "Spanish":
				colWidths(0)=18' номер
				colWidths(1)=60' номер
				colWidths(2)=80' номер
				'colWidths(2)=550 'поиск'
				OnDrawCell = LangGrid1DrawCell
				OnSelectCell = LangGrid1SelectCell
				
			END CREATE
			
			CREATE LangGrid1H AS QSTRINGGRID '-- теневая таблица !!!
				top=5
				'Align = alClient 
				FixedRows = 1 
				FixedCols = 0 
				ColCount = 5 '9 
				RowCount = 5
				DefaultColWidth = 120
				DefaultRowHeight = 20
				enabled=true
				Separator=chr$(160)
				'AddOptions=goThumbTracking, goDrawFocusSelected, goColSizing, goRowMoving , goColMoving' ,goEditing
				
				cell(0,0) = "Use":
				cell(1,0) = "From line":
				cell(2,0) = "Value#":
				cell(3,0) = "English":
				cell(4,0) = "Russian":
				'cell(5,0) = "French":
				'cell(6,0) = "Italian":
				'cell(7,0) = "Germany":
				'cell(8,0) = "Spanish":
				colWidths(0)=25' номер
				colWidths(1)=80' номер
				colWidths(2)=80' номер
				'colWidths(2)=550 'поиск'
				'OnDrawCell = LangGrid1DrawCell
				'OnSelectCell = LangGrid1SelectCell
				visible=0
			END CREATE
		END CREATE ' gridPanel
		
		CREATE SplitterLang AS QSPLITTER '-- Note position
			Align = alBottom 'Top            '-- Fill middle alBottom '
			Cursor = crVSplit
			Height = 5
			color=clRed
			'OnMoved=Splitter2Moved
		END CREATE
		
		
		CREATE LangMngSearchPanel AS QPANEL
			Align =alBottom' Client
			height=200'LangPanel.height/2
			
			CREATE LangGrid2 AS QSTRINGGRID
				Align = alClient 
				AddOptions(goEditing) 
				FixedRows = 1 
				FixedCols = 0 
				ColCount = 4 
				RowCount = 5
				DefaultColWidth = 80
				DefaultRowHeight = 20
				enabled=true
				AddOptions=goThumbTracking, goDrawFocusSelected, goColSizing
				Separator=chr$(160)
				cell(0,0) = "№":
				cell(1,0) = "Line №":
				cell(2,0) = "Literal":
				cell(3,0) = "Data":
				colWidths(0)=40' номер
				colWidths(1)=40' номер
				colWidths(2)=140' номер
				colWidths(3)=400 'LangGrid2.Width -colWidths(0)-5'  550 'поиск'
				OnDrawCell = LangGrid2DrawCell
				OnSelectCell = LangGrid2SelectCell
				OnSetEditText=LangGrid2SetEditText
				
			END CREATE
			
		END CREATE 'lang search panel 
		
	END CREATE 'lang panel 
	
	
END CREATE


CREATE DirTreeForm AS QForm  '============== DirTreeForm ================='
	Caption = "Directory Tree"
	''  Center
	top=60
	left=250
	height=450
	FormStyle = fsStayOnTop
	FormStyle = fsStayOnTop
	BorderStyle=bsSizeToolWin 
	
	OnResize=DirTreeFormResize
	AutoScroll=false
	OnKeyPress = CheckDir 
	OnClose=FrmMinimized
	OnShow=DirTreeFormShow
	color=&h9E8865
	
	CREATE DirTree AS QDirTree
		'DelDriveTypes drtCDRom 
		Align=center
		Left = 4
		top=60
		'InitialDir = curdir$'"c:\"
		Width =DirTreeForm.ClientHeight -5
		Height =DirTreeForm.ClientHeight -65
		OnChange =ChangeDirectory1
		TabOrder=1
		ShowHint=false
		font.color=clDred
		'font.name="Terminal"
		font.Charset=OEM_CHARSET
		'astLoad=1
	END CREATE
	CREATE LblHotDir AS QLABEL
		Caption = "Hot folders":
		Left = 4:Top = 4:
		Transparent = 0
		Width =200
		''                   color=clBrown
	END CREATE
	CREATE BtnAddHotDir AS QButtonXP     
		XP=1
		
		Caption = "+"         
		Left = 220         
		Top = 4         
		Width = 15         
		Height = 15     
		OnClick = BtnAddHotDirClick
		Spacing=1
	END CREATE
	
	CREATE BtnDelHotDir AS QButtonXP           
		XP=1
		Caption = "-"         
		Left = 240         
		Top = 4         
		Width = 15         
		Height = 15     
		OnClick = BtnDelHotDirClick
		Spacing=1
	END CREATE
	CREATE CBoxHotDir AS QCOMBOBOX
		Text =StartDir 'curdir1$ 'curdir$' DirTree.InitialDir '"c:\"
		Left = 4
		Top = 25
		Width = DirTreeForm.ClientWidth -5
		Height = 99
		'DropDownCount = 9
		'Sorted = 1
		TabOrder = 10
		'AddItems "c:\", "c:\temp"
		OnChange=GotoHotDir
	END CREATE
	
END CREATE 'dirform'

'print 627," ",timer

CREATE DebugSrcForm AS QForm '================================================
	Caption = "DebugSrc"
	top=200
	left=300
	height= 350
	Width =    500
	'FormStyle = fsStayOnTop
	''  OnResize=DirTreeFormResize
	OnClose=FrmMinimized
	'WindowState
	AutoScroll=false
	'KeyPreview = 1        'this line does the trick
	''  OnKeyPress = CheckDir 
	''  color=&hdEd8d5
	
	CREATE  ~SrcD as QRICHEDIT ' в ~SrcD - операторы в нем мы будем подсвечивать при пошаговой отладке
		visible=0
		Wordwrap=false
		plaintext=1
	END CREATE
	
	CREATE DebugSrcEdit AS QRICHEDIT '=== редактор файла исходника c дебаггером =====
		Left =2
		Top = 2
		Height =DebugSrcForm.ClientHeight -5
		Width = DebugSrcForm.ClientWidth  -5
		
		ScrollBars = 3
		TabOrder = 10
		AddStrings "--- source&debug ---"
		Wordwrap=false
		color=&hc2d7ce'cl
		font.name="Courier"   
		''        Font.Color=clBlue
		''       OnChange=R1Change
		HideSelection=false
		''      OnKeyPress=R2Change
		'OnMouseMove =RichShowXY 'R2Change
		'OnKeyUp =RichShowXY 'R2Change'' '
		tag=32
		plaintext=1
	END CREATE '- DebugSrcEdit'
	
end create 'DebugSrcForm 
CREATE WaitForm AS QFORM
	Caption = "Wait please..."
	Width = 120
	Height = 150
	Center
	'color=clr
	'font.size=18
	CREATE WaitLabel AS QLABEL
		Caption = "----------"
		Left =6
		Top = 60
		Transparent = 1
		autosize=1
		visible=0
	END CREATE
	CREATE Gauge2 AS QGAUGE
		Left =1
		Top = 1
		Width = 120
		Height = 120
		align=alclient
		Kind = gkPie
		BorderStyle=0
		visible=1
		ForeColor=clb
		Font.NAme="Arial"
		Font.size=22
		Font.Color=clb
	END CREATE
	
	
	'OnClose=FrmMinimized
END CREATE

CREATE CmdLineForm AS QFORM
	Caption = "Command line parameters"
	Width = 420
	Height = 240
	Center
	OnClose=FrmMinimized
	CREATE CmdLineEdit AS QEDIT
		Text = ""
		Left = 7
		Top = 7
		width=400
		color=&H9DE9FD
	END CREATE 
	'CREATE CMDLineButton AS QButtonXP
	'        Caption = "OK"
	'        Left = 4
	'        Top = 34
	'        'TabOrder = '
	'onclick=FrmMinimized
	'    END CREATE
	
END CREATE
CREATE SearchListForm AS QFORM
	Caption = "List of lines"
	Width = 300
	Height = 480
	left=1'700
	top=25'150
	'Center
	'OnClose=FrmMinimized
	FormStyle = fsStayOnTop
	BorderStyle=bsSizeToolWin 
	onclose=SearchListFormClose
	'DelBorderIcons  (biMinimize , biMaximized, biSystemMenu  ) ', biSystemMenu 
	
	CREATE SEarchListGrid AS QSTRINGGRID
		'Left = 5
		'Top = 5
		'height=400
		'width=650
		align=alclient
		FixedRows = 1 
		FixedCols = 1 
		ColCount = 3 
		RowCount = 5 
		DefaultColWidth = 80
		DefaultRowHeight = 20
		enabled=true
		AddOptions=goThumbTracking, goDrawFocusSelected, goColSizing
		cell(0,0) = "Line N":
		cell(1,0) = "Use":
		colWidths(0)=40' номер
		colWidths(1)=25' галочка
		colWidths(2)=550 'поиск'
		OnDrawCell = ListGridDrawCell
		OnSelectCell = ListSelectCell
	END CREATE
	CREATE PanelSEarchList AS QPANEL
		height=50
		align=alBottom'alclient
		CREATE DisRemBtn AS QCheckBox
			Caption = "Disable Rem"
			Left = 4
			Top = 4
			OnClick=DisRemBtnOnClick
		END CREATE
		
	END CREATE
	
END CREATE



CREATE NewForm AS QForm '=================================================
	Caption = "NewForm"
	top=200
	left=300
	height= 350
	Width =    500
	FormStyle = fsStayOnTop
	''  OnResize=DirTreeFormResize
	OnClose=FrmMinimized
	'WindowState
	AutoScroll=false
	' KeyPreview = 1        'this line does the trick
	''  OnKeyPress = CheckDir 
	''  color=&hdEd8d5
	
end create

'========================================================================
declare sub NewProjectBoxClose (Sender as QButtonXP)                              

CREATE NewProjectBox AS QFORM
	Caption = "Project"
	Width = 338
	Height = 217
	Center
	OnClose=FrmMinimized
	
	CREATE PrjNameLbl AS QLABEL
		Caption = "Project Name"
		Left = 4
		Top = 22
		Width = 120
		Transparent = 1
	END CREATE
	CREATE MainModuleLbl AS QLABEL
		Caption = "Main Module"
		Left = 3
		Top = 73
		Width = 61
		Transparent = 1
	END CREATE
	CREATE MMpathBtn1 AS QCOOLBTN
		Caption = "..."
		Left = 252
		Top = 97
		Width = 22
		Height = 22
		OnClick = MainModulePathClick
	END CREATE
	CREATE PrjNameEdit AS QEDIT
		Text = ""
		Left = 5
		Top = 40
		Width = 265
	END CREATE
	CREATE MMpathEdit AS QEDIT
		Text = ""
		Left = 5
		Top = 99
		Width = 241
		TabOrder = 1
	END CREATE
	CREATE OKBtn AS QButtonXP
		XP=1
		'Caption = "Button1"
		Left = 13
		Top = 145
		TabOrder = 10
		kind=1
		onclick =NewProjectBoxClose
		
	END CREATE
	CREATE CancelBtn AS QButtonXP
		XP=1
		'Caption = "Button2"
		Left = 108
		Top = 145
		TabOrder = 11
		kind=bkCancel
		onclick =NewProjectBoxClose
		'ModalResult=mrCancel
	END CREATE
END CREATE

'----------------------------------------------------------------

CREATE SearchFrm as QForm '=======================================
	Caption = "Search"
	top=30'00
	height=80'90'120
	left=750'0
	Width = 230'315'350   
	FormStyle = fsStayOnTop
	'BorderStyle =bsToolWindow 
	''  OnResize=DirTreeFormResize
	OnClose=FrmMinimized
	AutoScroll=false
	'KeyPreview = 1        'this line does the trick
	''  OnKeyPress = CheckDir 
	''  color=&hdEd8d5
	visible=0
end create      '- SearchFrm

CREATE ColorDialogFrm AS QForm
	Left =5
	Top = 60
	'FormStyle=fsStayOnTop 
END CREATE


CREATE PrjPropert AS QForm '= '== форма настроек IDE ===============================================
	Caption = "IDE Options"
	Center
	height=550
	Width = 500   
	'FormStyle = fsStayOnTop
	OnClose=SaveIDEOptions
	AutoScroll=false
	'font.color=clsilver
	'KeyPreview = 1        'this line does the trick
	'color=clblack'darkcolor
	BorderStyle=bsSizeToolWin 'bsNone '
	
	CREATE PrjPropertStatBar AS QSTATUSBARex
		AddPanels "" ', "Col ", "Pos ","Asc ", "EOL ",""
		Panel(0).Width = 105
		
		
	END CREATE
	
	CREATE PropertTab AS QTabControl
		AddTabs "Editor", "Colors", "Directories", "Compiler", "Debugger", "VersionInfo", "Localization", "Arduino"
		top=1
		Left = 1
		Width = PrjPropert.ClientWidth 
		Height = PrjPropert.ClientHeight 
		OnChange = PropertTabChange
		'HotTrack = True
		align=alclient
		TabIndex=7
		TabInactiveColor=clgrey'darkcolor
		'font.color=cls
		
		'color=darkcolor
		
		CREATE PropPanelEditor AS QPanel '"Editor"
			'Top = 24
			'Left = 5
			'Width = PropertTab.ClientWidth - 10
			'Height = PropertTab.ClientHeight - 30
			align=alclient
			Caption = ""
			''      Visible = False
			BorderStyle=bpNone 
			BevelInner =bvnone' bvLowered
			BevelWidth=0
			'color=darkcolor
			
			CREATE EditOptGrBox AS QGroupBox
				Caption = "Editor options:  "
				top=5
				Left = 5
				Width = PropertTab.ClientWidth - 20
				Height = 40
				'color=darkcolor
				align=altop
				
				create SyntaxHLBox as QCheckBox
					Caption="Use syntax highlight"
					Left =7
					Top = 15
					Width =150
					'font.color=clDBlue
					checked=true
					OnClick=SyntaxHLBoxOnClick
				end create
				
				create HtHLBox as QCheckBox
					Caption="Use HotTabs highlight"
					Left =170
					Top = 15
					Width =150
					'font.color=clDBlue
					checked=true
					OnClick=HtHLBoxClick
					
				end create
			END CREATE '- EditOptGrBox'
			
			CREATE ChClrGrBox AS QGroupBox
				Caption = "Color
				top=50
				Left = 5
				Width = PropertTab.ClientWidth - 20
				Height = 260
				align=alclient
				'font.bold=1
				'font.Name="Arial"
				
				CREATE PanelClrSch AS QPANEL
					Left = 1
					Top = 1
					'Caption = "Panel1"
					TabOrder = 8
					align=alTop
					'color=clm'darkcolor
					Height = 22
					BorderStyle=bpNone 
					BevelWidth=0
					BevelOuter=0
					BevelInner=0 
					font.bold=0
					
					
					create ClrSchLbl as QPanel'QLabel
						caption="Color Schem "
						'top =20
						left=1
						Width =75    
						align=alleft
						color=cla
						Alignment=taCenter 'taRightJustify 
						'Layout=tlBottom 'tlCenter 
						BorderStyle=0
						BevelWidth=0
						BevelOuter=0
					end create
					
					
					
					
					create ClrSchComBox as QComboBox
						Left = 120
						Top = 4
						Width =125' PropertTab.ClientWidth -150
						'Height = 99
						left=155
						'DropDownCount = 9
						'Sorted = 1
						TabOrder = 10
						AddItems "Classic Windows", "Dark","NightVision"
						OnChange=ThemeChange
						align=alleft 'client
						Style=csDropDownList 
						'color=clblack 'darkcolor
						text="Classic Windows"
						
					end create
					CREATE ClrSchEdit AS QEDIT
						Text = ClrSchComBox.text
						Left = ClrSchComBox.left
						Top = ClrSchComBox.top
						height=ClrSchComBox.height+6
						Width =ClrSchComBox.width-15
						OnKeyPress=ClrSchEditOnKeyPress
						'align=alleft
						color=cly
					END CREATE
					
					
					CREATE DelSchemBtn AS QButtonXP
						XP=1
						Caption = "Delete"
						align=alleft
						Left = 254
						width=40
						Font.color=clr
						OnClick=DelSchemBtnOnClick
					END CREATE
					
					CREATE AddSchemBtn AS QButtonXP
						XP=1
						Caption = "Add"
						align=alleft
						Left = 214
						width=30
						Font.color=cldg
						OnClick=AddSchemBtnOnClick
					END CREATE
					
					
					CREATE PanSpc1 AS QPANEL
						align=alleft
						width=12
						Left = 294
						visible=1
						'color=cly
						bevelwidth=0
					END CREATE
					
					
					CREATE RadioBLite AS QRADIOBUTTON
						Caption = "IDE Light"
						Left = 364
						width=80
						TabOrder = 8
						align=alleft
						Font.color=clr
						checked=1
						
					END CREATE
					CREATE RadioBDark AS QRADIOBUTTON
						Caption = "IDE Dark"
						Left = 464
						width=80
						TabOrder = 9
						align=alleft
						Font.color=clb
						
					END CREATE
					
					
					
				END CREATE
				
				CREATE TypeListGrid AS QSTRINGGRID '----- keywords -----'
					Left = 5
					Top = 40
					Height = 210
					Width = 250
					ScrollBars = 0
					ColCount = 3
					RowCount = 17
					Row = 0
					DefaultRowHeight = 18
					FixedRows = 1
					FixedCols = 0
					ScrollBars=2
					align=alclient
					'color=darkcolor
					'FixedColor=darkcolor'&H28351A
					'Font.name="FixedSys"
					BorderStyle=0
					'Font.color=clred
					'GridLineColor=clr
					
					AddOptions(goEditing,goThumbTracking )',goRowSelect )
					ColWidths(0)=100
					ColWidths(1)=100
					ColWidths(2)=80
					'ColumnStyle(2) =gcsList '' gcsEllipsis
					' ColumnList(1) = "Заменить"+chr$(10)+_
					'"Удалить"+chr$(10)
					
					'ColumnList(2) = TypeList
					cell(0,0)=" Name"
					cell(1,0)=" Font color"
					cell(2,0)=" BG Color"
					
					cell(0,1)=" BackGroung"
					cell(0,2)=" KeyWords"
					cell(0,3)=" Operators"
					cell(0,4)=" Directives"
					cell(0,5)=" Properties"
					cell(0,6)=" Types"
					cell(0,7)=" Comments"
					cell(0,8)=" Strings"
					cell(0,9)=" Numbers"
					cell(0,10)=" Text"
					cell(0,11)=" GutterBG"
					cell(0,12)=" GutterTxt"
					cell(0,13)=" LogEditBG"
					cell(0,14)=" Custom1"
					cell(0,15)=" Custom2"
					cell(0,16)=" Custom3"
					
					
					'------------------------------------'
					OnDrawCell=HLDrawCell
					OnSelectCell=HLSelectCell 
					'OnSetEditText=NewTypeNAme
					
				END CREATE
				CREATE RichBox AS QRichEdit'_xt'QLISTBOX
					Left =TypeListGrid.Left+TypeListGrid.Width+5'  180
					Top = 40
					Width = 180
					''            Width = 300
					Height = 210
					TabOrder = 1
					font.name="FixedSys"
					ScrollBars=ssVertical 
					align=alright
					'color=HLColor(0)
					'OnChange=RichBoxOnChange
					visible=0
					
				END CREATE
				
			END CREATE '- ChClrGrBox'
			
			CREATE FontGrBox AS QGroupBox
				Caption = "Font"
				top=325'250
				Left = 5
				Width = PropertTab.ClientWidth - 20
				Height = 50
				align=albottom
				
				create FontLbl as QLabel
					caption="AaBbCcDd / АаБбВв"
					top =20
					left=10
					Width = 200
				end create
				
				CREATE ChangeFontBtn AS QButtonXP  'Qpanel '       
					XP=1
					Caption = "Change"         
					Left = PropertTab.ClientWidth - 150         
					Top = 20         
					OnClick = ChangeFontBtnClick
					'Width = 100'PropertTab.ClientWidth - 10
					'Height =24' PropertTab.ClientHeight - 30
					font.color=0
					
				END CREATE
				
			END CREATE '- FontGrBox'
			
		END CREATE
		
		CREATE PropPanel1 AS QPanel '-- colors
			Top = 24
			Left = 5
			Width = 100'PropertTab.ClientWidth - 10
			Height =100' PropertTab.ClientHeight - 30
			align=alclient
			Caption = "colors"
			BevelInner = bvLowered
			BorderStyle=bpNone 
			'color=darkcolor
			BorderStyle=bpNone 
			BevelInner =bvnone' bvLowered
			BevelWidth=0
			
			
			
			Visible = False
		END CREATE
		
		'------------------------------
		CREATE PropPanel2 AS QPanel '!!! -- directories
			Top = 24
			Left = 5
			Width = PropertTab.ClientWidth - 10
			Height = PropertTab.ClientHeight - 30
			BevelInner = bvLowered
			Visible = False
			color=cly
			align=alclient
			
			CREATE RQPanel AS QPanel '-- directories
				Top = 3
				Left =3
				Width = PropPanel2.ClientWidth - 4
				Height =PropPanel2.ClientHeight/2 - 5
				color=&HDBB6C8 
				
				CREATE RQLabel AS QLABEL
					Caption = "Rapid-Q compiler"
					Left = PropPanel2.ClientWidth/2 - 84
					Top = 4
					Transparent = 1
					font.size=14
					font.bold=1
					UpperColor=&HFFD2AE '&HB6A3FF
					font.color=&H306200
					font.name="Arial"
					LabelStyle=lsRaised 
					
				END CREATE
				CREATE UseIDEPathChB AS QCHECKBOX
					Caption = "Use IDE path"
					Left = 8
					Top = 35
					checked=1
					TabOrder = 8
				END CREATE
				
				
				CREATE RQPathPanel AS QPanel '-- directories
					'Top = 44
					Left =3
					align=alBottom
					'Width = PropPanel2.ClientWidth - 4
					Height = 170 'PropPanel2.ClientHeight/2 - 5
					color=cla '&HDBB6C8 
					showhint=1
					
					CREATE IDEPathLbl AS QLABEL
						Caption = "IDE path":
						Left = 8:Top = 3:
						Transparent = 0
						Width = 50
					END CREATE
					
					CREATE IDEPathEdit AS QEDIT         
						Left = 70         
						Top =IDEPathLbl.Top
						Width = 370
						text=StartPath$ 'curdir$
					END CREATE
					
					CREATE IDEDirBtn AS QButtonXP
						XP=1
						Caption = "..."
						Left =445: Top = IDEPathLbl.Top
						'Spacing=2:'bmpHandle = sOpen 
						Width = 20:Height = 20
						tag=300: ' !!! IDEPath
						font.name= "system"
						hint="Ide path"
						OnClick = DirTreeOnClick
					END CREATE
					
					CREATE CompPathLbl AS QLABEL
						Caption = "Compiler":
						Left = 8:Top = 30:
						Transparent = 0
						Width = 50
					END CREATE
					
					CREATE CmpPathEdit AS QEDIT         
						Left = 70         
						Top =CompPathLbl.Top
						Width = 370
						text=StartPath$ 'curdir$
					END CREATE
					
					CREATE CompDirBtn AS QButtonXP
						XP=1
						Caption = "..."
						Left =445: Top = CompPathLbl.Top
						'Spacing=2:'bmpHandle = sOpen 
						Width = 20:Height = 20
						tag=301: ' !!! CompPath
						font.name= "system"
						hint="Compiler path"
						OnClick = DirTreeOnClick
					END CREATE
					
					CREATE IncPathLbl AS QLABEL
						Caption = "Include":
						Left = 8:Top = CompPathLbl.Top+20:
						Transparent = 0
						Width = 50
					END CREATE
					
					CREATE IncPathEdit AS QEDIT         
						Left = 70         
						Top =IncPathLbl.Top
						Width = 370
						text=StartPath$+"inc\"
					END CREATE
					
					CREATE IncDirBtn AS QButtonXP
						XP=1
						Caption = "..."
						Left =445: Top = IncPathLbl.Top
						'Spacing=2:'bmpHandle = sOpen 
						Width = 20:Height = 20
						tag=302: ' !!! IncPath
						font.name= "system"
						OnClick = DirTreeOnClick
					END CREATE
					
					CREATE LibPathLbl AS QLABEL
						Caption = "Library":
						Left = 8:Top = CompPathLbl.Top+40:
						Transparent = 0
						Width = 50
					END CREATE
					
					CREATE LibPathEdit AS QEDIT         
						Left = 70         
						Top =LibPathLbl.Top
						Width = 370
						text=StartPath$+"lib\"
					END CREATE
					
					CREATE LibDirBtn AS QButtonXP
						XP=1
						Caption = "..."
						Left =445: Top = LibPathLbl.Top
						'Spacing=2:'bmpHandle = sOpen 
						Width = 20:Height = 20
						tag=303: ' !!! LibPath
						font.name= "system"
						OnClick = DirTreeOnClick
					END CREATE
					'------------------------------
					
					CREATE PrjPathLbl AS QLABEL
						Caption = "Project":
						Left = 8:Top = CompPathLbl.Top+60:
						Transparent = 0
						Width = 50
					END CREATE
					
					CREATE PrjPathEdit AS QEDIT         
						Left = 70         
						Top =PrjPathLbl.Top
						Width = 370
						text=StartPath$+"projects\"
					END CREATE
					
					CREATE prjDirBtn AS QButtonXP
						XP=1
						Caption = "..."
						Left =445: Top = PrjPathLbl.Top
						'Spacing=2:'bmpHandle = sOpen 
						Width = 20:Height = 20
						tag=304:' !!! PrjPath
						font.name= "system"
						OnClick = DirTreeOnClick
					END CREATE
					'------------------------------
					CREATE TplPathLbl AS QLABEL
						Caption = "Templates":
						Left = 8:Top = CompPathLbl.Top+80:
						Transparent = 0
						Width = 50
					END CREATE
					
					CREATE TplPathEdit AS QEDIT         
						Left = 70         
						Top =TplPathLbl.Top
						Width = 370
						text=StartPath$+"templates\"
					END CREATE
					
					CREATE TplDirBtn AS QButtonXP
						XP=1
						Caption = "..."
						Left =445: Top = TplPathLbl.Top
						'Spacing=2:'bmpHandle = sOpen 
						Width = 20:Height = 20
						tag=305:' !!! TplPath
						font.name= "system"
						OnClick = DirTreeOnClick
					END CREATE
					'----------------------------
					CREATE IcoFileLbl AS QLABEL
						Caption = "App Icon":
						Left = 8:Top = CompPathLbl.Top+100:
						Transparent = 0
						Width = 50
					END CREATE
					
					CREATE IcoFileEdit AS QEDIT         
						Left = 70         
						Top =IcoFileLbl.Top
						Width = 370
						text=StartPath$+"default.ico"
					END CREATE
					
					CREATE IcoFileBtn AS QButtonXP
						XP=1
						Caption = "..."
						Left =445: Top = IcoFileLbl.Top
						'Spacing=2:'bmpHandle = sOpen 
						Width = 20:Height = 20
						tag=307: ' !!! IcoPath
						font.name= "system"
						OnClick =ChooseIcon 'DirTreeOnClick
					END CREATE
				END CREATE 'RQPath Panel
			END CREATE 'RQ Panel
			
			CREATE FBPanel AS QPanel '-- directories
				Top = PropPanel2.ClientHeight/2 
				Left =3
				Width = PropPanel2.ClientWidth - 4
				Height = 80 'PropPanel2.ClientHeight/2 - 4
				color=&HDBB69B 
				
				CREATE FBLabel AS QLABEL
					Caption = "FreeBasic compiler"
					Left = PropPanel2.ClientWidth/2 - 84
					Top = 4
					Transparent = 1
					font.size=16
					font.bold=1
					font.name="Arial"
					'LowerColor=cldr
					UpperColor=&HFFD2AE '&HB6A3FF
					font.color=&H5C066F
					LabelStyle=lsRaised 
				END CREATE
				
				CREATE FBCompPathLbl AS QLABEL
					Caption = "FB Compiler":
					Left = 8:Top = 30:
					Transparent = 0
					Width = 60
				END CREATE
				
				CREATE FBCmpPathEdit AS QEDIT         
					Left = 70         
					Top =FBCompPathLbl.Top
					Width = 370
					text="C:\Program Files\FreeBASIC\" 'curdir$
				END CREATE
				
				CREATE FBCompVerLbl AS QLABEL
					Caption = "FB Compiler version":
					Left = 8:Top = 60:
					Transparent = 0
					Width = 60
					visible=0
					
				END CREATE
				
				CREATE FBCmpVerEdit AS QComboBox         
					Left = 70         
					Top =FBCompVerLbl.Top
					Width = 80
					text="0.16"
					additems "0.12","0.15","0.16","0.17","0.18"
					visible=0
					
				END CREATE
				
				CREATE FBCompDirBtn AS QButtonXP
					XP=1
					Caption = "..."
					Left =445: Top = FBCompPathLbl.Top
					'Spacing=2:'bmpHandle = sOpen 
					Width = 20:Height = 20
					tag=1300:font.name= "system"
					OnClick = DirTreeOnClick
					visible=0
					
				END CREATE
				
				CREATE FBIncPathLbl AS QLABEL
					Caption = "Include":
					Left = 8:Top = FBCompPathLbl.Top+20:
					Transparent = 0
					Width = 50
					visible=0
				END CREATE
				CREATE FBIncPathEdit AS QEDIT         
					Left = 70         
					Top =FBIncPathLbl.Top
					Width = 370
					text=StartPath$+"inc\"
					visible=0
				END CREATE
				CREATE FBIncDirBtn AS QButtonXP
					Caption = "FBIncDirBtn..."
					Left =445: Top = FBIncPathLbl.Top
					'Spacing=2:'bmpHandle = sOpen 
					Width = 20:Height = 20
					tag=400:' !!! FBIncPath
					font.name= "system"
					OnClick = DirTreeOnClick
					visible=0
					'XP=visible
					
				END CREATE
				'------------------------------
				
				CREATE FBLibPathLbl AS QLABEL
					Caption = "Library":
					Left = 8:Top = FBCompPathLbl.Top+40:
					Transparent = 0
					Width = 50
					visible=0
				END CREATE
				
				CREATE FBLibPathEdit AS QEDIT         
					Left = 70         
					Top =FBLibPathLbl.Top
					Width = 380
					text=StartPath$+"lib"
					visible=0
				END CREATE
				CREATE FBLibDirBtn AS QButtonXP
					Caption = "FBLibDirBtn..."
					Left =452: Top = FBLibPathLbl.Top
					'Spacing=2:'bmpHandle = sOpen 
					Width = 20:Height = 20
					tag=308:' !!! FBLibPath
					font.name= "system"
					OnClick = DirTreeOnClick
					visible=0
					'XP=0
					
				END CREATE
				'------------------------------
				
				CREATE FBPrjPathLbl AS QLABEL
					Caption = "Project":
					Left = 8:Top = FBCompPathLbl.Top+60:
					Transparent = 0
					Width = 50
					visible=0
				END CREATE
				CREATE FBPrjPathEdit AS QEDIT         
					Left = 70         
					Top =FBPrjPathLbl.Top
					Width = 380
					text=StartPath$+"projects"
					visible=0
				END CREATE
				CREATE FBprjDirBtn AS QButtonXP
					'XP=0
					Caption = "..."
					Left =452: Top = FBPrjPathLbl.Top
					'Spacing=2:'bmpHandle = sOpen 
					Width = 20:Height = 20
					tag=309: ' !!! FBPrjPath
					font.name= "system"
					OnClick = DirTreeOnClick
					visible=0
				END CREATE
				'------------------------------
				CREATE FBTplPathLbl AS QLABEL
					Caption = "Templates":
					Left = 8:Top = FBCompPathLbl.Top+80:
					Transparent = 0
					Width = 50
					visible=0
				END CREATE
				
				CREATE FBTplPathEdit AS QEDIT         
					Left = 70         
					Top =FBTplPathLbl.Top
					Width = 370
					text=StartPath$+"templates"
					enabled=0
					visible=0
				END CREATE
				
				CREATE FBTplDirBtn AS QButtonXP
					'XP=1
					Caption = "..."
					Left =445: Top = FBTplPathLbl.Top
					'Spacing=2:'bmpHandle = sOpen 
					Width = 20:Height = 20
					tag=310: ' !!! FBTplPath
					font.name= "system"
					OnClick = DirTreeOnClick
					enabled=0
					visible=0
				END CREATE
				'----------------------------
				CREATE FBIcoFileLbl AS QLABEL
					Caption = "Icons":
					Left = 8:Top = FBCompPathLbl.Top+100:
					Transparent = 0
					Width = 50
					enabled=0
					visible=0
				END CREATE
				
				CREATE FBIcoFileEdit AS QEDIT         
					Left = 70         
					Top =FBIcoFileLbl.Top
					Width = 370
					text=StartPath$+"default.ico"
					enabled=0
					visible=0
				END CREATE
				CREATE FBIcoFileBtn AS QButtonXP
					'XP=1
					Caption = "..."
					Left =445: Top = FBIcoFileLbl.Top
					Width = 20:Height = 20
					tag=311:' !!! FBIcoPath
					font.name= "system"
					OnClick =ChooseIcon 
					enabled=0
					visible=0
				END CREATE
				
			END CREATE ' FB Panel
			CREATE ArdPanel AS QPanel '
				Top = PropPanel2.ClientHeight/2 +85
				Left =3
				Width = PropPanel2.ClientWidth - 4
				Height = 180 'PropPanel2.ClientHeight/2 - 4
				color=&H76CBEF 
				CREATE ArdLabel AS QLABEL
					Caption = "Arduino"
					Left = PropPanel2.ClientWidth/2 - 54
					Top = 4
					Transparent = 1
					font.size=16
					font.bold=1
					font.name="Arial"
					'LowerColor=cldr
					UpperColor=&HFFD2AE '&HB6A3FF
					font.color=&H0052FF
					LabelStyle=lsRaised 
					visible=0
				END CREATE
				
				CREATE ArdCompPathLbl AS QLABEL
					Caption = "Arduino":
					Left = 8:Top = 37:
					Transparent = 0
					Width = 60
				END CREATE
				
				CREATE ArdCmpPathEdit AS QEDIT         
					Left = 70         
					Top =ArdCompPathLbl.Top-4
					Width = 370
					text="C:\Program Files\Arduino\" 'curdir$
				END CREATE
				CREATE ArdFileBtn AS QButtonXP
					XP=1
					Caption = "..."
					Left =445: Top = ArdCompPathLbl.Top-3
					Width = 20:Height = 20
					tag=312: ' !!! ArdCmpPathEdit
					font.name= "system"
					'OnClick =ChooseIcon 
					enabled=1
					'visible=0
				END CREATE
				
			END CREATE '-панель 
			
		END CREATE '-- directories 
		
		
		'------------------------------------------------------------------
		
		CREATE PropPanel3 AS QPanel ' Compiler
			Top = 24
			Left = 5
			Width = PropertTab.ClientWidth - 10
			Height = PropertTab.ClientHeight - 30
			'Caption = "Compiler"
			BevelInner = bvLowered
			Visible = False
		END CREATE
		CREATE PropPanel4 AS QPanel 'Debugger
			Top = 24
			Left = 5
			Width = PropertTab.ClientWidth - 10
			Height = PropertTab.ClientHeight - 30
			Caption = "Debugger"
			BevelInner = bvLowered
			Visible = False
		END CREATE
		CREATE PropPanel5 AS QPanel 'VersionInfo
			Top = 24
			Left = 5
			Width = PropertTab.ClientWidth - 10
			Height = PropertTab.ClientHeight - 30
			Caption = "VersionInfo"
			BevelInner = bvLowered
			Visible = False
		END CREATE
		CREATE PropPanel6 AS QPanel 'Localization
			Top = 24
			Left = 5
			Width = PropertTab.ClientWidth - 10
			Height = PropertTab.ClientHeight - 30
			Caption = "Localization"
			BevelInner = bvLowered
			Visible = False
		END CREATE
		CREATE PropPanel7 AS QPanel '!!! Arduino
			Top = 24
			Left = 5
			Width = PropertTab.ClientWidth - 10
			Height = PropertTab.ClientHeight - 30
			Caption = "Arduino"
			BevelInner = bvLowered
			Visible = True
			showhint=1
			
			CREATE ArduinoTabCtrl1 AS QTABCONTROL 
				Align = alClient
				Color = &HF7FFB0
				AddTabs "Настройки","Сеть"
				
				CREATE sketchbookPathEdit AS QEDIT
					Left = 10
					Top = 25
					Width = 323
					Text = "Sketchbook Path"
				END CREATE
				
				CREATE sketchbookPathBtn AS QButtonXP
					XP=1
					Caption = "Sketch path"
					Top = 25
					'align=alleft
					Left = 350
					width=100
					height=20
					Font.color=clr
					tag=320: ' !!! sketchbookPath
					OnClick=DirTreeOnClick 'sketchbookPathBtnonclick
				END CREATE
				
				CREATE CliPathEdit AS QEDIT
					Left = 10
					Top = 50
					Width = 323
					Text = "arduino-cli"
				END CREATE
				
				CREATE CliFileButn AS QButtonXP
					XP=1
					Caption = "Arduino CLI file"
					Top = 50
					'align=alleft
					Left = 350
					width=100
					height=20
					tag=321: ' !!! CliFile
					Font.color=clr
					OnClick=CliFileButnonclick
					
				END CREATE
				
				CREATE BuildPathEdit AS QEDIT
					Left = 10
					Top = 75
					Width = 323
					Text = StartPath$+"BuildPath"
				END CREATE
				
				CREATE BuildPathButn AS QButtonXP
					XP=1
					Caption = "Build Path"
					Top = 75
					'align=alleft
					Left = 350
					width=100
					height=20
					tag=322: ' !!! BuildPath
					Font.color=clr
					OnClick=DirTreeOnClick 'CliFileButnonclick
					hint="Path where to save compiled files. If omitted, a directory will be created in the default temporary path of your OS."
					
				END CREATE
				
				CREATE ArdLibraryPathEdit AS QEDIT
					Left = 10
					Top = 100
					Width = 323
					Text = StartPath$+"Library"
				END CREATE
				
				CREATE ArdLibraryPathButn AS QButtonXP
					XP=1
					Caption = "Library Path"
					Top = 100
					'align=alleft
					Left = 350
					width=100
					height=20
					Font.color=clr
					tag=323: ' !!! ArdLibraryPath
					OnClick=DirTreeOnClick 'CliFileButnonclick
					hint=" List of custom libraries paths separated by commas. Or can be used multiple times for multiple libraries paths"
					
				END CREATE
				
				CREATE ArdLogFileEdit AS QEDIT
					Left = 10
					Top = 125
					Width = 323
					Text = StartPath$+"CompileMsg.log"
				END CREATE
				
				CREATE ArdLogFileButn AS QButtonXP
					XP=1
					Caption = "Log file"
					Top = 125
					'align=alleft
					Left = 350
					width=100
					height=20
					Font.color=clr
					tag=324: ' !!! ArdLogFile
					OnClick=ArdLogFileButnonclick
					visible=0
					
				END CREATE
				
				CREATE ArdWarningLbl AS QLABEL
					Left = 12
					Top = 154
					Width = 137
					Caption = "Warnings"
				END CREATE
				CREATE ArdWarningCmb AS QCOMBOBOX
					Left = 65
					Top = 150
					Height = 21
					width=60
					Text = "none"
					AddItems "none","default","more","all"
				END CREATE
				
				
				
				CREATE ArdLogLevelLbl AS QLABEL
					Left = 180
					Top = 150
					Width = 137
					Caption = "Log level"
				END CREATE
				CREATE ArdLogLevelCmb AS QCOMBOBOX
					Left = 230
					Top = 150
					Height = 21
					width=60
					Text = "info"
					AddItems "trace", "debug", "info", "warn", "error", "fatal", "panic"
				END CREATE
				
				
				
				CREATE Label3 AS QLABEL
					Left = 10
					Top = 250
					Width = 188
					Caption = "Показать подробный вывод"
					visible=0
				END CREATE
				CREATE ChkBox1 AS QCHECKBOX
					Left = 205
					Top = 250
					Caption = "Компиляция"
					visible=0
				END CREATE
				CREATE ChkBox2 AS QCHECKBOX
					Left = 321
					Top = 250
					Caption = "Загрузка"
					visible=0
				END CREATE
				CREATE ChkBox3 AS QCHECKBOX
					Left = 13
					Top = 295
					Width = 131
					Caption = "Номера строк"
					visible=0
				END CREATE
				CREATE ChkBox4 AS QCHECKBOX
					Left = 12
					Top = 326
					Width = 193
					Caption = "Проверять код после загрузки"
				END CREATE
				CREATE ChkBox5 AS QCHECKBOX
					Left = 13
					Top = 351
					Width = 284
					Caption = "Сохранять скетч при проверке или компиляции"
				END CREATE
				CREATE Label5 AS QLABEL
					Left = 16
					Top = 382
					Width = 363
					Caption = "Дополнительные ссылки для менеджера плат"
				END CREATE
				CREATE Edit2 AS QEDIT
					Left = 16
					Top = 413
					Width = 355
				END CREATE
				CREATE Cool1 AS QCOOLBTN
					Left = 392
					Top = 412
					Width = 44
					Caption = "Add new"
				END CREATE
			END CREATE
			
			
		END CREATE
		
	end create
	
	
end create


CREATE WatchList AS QForm '==========================================
	Caption = "Watch List"
	''  Center
	top=115
	left=2
	height=405
	Width = 200   
	FormStyle = fsStayOnTop
	OnClose=FrmMinimized
	OnResize=WatchListFormResize
	AutoScroll=false
	' KeyPreview = 1        'this line does the trick
	''  OnKeyPress = CheckDir 
	''  color=&hdEd8d5
	
	CREATE WatchEdit AS QListbox 'QRICHEDIT
		align=alClient
		'Left = 2
		'Top = 1
		'Height =WatchList.ClientHeight -23
		'Width = WatchList.ClientWidth-5
		Color=&HF7D6EC
		Columns=2
	end create
	
end create

CREATE ObjectInspector AS QForm '========================================
	Caption = "Object Inspector"
	''  Center
	top=115
	left=2
	height=405
	Width = 200   
	'FormStyle = fsStayOnTop
	OnClose=FrmMinimized
	'OnResize=ObjInspectorFormResize
	'OnShow=ObjInspectorFormShow
	AutoScroll=false
	'KeyPreview = 1        'this line does the trick
	''  OnKeyPress = CheckDir 
	''  color=&hdEd8d5
	
	CREATE ObjCBox AS QCOMBOBOX
		Text = ""
		Left = 2
		Top = 2
		Width = ObjectInspector.ClientWidth -5
		Height = 99
		'DropDownCount = 9
		'Sorted = 1
		TabOrder = 10
		''  AddItems "c:\", "c:\temp"
		'' OnChange=GotoHotDir
	END CREATE
	CREATE ObjTab AS QTabControl
		AddTabs "Properties","Events"
		top=25
		Left = 1'WindChoose
		Width = ObjectInspector.ClientWidth 
		Height = ObjectInspector.ClientHeight -25
		OnChange = ObjTabChange
		HotTrack = True
		
		CREATE PropertiesStringGrid AS QSTRINGGRID
			''       color=clSilver'clWhite
			Left = 1
			Top = 30
			Height =ObjectInspector.ClientHeight -23
			Width = ObjectInspector.ClientWidth 
			TabOrder = 2
			ColCount = 2
			RowCount = 18
			DefaultColWidth = 80
			DefaultRowHeight = 20
			enabled=true
			separator=chr$(09)
			cell(0,0) = "Fields":
			'colWidths(5)=80'
			Cell(1,0) = "Value":
			
			''         OnDrawCell = DrawCell
			''         OnSelectCell = SelectCell
			AddOptions=goThumbTracking, goDrawFocusSelected, goColSizing
			
			ColumnStyle(1)=gcsList
			ColumnList(1) = "Replace"+chr$(10)+_
			"Delete"+chr$(10)
			'OnSetEditText=ChangeSel 
		END CREATE
		
		CREATE EventsStringGrid AS QSTRINGGRID
			''       color=clSilver'clWhite
			Left = 1
			Top = 30
			Height =ObjectInspector.ClientHeight -23
			Width = ObjectInspector.ClientWidth
			TabOrder = 2
			ColCount = 2
			RowCount = 18
			DefaultColWidth = 80
			DefaultRowHeight = 20
			enabled=true
			separator=chr$(09)
			cell(0,0) = "Events":
			'colWidths(5)=80'
			
			Cell(1,0) = "Procedure":
			
			''         OnDrawCell = DrawCell
			''         OnSelectCell = SelectCell
			AddOptions=goThumbTracking, goDrawFocusSelected, goColSizing
			
			ColumnStyle(1)=gcsList
			ColumnList(1) = "Replace"+chr$(10)+_
			"Delete"+chr$(10)
			
			'OnSetEditText=ChangeSel
		END CREATE
		
	END CREATE
END CREATE


SrcEdH=90'165

dim ObjTreePopUprefresh as QMenuItem
ObjTreePopUprefresh.caption="Refresh"

dim ObjTreePopUp as QPopUpMenu
with ObjTreePopUp
	.autopopup=1
	.AddItems ObjTreePopUprefresh
end with


create IncTreePopUp as QPopUpMenu
	autopopup=1
end create

dim TabPopUpMenuItemDel as QMenuItem
TabPopUpMenuItemDel.caption="Delete Tab"
TabPopUpMenuItemDel.onclick=CloseTabBnOnClick 'TabPopUpMenuDel

dim TabPopUpMenuItemAdd as QMenuItem
TabPopUpMenuItemAdd.caption="Add Tab"
TabPopUpMenuItemAdd.onclick=TabPopUpMenuAdd

dim TabPopUpMenuItemReload as QMenuItem
TabPopUpMenuItemReload.caption="Reload file"
TabPopUpMenuItemReload.onclick=TabPopUpMenuReloadFile


dim TabPopUp as QPopUpMenu
with TabPopUp
	.autopopup=1
	.AddItems TabPopUpMenuItemAdd
	.AddItems TabPopUpMenuItemDel
	.AddItems TabPopUpMenuItemReload
end with

dim TabInactiveFont as QFont
'TabInactiveFont.size=8
TabInactiveFont.Name="Arial" '"Lucida Console" '"Courier New"
TabInactiveFont.color=&&H4D4A4C

CREATE DelWinForm AS QForm ' панель табов открытых окон
	
	top=50
	Width =400
	Height =Screen.Height*0.7
	left=500'Screen.Height*0.5
	color=clo
	'color=clg 'clsilver'darkcolor
	'visible=1
	BorderStyle=bsNone 
	CREATE DelWinListBox AS QLISTBOX
		Left = 1
		Top = 1
		TabOrder = 8
		align=alclient
		ExtendedSelect=1
		MultiSelect=1
		Style=lbOwnerDrawFixed 
		color=clMenu
		'Style = lbOwnerDrawVariable               '-- Variable height
		
		'OnDrawItem = ListBoxDrawItem
		'OnMeasureItem = ListBoxMeasureItem        '-- Note, order IS important!
		
		ItemHeight=20
		
	END CREATE
	
	CREATE DelWinBtn AS Qbutton 'QPANEL
		Left = 1
		Top = 1
		align=albottom
		Caption = "Close selected windows"
		TabOrder = 8
	font.bold=1
	font.size=16
	font.name="Arial"
	onclick=DelWinBtnOnClick
	END CREATE
	
	CREATE CancelDelWinBtn AS Qbutton 'QPANEL
		Left = 1
		Top = 1
		align=albottom
		Caption = "Cancel"
		TabOrder = 8
	font.bold=1
	font.size=16
	font.name="Arial"
	onclick=CancelDelWinBtnOnClick
	END CREATE
	
end create



CREATE RQForm AS QFORM '----------- главная форма ----------------  '
	Caption = "RQ Debugger "+ver$
	Width =Screen.Width*0.6'-700
	Height =Screen.Height*0.9
	
	KeyPreview = 1    
	Font.size=9
	OnClose=FrmClose
	onresize=RQFormonresize
	onshow=RQFormonshow
	OnMouseMove=RQFormOnMouseMove
	AutoScroll=0
	'WindowState=2
	'color=darkcolor
	'!*********************************************************
	CREATE TabPanel AS QPANEL ' панель табов открытых окон
		align=alTop
		top=20
		height=24
		'color=clg 'clsilver'darkcolor
		
		'color=clb
		CREATE CloseTabBn11 AS QButtonXP
			XP=1
			Caption = "X"
			font.bold=1
			font.name="Tahoma"
			Align=alleft 'alRight alRight '
			left=1
			height=21
			Width=22
			hint="Delete current tab"
			OnClick=CloseTabBnOnClick
		END CREATE
		CREATE TabListCmbox AS QCOMBOBOX
			Text = "Select file"
			Left = 1
			Top = 1
			TabOrder = 8
			align=alRight
			width=400
			onchange=TabListCmboxChange
		END CREATE
		
		
		
		'CREATE Tab25 AS QTabControl
		CREATE Tab25 AS  QtabControlEx
			'AddTabs "Sub list","ObjTree","IncTree","ArduinoCli Help"
			'AddTabs "0000000000000000000000","11111111111111","1222222222222222"
			'AddTabs "","",""
			'align=alclient
			Align=alleft 'alRight alRight '
			
			OnChange = Tab25Change
			showhint=1
			hint="Hint!"
			'HotTrack = 1 'True   
			POPUPMENU=TabPopUp
			TabInactiveColor=CLsILVER
			color=clsilver
			'color=&HFFFFC7
			'font.size=9
			font.Name="Arial" 'Source Code Pro" '"Lucida Console" 'Courier New" '"Lucida Console" '
			'font.bold=1
			font.color=clb
			TabInactiveFont=TabInactiveFont
			width=60
			'MultiLine=1
			'TabInactiveFont.size=8
			'TabHeight=15
		END CREATE
		
		
		
	END CREATE '--- ' панель табов открытых окон
	
	Create ToolPanel AS QPANEL
		align=altop
		top=1
		height=100
		'color=cly 'clsilver'darkcolor
		
		Create ToolbarPanel AS QPANEL
			align=alleft
			width=650
			color=clg 'clsilver'darkcolor
			
			Create Toolbar AS QPANEL 'Top panel for edit
				width=650
				BevelInner = 2
				BevelOuter = 1
				Height = 40
				ShowHint=1
				left=1
				'color=&H67E8E2'&H9EA681'clr 'clsilver'grey'darkcolor &H8B8B8B '&H646464'
				align=altop
				
				Create cmdNew AS QCOOLBTN
					Align = 3
					BMPHandle = New_BMP
					Flat = 1
					Hint = "New file"
					OnClick = CreateFileOnClick
					Width = 22
				End Create
				
				Create cmdOpen AS QCOOLBTN
					Align = 3
					BMPHandle = Open_BMP
					Flat = 1
					Hint = "Open file"
					OnClick =FileLoad ' Open_Click
					Width = 22
				End Create
				
				Create cmdOpenHEX AS QCOOLBTN
					Align = 3
					BMPHandle = OpenHEX_BMP
					Flat = 1
					Hint = "Open HEX"
					OnClick =FileHEXLoad ' Open_Click
					Width = 22
					visible=0
				End Create
				
				Create cmdSave AS QCOOLBTN
					Align = 3
					BMPHandle = Save_BMP
					Flat = 1
					Hint = "Save"
					OnClick =SaveFileOnClick' Save_Click
					Width = 22
				End Create
				
				
				
				Create cmdSavePrj AS QCOOLBTN
					Align = 3
					BMPHandle = SavePrj_BMP
					Flat = 1
					Hint = "Save Project"
					OnClick =SavePrjFileOnClick '  Save_Click
					Width = 24
				End Create
				
				Create Bar0 AS QCOOLBTN
					Align = 3
					Enabled = 0
					Flat = 0
					Width = 3
				End Create
				
				Create cmdUndo AS QCOOLBTN
					Align = 3
					BMPHandle = Undo_BMP
					Flat = 1
					Hint = "Undo"
					OnClick = RichEditUndo
					Width = 22
				End Create
				
				
				Create cmdCut AS QCOOLBTN
					Align = 3
					BMPHandle = Cut_BMP
					Flat = 1
					Hint = "Cut"
					OnClick = RichEditCut
					Width = 21
				End Create
				
				Create cmdCopy AS QCOOLBTN
					Align = 3
					BMPHandle = Copy_BMP
					Flat = 1
					Hint = "Copy"
					OnClick = RichEditCopy
					Width = 22
				End Create
				
				Create cmdPaste AS QCOOLBTN
					Align = 3
					BMPHandle = Paste_BMP
					Flat = 1
					Hint = "Paste"
					OnClick = RichEditPaste
					Width = 22
				End Create
				
				'Create cmdSelectAll AS QCOOLBTN
				'Align = 3
				'BMPHandle = SelectAll_BMP
				'Flat = 1
				'Hint = "Select All"
				'OnClick = RichEditSelectAll
				'Width = 22
				'End Create
				
				'Create cmdDelete AS QCOOLBTN
				'Align = 3
				'BMPHandle = Delete_BMP
				'Flat = 1
				'Hint = "Delete"
				'OnClick = RichEditDelete
				'Width = 22
				'End Create
				
				'Create Bar1 AS QCOOLBTN
				'  Align = 3
				'  Enabled = 0
				'  Flat = 0
				'  Width = 3
				'End Create
				
				
				Create Bar2 AS QCOOLBTN
					Align = 3
					Enabled = 0
					Flat = 0
					Width = 3
				End Create
				
				Create cmdSubNext AS QCOOLBTN
					Align = 3
					BMPHandle = SubNext_BMP
					Flat = 1
					Hint = "Next Sub"
					OnClick = NextSub
					Width = 24
				End Create
				
				Create cmdSubPrev AS QCOOLBTN
					Align = 3
					BMPHandle = SubPrev_BMP
					Flat = 1
					Hint = "Previous Sub"
					OnClick = PrevSub
					Width = 24
				End Create
				
				
				Create Bar4 AS QCOOLBTN
					Align = 3
					Enabled = 0
					Flat = 0
					Width = 3
				End Create
				
				Create FileMngBtn AS QCOOLBTN
					Align = 3
					BMPHandle = FileMng_BMP
					Flat = 1
					Hint = "File manager"
					OnClick = ViewFileMng
					Width = 24
					down=true
					'GroupIndex = 1  
					
				End Create
				
				Create SubListBtn AS QCOOLBTN
					Align = 3
					BMPHandle = SubList_bmp
					Flat = 1
					Hint = "Sub List"
					OnClick = SubsListOnClick
					Width = 24
					visible=0
					'down=true
					'GroupIndex = 1  
					
				End Create
				Create ObjTreeBtn AS QCOOLBTN
					Align = 3
					BMPHandle = DirTree_bmp
					
					Flat = 1
					Hint = "Object Tree"
					OnClick = ObjTreeOnClick
					Width = 25
					visible=0
					'down=true
					'GroupIndex = 1  
					
				End Create
				
				Create IncTreeBtn AS QCOOLBTN
					Align = 3
					BMPHandle = IncTree_bmp
					
					Flat = 1
					Hint = "Includes Tree"
					OnClick =IncFilesMnuOnClick ' 
					Width = 24
					visible=0
					'down=true
					'GroupIndex = 1  
					
				End Create
				
				Create TSpPanel1 AS QPANEL
					align=alclient
					left=500
					top=1
					height=5
					'color=cla '&H67E8E2 'clsilver'darkcolor
					BevelWidth=0
					
					Create TSpPanel AS QPANEL
						align=altop
						top=1
						height=15
						'color=clo '&H67E8E2 'clsilver'darkcolor
						BevelWidth=0
						
						CREATE CodeSectLabel AS QLABEL
							Caption = "Code Sections"
							Left = 30
							Top = 1
							'Transparent = 1
							'color=clr
						END CREATE
						
						CREATE TPLLabel AS QLABEL
							Caption = "Templates"
							Left = 200
							Top = 1
							Transparent = 1
						END CREATE
						
					End Create
					CREATE SectionListBox AS QComboBOX 'QLISTBOX
						Align = alleft
						'Left = 12
						'Top = 5   
						text="1"+chr$(160)+" Choose section"
						style=csDropDownList 
						width=150'TabBAr.width-4
						OnChange=SectionOnClick
						color=&H00E8FA 'clsilver 'clgrey'darkcolor
						'font.color=clsilver
						Align = alleft
						visible=1
						
					END CREATE
					
					
					
					Create Bar5a AS QPanel 'COOLBTN
						Align = 3
						Width = 13
						'color=clr
						Left = 15
						BevelWidth=0
						
					End Create
					
					
					CREATE TplListBox AS QComboBOX
						Left = 22
						Top = 2
						TabOrder = 8
						text="Code templates"
						style=csDropDownList 
						'color=clsilver'clgrey '&H8B8B8B '&H646464' clsilver' clgrey'
						'font.color=clsilver
						Align = alleft
						visible=1
						
						
					END CREATE
					Create Bar6a AS QPanel 'COOLBTN
						Align = 3
						Width = 5
						'color=clr
						Left = 25
						BevelWidth=0
						
					End Create
					
					CREATE OKTplButton AS QButtonXP
						Left = 34
						XP=1
						Caption = "OK"
						Left = TplListBox.left+TplListBox.width+2
						width =25
						height=25
						Top = 2
						OnClick=RqTplChange
						Align = alleft
					END CREATE
					
					
					
				End Create
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
				
			End Create '!!! ----- end toolbar -------
			
			CREATE Arduino_BasicTab AS Qpanel 'QTabControl
				align=alclient
				Top = 34
				'OnChange = ArdBasTabChange
				showhint=1
				hint="Hint!"
				POPUPMENU=TabPopUp
				'color=&&H00FFC7
				create SelectARDBas AS QPanel 'QGroupBox '
					align=alleft
					width=60
					
					CREATE BasicRBtn AS QRADIOBUTTON
						Caption = "Basic"
						Left = 1
						Top = 1
						TabOrder = 8
						align=altop
						checked=1
						OnClick=ArdBasTabChange
					END CREATE
					CREATE ArduinoRBtn AS QRADIOBUTTON
						Caption = "Arduino"
						Left = 1
						Top = 1
						TabOrder = 8
						align=alclient
						checked=0
						OnClick=ArdBasTabChange
					END CREATE
					
				END CREATE
				
				'-- --------------------------------------------------'
				create RunDebugBar AS QPanel 'QGroupBox '
					Left = 75
					Top = 1
					Width =200 '
					Height =42
					BevelWidth=0
					'align=alclient
					'align=alleft
					'BevelInner = 2
					'BevelOuter = 1
					ShowHint=true
					'color=clo'darkcolor &H8B8B8B '&H646464' silver'
					'caption="Basic"
					visible=1
					
					CREATE Runbtn AS QCoolBtn'BUTTON
						WIDTH=ButtonSize
						ONCLICK=RunItOnClick 
						ShowHint=true
						Hint = "Compile and Run"
						BMPHandle=Run_rsc
						Flat = 1
						Align = alleft
						
					END CREATE
					
					CREATE DebugBtn AS QCoolBtn'BUTTON
						WIDTH=ButtonSize
						ONCLICK=CompDebugOnClick1 
						'ShowHint=true
						Hint = "Compile and Run with debug"
						BMPHandle=Bug2s
						Flat = 1
						Align = 3
					END CREATE
					
					Create Bar5 AS QCOOLBTN
						Align = 3
						Enabled = 0
						Flat = 0
						Width = 3
					End Create
					
					CREATE AddWatchbtn AS QCoolBtn
						WIDTH=ButtonSize
						ONCLICK=AddWatchOnClick
						'ShowHint=true
						Hint = "AddWatch"
						BMPHandle=AddWatch_rsc
						Flat = 1
						Align = 3
					END CREATE
					
					CREATE DelWatchBtn AS QCoolBtn
						WIDTH=ButtonSize
						ONCLICK=DelWatchOnClick 
						ShowHint=true
						Hint = "DelWatch"
						BMPHandle=DelWatch_rsc
						Flat = 1
						Align = 3
					END CREATE
					
					CREATE DelAllWatchbtn AS QCoolBtn
						WIDTH=ButtonSize
						ShowHint=true
						ONCLICK=DelAllWatchOnClick
						Hint = "DelAllWatch"
						BMPHandle=DelAllWatch_rsc
						Flat = 1
						Align = 3
					END CREATE
					
					
					Create Bar6 AS QCOOLBTN
						Align = 3
						Enabled = 0
						Flat = 0
						Width = 3
					End Create
					
					CREATE FBCompile AS QCoolBtn
						WIDTH=ButtonSize
						'CAPTION="FB"
						ShowHint=true
						BMPHandle=FBLogo_bmp
						ONCLICK=FBExeOnClick
						Hint = "FreeBasic compile and run"
						Flat = 1
						Align = 3
					END CREATE
					
					CREATE FBDll AS QCoolBtn
						WIDTH=ButtonSize+3
						CAPTION="DLL"
						ShowHint=true
						ONCLICK=FBDllOnClick
						Hint = "FreeBasic compile DLL"
						Flat = 1
						Align = 3
					END CREATE
					'CREATE SubLabel AS QLABEL
					'Caption = "Count"
					'Left = 86
					'Top = 58
					'Transparent = 1
					'align=alright
					'Hint = "Sub calling count"
					'END CREATE
					
					'CREATE MemStatusLabel AS QLABEL
					'Caption = "Mem. avail."
					'Left =RunDebugBar.left-MemStatusLabel.width   ' SubLabel.left
					'Transparent = 1
					'align=alright
					'Top = 20
					'END CREATE
					CREATE MemStatusLabel1 AS QLABEL
						Caption = "000"
						Left =RunDebugBar.width-150
						Transparent = 1
						'align=alright
						Top = 20
						Hint = "Memory available"
						visible=0
						
					END CREATE
					CREATE MemStatusLabel2 AS QLABEL
						Caption = "000"
						Left =RunDebugBar.width-150
						Transparent = 1
						'align=alright
						Top = 3
						Hint = "Memory available"
						visible=0
					END CREATE
					
				end create  '-- --------------------------------------------------'
				'End Create ' toolpanel
				
				create ArduinoBar AS QPanel 'QGroupBox '
					Left = 1
					Top = 20
					Width =178 '
					Height =66
					align=alclient
					'BevelInner = 2
					'BevelOuter = 1
					ShowHint=true
					'color=&&HB6FFC7 'clsilver'grey'darkcolor &H8B8B8B '&H646464'
					'caption="Arduino"
					visible=0
					CREATE ArdVerifyButton AS QButtonXP
						XP=1
						Caption = "Verify"
						Left = 1
						width =45
						height=25
						Top = 1
						OnClick=VerifyArduinoSketch
						'align=alleft
						tag=10
					END CREATE
					CREATE UploadButton AS QButtonXP
						XP=1
						Caption = "Upload"
						Left = 50
						width =45
						height=25
						Top = 1'27
						'align=alleft
						OnClick=UploadArduinoSketch
						tag=10
					END CREATE
					
					CREATE BoardComboBox AS QCOMBOBOX
						Text = "Select board"
						Left = 1' 50
						Top = 27'4
						TabOrder = 8
						width=220
						OnChange=BoardCmbBoxOnChange1
					END CREATE
					
					create ArduinoToolsCmbox as QComboBox
						Text = "Tools"
						Left =490
						Top = 1
						Width = 90
						TabOrder = 10
						AddItems "Board manager","Library manager","Serial monitor"
						OnChange=ArduinoToolsCmboxOnChange
						'color=clsilver'darkcolor &H8B8B8B '&H646464' clsilver' clgrey'
						'font.color=clsilver
						
					end create
					
					
					
					create ArduinoCmdCmbox as QComboBox
						Text = "Cli commands"
						Left =190
						Top = 1
						Width = 190
						TabOrder = 10
						AddItems _ 
						"board           Arduino board commands", _
						"burn-bootloader Upload the bootloader", _
						"cache           Arduino cache commands", _
						"compile         Compiles Arduino sketches", _
						"completion      Generates completion scripts", _
						"config          Arduino configuration commands", _
						"core            Arduino core operations", _
						"daemon          Run as a daemon on port 50051", _
						"debug           Debug Arduino sketches", _
						"help            Help about any command", _
						"lib             Arduino commands about libraries", _
						"outdated        Lists cores and libraries that can be upgraded", _
						"sketch          Arduino CLI sketch commands", _
						"update          Updates the index of cores and libraries", _
						"upgrade         Upgrades installed cores and libraries", _
						"upload          Upload Arduino sketches", _
						"version         Shows version number of Arduino CLI" 
						
						
						
						OnChange=ArduinoCmdCmboxOnChange
						'color=clsilver'darkcolor &H8B8B8B '&H646464' clsilver' clgrey'
						'font.color=clsilver
						
					end create
					
					create ArduinoFlgCmbox as QComboBox
						Text = "Cli flags"
						Left =400
						Top = 1
						Width = 90
						TabOrder = 10
						AddItems _
						"--additional-urls strings   Comma-separated list of additional URLs for the Boards Manager", _
						"--config-file string        The custom config file (if not specified the default will be used)", _
						"--format string             The output format, can be {text|json}. (default "text")", _
						"-h, --help                      help for arduino-cli", _
						"--log-file string           Path to the file where logs will be written", _
						"--log-format string         The output format for the logs, can be {text|json}", _
						"--log-level string          Messages with this level and above will be logged. Valid levels are: trace, debug, info, warn, error, fatal, panic", _
						"-v, --verbose                   Print the logs on the standard output"
						
						OnChange=ArduinoflgCmboxOnChange
						'color=clsilver'darkcolor &H8B8B8B '&H646464' clsilver' clgrey'
						'font.color=clsilver
						
					end create
					
					
					
					
					
					
				End Create ' toolpanel
				
			END CREATE
		END CREATE   
		CREATE LabelPrj AS QLABEL
			Caption = "  Project "
			Left = 700
			Top = 1
			align=alleft
			
			Transparent = 0
		END CREATE
		
		
		CREATE FindGroupBox AS QPanel '!!! ----------------
			'Top = 1
			Height = 72
			Width=231
			Left =400 'TabBAr.left+TabBAr.Width+3 ' RQForm.ClientWidth-FindGroupBox.Width'-50'380+255
			visible=1
			align=alleft
			color=&HC4AD98'&HD7DFE6
			'color=&HB0C3BB
			'color=darkcolor &H8B8B8B '&H646464' clsilver' clgrey'
			'color=clgrey'clsilver 'clgrey'darkcolor
			'font.color=clsilver
			
			BevelInner = 0
			BevelOuter = 0
			create SearchCBox as QComboBox
				Text = ""
				Left =2
				Top = 3
				Width = 120
				'DropDownCount = 9
				TabOrder = 10
				'color=clsilver'darkcolor &H8B8B8B '&H646464' clsilver' clgrey'
				'font.color=clsilver
				
			end create
			
			create ReplaceComBox as QComboBox
				Text = ""
				Left =2
				Top = 26
				Width = 120
				TabOrder = 10
				'color=clsilver'darkcolor &H8B8B8B '&H646464' clsilver' clgrey'
				'font.color=clsilver
				
			end create
			
			create FindBtnUp as QButtonXP
				XP=1
				
				Left =SearchCBox.Left+SearchCBox.Width+2'5' 305
				Top = SearchCBox.top'-1
				Width = 37'24
				Height = 22
				Caption="<-Find"
				'font.bold=1
				OnClick=FindText '
				tag=22 'find button
				enabled=0
			end create
			
			create FindBtnDn as QButtonXP
				XP=1
				Left = FindBtnUp.Left+FindBtnUp.Width+3
				Top = SearchCBox.top'-1
				Width = 37'24
				Height = 22
				Caption="Find->"
				OnClick=FindText '
				tag=21
				Hint="Find forward"
				ShowHint=1
				
				
			end create
			
			create FindBtnAll as QButtonXP
				XP=1
				Left = FindBtnDn.Left+FindBtnDn.Width+3
				Top = SearchCBox.top'-1
				Width = 22
				Height = 22
				Caption="All"
				OnClick=FindAll 'FindText '
				tag=23
				enabled=1
			end create
			
			create ReplaceBtn as QButtonXP
				XP=1
				Left =SearchCBox.Left+SearchCBox.Width+2'5' 315
				Top = ReplaceComBox.top'-1
				Width = 46
				Height = 22
				tag=24
				Caption="Replace" '"OK"
				OnClick=FindText '
			end create
			
			create ReplaceSkipBtn as QButtonXP
				XP=1
				Left = ReplaceBtn.Left+ReplaceBtn.Width+3'355
				Top = ReplaceComBox.top'-1
				Width = 28
				Height = 22
				tag=21
				Caption="Skip"'"No" '"Skip"
				OnClick=FindText '
			end create
			
			create ReplaceAllBtn as QButtonXP
				XP=1
				Left = ReplaceSkipBtn.Left+ReplaceSkipBtn.Width+3'355
				Top = ReplaceComBox.top'-1
				Width = 22
				Height = 22
				tag=25
				Caption="All"
				OnClick=FindText '
			end create
			CREATE WhooleWordCheckBox AS  QComboBox 'QCHECKBOX
				text = "Normal"
				Left =2
				Top = ReplaceComBox.top+22
				Width = 120
				Height = 22
				AddItems "Normal","WholeWord", "Sub/Functions", "Object","Go to line"
				'color=clsilver'darkcolor &H8B8B8B '&H646464' clsilver' clgrey'
				'font.color=clsilver
				
			END CREATE
			
			CREATE CAseSensCheckBox AS QCHECKBOX
				Caption = "Case"
				Left =ReplaceBtn.left+2
				Top = ReplaceComBox.top+25
			END CREATE
			
			CREATE ListOfSearchChBox AS QCHECKBOX
				Caption = "List"
				Left =ReplaceBtn.left+50
				Top = ReplaceComBox.top+25
				width=45
				OnClick=ListChBoxClick
			END CREATE
			
			
			
			CREATE WinButton1 AS QButtonXP
				XP=1
				Caption = "Win"
				Left = 1
				width =28
				height=20
				Top = 71
				OnClick=CodPChange
				tag=10
			END CREATE
			CREATE DosButton1 AS QButtonXP
				XP=1
				Caption = "Dos"
				Left = WinButton1.left+WinButton1.width+1
				width =28
				height=20
				Top = 71
				OnClick=CodPChange
				tag=11
			END CREATE
			CREATE KoiButton1 AS QButtonXP
				XP=1
				Caption = "Koi"
				Left = DosButton1.left+DosButton1.width+1
				width =30
				height=20
				Top = 71
				OnClick=CodPChange
				tag=12
			END CREATE
			CREATE QPButton1 AS QButtonXP
				'XP=1
				Caption = "=QP"
				Left = KoiButton1.left+KoiButton1.width+1
				width =30
				height=20
				Top = 71
				'OnClick=CodPChange
				tag=13
				visible=0
			END CREATE
			
			CREATE UTF8Button1 AS QButtonXP
				XP=1
				Caption = "UTF8"
				Left = QPButton1.left
				width =36
				height=20
				Top = 71
				OnClick=CodPChange
				tag=14
			END CREATE
			
			
			
			
		END CREATE '!!! ------  searchgroup -------
	End Create ' toolpanel
	
	
	CREATE MainMenu AS QMainMenu
		CREATE FileMenu AS QMenuItem '===============
			Caption = "&File"
			CREATE CreateItem  AS QMenuItem
				Caption = "&New"
				Hint = "New"
				CREATE EmptyNewWin AS QMenuItem
					Caption = "Empty"
					OnClick=CreateFileOnClick
					
				END CREATE
				CREATE CopyNewWin AS QMenuItem
					Caption = "Copy current window"
				END CREATE
				CREATE ClipBoardNewWin AS QMenuItem
					Caption = "From ClipBoard"
				END CREATE
				
			END CREATE
			CREATE OpenItem AS QMenuItem
				Caption = "&Open"
				Hint = "Open"
				OnClick=FileLoad 'OpenStFiles '
				ShortCut="Ctrl+O"
				tag=104
			END CREATE
			
			CREATE OpenStnd AS QMenuItem
				Caption = "&Open Standart files"
				Hint = "Open Standart files"
				
				CREATE OpenInc AS QMenuItem
					Caption = "&Open Includes"
					Hint = "Open Includes"
					OnClick=OpenStFiles
					tag=105
				END CREATE
				
				CREATE OpenTpl AS QMenuItem
					Caption = "&Open Templates"
					Hint = "Open Includes"
					OnClick=OpenStFiles
					tag=106
				END CREATE
				
				CREATE OpenPrj AS QMenuItem
					Caption = "&Open Projects"
					Hint = "Open Projects"
					OnClick=OpenStFiles
					tag=107
				END CREATE
				CREATE OpenCurrPrj AS QMenuItem
					Caption = "&Open Current Projects"
					Hint = "Open Current  Projects"
					OnClick=OpenCurrPrjClick
					tag=107
				END CREATE
				
			END CREATE
			
			CREATE SaveItem AS QMenuItem
				Caption = "&SaveFile"
				Hint = "Save"
				OnClick=SaveFileOnClick
				ShortCut="Ctrl+S"
			END CREATE
			CREATE SaveAsItem AS QMenuItem
				Caption = "&SaveFileAs.."
				Hint = "SaveAs"
				OnClick=SaveAsOnClick
				ShortCut="Ctrl+A"
			END CREATE
			CREATE DeleteItem AS QMenuItem
				Caption = "&DeleteFile"
				Hint = "DeleteFile"
				'OnClick=DeleteFileOnClick
			END CREATE
			CREATE BreakItem1 AS QMenuItem
				Caption = "-"
			END CREATE
			
			CREATE CreatePrj  AS QMenuItem
				Caption = "&New project"
				Hint = "New"
				OnClick=CreatePrjOnClick
			END CREATE
			
			CREATE SavePrj AS QMenuItem
				Caption = "&Save Project"
				Hint = "Save"
				OnClick=SavePrjFileOnClick
				'            ShortCut="Ctrl+S"
			END CREATE
			CREATE SavePrjAs AS QMenuItem
				Caption = "Save Project As.."
				Hint = "Save Project As"
				OnClick=SavePrjAsOnClick
				'ShortCut="Ctrl+A"
			END CREATE
			CREATE BreakItem3 AS QMenuItem
				Caption = "-"
			END CREATE
			
			CREATE ArduinoExamplesMnu AS QMenuItem
				Caption = "Arduino Examples"
				Hint = "Examples"
				
				CREATE BasicsMnu AS QMenuItem
					Caption = "01.Basics"
				END CREATE
				CREATE DigitalMnu AS QMenuItem
					Caption = "02.Digital"
				END CREATE
				CREATE AnalogMnu AS QMenuItem
					Caption = "03.Analog"
				END CREATE
				CREATE CommunicationMnu AS QMenuItem
					Caption = "04.Communication"
				END CREATE
				CREATE ControlMnu AS QMenuItem
					Caption = "05.Control"
				END CREATE
				CREATE SensorsMnu AS QMenuItem
					Caption = "06.Sensors"
				END CREATE
				CREATE DisplayMnu AS QMenuItem
					Caption = "07.Display"
				END CREATE
				CREATE StringsMnu AS QMenuItem
					Caption = "08.Strings"
				END CREATE
				CREATE USBMnu AS QMenuItem
					Caption = "09.USB"
				END CREATE
				CREATE StarterMnu AS QMenuItem
					Caption = "10.StarterKit_BasicKit"
				END CREATE
				
				CREATE ISPMnu AS QMenuItem
					Caption = "11.Arduino ISP"
				END CREATE
			END CREATE
			
			CREATE ArduinoPref AS QMenuItem
				Caption = "Preferences..."
				onclick=ArduinoPrefOnClick
			END CREATE
			
			
			CREATE BreakItem2 AS QMenuItem
				Caption = "-"
			END CREATE
			
			CREATE ExitItem AS QMenuItem
				Caption = "Exit"
				Hint = "Exit"
				ShortCut="Alt+X"
				OnClick=  exitProg'OnClick
			END CREATE
		END CREATE
		
		CREATE EditMenu AS QMenuItem '================
			Caption = "&Edit"
			visible=1
			' disable=1
			CREATE Undo As QMenuItem
				Caption = "Undo"
				ShortCut = "Ctrl+Z"
				onClick=RichEditUndo
			END CREATE
			CREATE BreakItem2e AS QMenuItem
				Caption = "-"
			END CREATE
			CREATE CutItem As QMenuItem
				Caption = "Cu&t"
				ShortCut = "Ctrl+X"
				OnClick=RichEditCut
			END CREATE
			CREATE CopyItem As QMenuItem
				Caption = "&Copy"
				OnClick=RichEditCopy
			END CREATE
			CREATE PasteItem As QMenuItem
				Caption = "&Paste"
				OnClick=RichEditPaste
			END CREATE
			CREATE DeletItem As QMenuItem
				Caption = "&Delete"
				OnClick=RichEditDelete
			END CREATE
			CREATE SelectAllIt As QMenuItem
				Caption = "&SelectAll"
				OnClick=RichEditSelectAll
			END CREATE
			CREATE BreakItem3e AS QMenuItem
				Caption = "-"
			END CREATE
			'CREATE PasteSpecial AS QMenuItem
			'Caption = "Paste Special"
			CREATE PasteCurFileName AS QMenuItem
				Caption = "Paste Current File Name"
				OnClick=PasteCurFileNameOnClick
			END CREATE
			CREATE PasteDate AS QMenuItem
				Caption = "Paste Date"
				OnClick=PasteDateOnClick
			END CREATE
			CREATE PasteDelim AS QMenuItem
				Caption = "Paste Sub Delimiter"
				OnClick=PasteSubDelimiter
			END CREATE
			
			'END CREATE
			
			CREATE BreakItem3f AS QMenuItem
				Caption = "-"
			END CREATE
			CREATE AlignToGrid AS QMenuItem
				Caption = "AlignToGrid"
				enabled=0
			END CREATE
			
		END CREATE '-EditMenu'
		
		CREATE ViewMenu AS QMenuItem  '===========
			Caption = "&View"
			visible=1
			' disable=1
			
			CREATE FileMng AS QMenuItem
				Caption = "File List"
				'Hint = "Project Source"yh
				OnClick=ViewFileMng
			END CREATE
			CREATE ObjTreeMnu AS QMenuItem
				Caption = "Objects tree"
				'Hint = "Project Source"
				OnClick=ObjTreeOnClick
			END CREATE
			
			CREATE ProjectSrc AS QMenuItem
				Caption = "Source"
				Hint = "Project Source"
				'OnClick=OpenPrjSrcOnClick
			END CREATE
			CREATE DebugSrc AS QMenuItem
				Caption = "Source with Debug"
				Hint = "Исходник с отладочной информацией"
				OnClick=OpenDebugSrcOnClick
			END CREATE
			CREATE ProjectFrm AS QMenuItem
				Caption = "Form"
				Hint = "Project Form"
				OnClick=OpenPrjFrmOnClick
			END CREATE
			CREATE ObjInspector AS QMenuItem
				Caption = "Object Inspector"
				Hint = "ObjectInspector"
				OnClick=ObjInspectorOnClick
			END CREATE
			CREATE SubsList AS QMenuItem
				Caption = "Subroutine list"
				Hint = "Subroutine list"
				OnClick=SubsListOnClick
			END CREATE
			CREATE WatchWnd AS QMenuItem
				Caption = "Watch List "
				Hint = "Watch List "
				OnClick=WatchListOnClick
			END CREATE
			CREATE DirTreeMnu AS QMenuItem
				Caption = "Directory Tree"
				Hint = "Directory Tree"
				'OnClick=DirTreeOnClick
				visible=0
			END CREATE
			CREATE LangManager AS QMenuItem
				Caption = "LangManager"
				Hint = "LangManager"
				OnClick=LangManagerOnClick
			END CREATE
		END CREATE '-- view'
		
		
		CREATE ComponentMnu AS QMenuItem '!!! ---- Components
			Caption = "Components"
			Hint = "Components"
		END CREATE '-Project'
		
		CREATE ProjectsMnu AS QMenuItem '!!!===
			Caption = "Projects"
			Hint = "Projects"
			OnClick=OpenProject
			CREATE DeletePrj AS QMenuItem
				Caption = "Delete Project"
				Hint = "Delete Project"
				OnClick=DeletePrjOnClick
			END CREATE
			CREATE CheckPrj AS QMenuItem
				Caption = "Delete wrong projects names"
				Hint = "Delete wrong projects names from this list"
				OnClick=CheckPrjOnClick
			END CREATE
			
		END CREATE '-Project'
		
		CREATE Verify AS QMenuItem '!!!=
			Caption = "Verify"
			Hint = "Verify source code"
			enabled=1
			CREATE VerifyEvent AS QMenuItem '==========
				Caption = "Verify Events handlers"
				OnClick=VerifyEventOnClick
			END CREATE '
			
			CREATE VerifyGoTo AS QMenuItem '==========
				Caption = "Verify GoTo"
				Hint = "Verify GoTo"
				enabled=0
			END CREATE '
			
			CREATE VerifyFileClose  AS QMenuItem '==========
				Caption = "Verify File Close"
				enabled=0
			END CREATE '
			
			CREATE VerifyWith  AS QMenuItem '==========
				enabled=0
				Caption = "Verify With..End With"
			END CREATE '
			
			CREATE V1  AS QMenuItem '==========
				Caption = "-"
			END CREATE '
			
			CREATE VerifyAll  AS QMenuItem '==========
				Caption = "Verify All"
				enabled=0
			END CREATE '
			
		END CREATE '
		
		CREATE FormatMnu  AS QMenuItem '==========
			Caption = "Format text"
			CREATE DelSpcMnu  AS QMenuItem '==========
				Caption = "Delete led spaces"
				tag=50
				OnClick=DeleteLedSpaces
			END CREATE '
			CREATE DelTabMnu  AS QMenuItem '==========
				Caption = "Delete HotTabs"
				tag=51
				OnClick=DelHotTabs
			END CREATE '
			CREATE DelCommentsMnu  AS QMenuItem '==========
				tag=56
				Caption = "Delete Comments"
				enabled=1
				OnClick=DelCommentsOnClick
				
			END CREATE '
			CREATE DelEmptyLines  AS QMenuItem '==========
				tag=57
				Caption = "Delete Empty Lines"
				enabled=1
				OnClick=DelEmptyLinesOnClick
				
			END CREATE '
			CREATE FrmD  AS QMenuItem '==========
				Caption = "-"
			END CREATE '
			
			CREATE RepTab2SpcMnu  AS QMenuItem '==========
				tag=52
				Caption = "Replace HotTabs to spaces"
				enabled=0
			END CREATE '
			
			CREATE ReFormatTabMnu  AS QMenuItem '==========
				tag=54
				Caption = "ReFormat with HotTabs"
				OnClick=ReFormatHotTabs
			END CREATE '
			CREATE ReFormatRTFMnu  AS QMenuItem '==========
				tag=54
				Caption = "ReFormat RTF"
				OnClick=ReFormatRTF
			END CREATE '
			CREATE ReFormatCMnu  AS QMenuItem '==========
				tag=54
				Caption = "ReFormat hottab C lang"
				OnClick=ReFormatC
			END CREATE '
			CREATE ReFormatHTMMnu  AS QMenuItem '==========
				tag=54
				Caption = "ReFormat HTML Tags"
				OnClick=ReFormatHTML
			END CREATE '
			
			CREATE ReFormatSpcMnu  AS QMenuItem '==========
				tag=55
				Caption = "ReFormat with spaces"
				enabled=0
			END CREATE '
			CREATE ReFormatTabMnuCurSub  AS QMenuItem '==========
				tag=54
				Caption = "ReFormat current sub with HotTabs"
				OnClick=ReFormatHotTabsCurSub
				enabled=1
			END CREATE '
			
			
		END CREATE '
		
		CREATE RunIt AS QMenuItem '==========
			Caption = "Run"
			Hint = "Run"
			CREATE Comp AS QMenuItem
				Caption = "RQ Compile and Run"
				Hint = "RapidQ Basic Compile"
				OnClick=RunItOnClick 'CompileOnClick
				ShortCut="F5"
				
			END CREATE '
			
			CREATE CompDebug AS QMenuItem
				Caption = "RQ Compile and Run with debugging"
				Hint = "RapidQ Basic Compile and Run with debugging"
				'OnClick=CompDebugOnClick
				OnClick=CompDebugOnClick1
			END CREATE '
			
			CREATE CompByte AS QMenuItem
				Caption = "Compile to bytecode"
				Hint = "Compile  to bytecode"
				enabled=0
				'OnClick=CompByteOnClick
			END CREATE '
			
			CREATE CMDLParam AS QMenuItem
				Caption = "Command Line Parameters"
				Hint = "Command Line Parameters"
				OnClick=CMDLParamOnClick
			END CREATE '
			
			CREATE BreakItemR1 AS QMenuItem
				Caption = "-"
			END CREATE
			
			CREATE RunIt1 AS QMenuItem
				Caption = "Run"
				Hint = "Run"
				'OnClick=RunOnClick
				enabled=0
			END CREATE '
			CREATE PauseIt AS QMenuItem
				Caption = "Pause"
				Hint = "Pause"
				OnClick=PauseOnClick
				enabled=0
			END CREATE '
			
			CREATE ContinueIt AS QMenuItem
				Caption = "Continue"
				Hint = "Continue"
				OnClick=ContinueOnClick
				enabled=0
			END CREATE '
			CREATE BreakItemR2 AS QMenuItem
				Caption = "-"
			END CREATE
			
			CREATE StepOver AS QMenuItem
				Caption = "Step Over"
				Hint = "Step Over"
				OnClick=StepOverOnClick
				enabled=0
			END CREATE '
			CREATE StepInTo AS QMenuItem
				Caption = "Step Into"
				Hint = "StepInTo"
				OnClick=StepInToOnClick
				enabled=0
			END CREATE '
			CREATE RunToCurs AS QMenuItem
				Caption = "Run To Cursor"
				Hint = "Run To Cursor"
				OnClick=RunToCursOnClick
				enabled=0
			END CREATE '
			CREATE BreakItemR3 AS QMenuItem
				Caption = "-"
			END CREATE
			CREATE AddWatch AS QMenuItem
				Caption = "Add Watch"
				Hint = "Add Watch"
				OnClick=AddWatchOnClick
			END CREATE '
			CREATE DelWatch AS QMenuItem
				Caption = "Delete Watch"
				Hint = "Delete Watch"
				OnClick=DelWatchOnClick
			END CREATE '
			CREATE DelAllWatch AS QMenuItem
				Caption = "Delete All Watch"
				Hint = "Delete All Watch"
				OnClick=DelAllWatchOnClick
			END CREATE '
			
			CREATE ToggleBreakPoint AS QMenuItem
				Caption = "Toggle BreakPoint"
				Hint = "Toggle BreakPoint"
				OnClick=ToggleBreakPointOnClick
				enabled=0
			END CREATE '
			
			CREATE ClearAllBreakPoints AS QMenuItem
				Caption = "Clear All BreakPoints"
				Hint = "Clear All BreakPoints"
				OnClick=ClearAllBreakPointsOnClick
				enabled=0
			END CREATE '
			CREATE BreakItemR4 AS QMenuItem
				Caption = "-"
			END CREATE
			
			CREATE EvalModif AS QMenuItem
				Caption = "Evaluate/Modify"
				Hint = "Evaluate/Modify"
				'OnClick=EvalModifOnClick
				enabled=0
			END CREATE '
		END CREATE '-RunIt
		
		CREATE SearchIt AS QMenuItem
			OnClick=SearchItOnClick
			ShortCut="Ctrl+L"
			visible=0
		END CREATE
		
		CREATE Options AS QMenuItem
			Caption = "&Options"
			Hint = "Options"
			OnClick=OptionsPrj
		END CREATE
		
		CREATE UtilsMnu AS QMenuItem
			Caption = "&Utilites"
			Hint = "Utils"
			'OnClick=UtilOnClick
			CREATE Str2Chr AS QMenuItem
				Caption = "String to Char"
				OnClick=Str2ChrOnClick
			END CREATE
			CREATE H2Inc AS QMenuItem
				Caption = "Convert .h to .inc"
				OnClick=H2IncOnClick
			END CREATE
			
		END CREATE
		
		CREATE IncFiles AS QMenuItem
			Caption = "Inc Files"
			OnClick=IncFilesMnuOnClick
			visible=0
		END CREATE
		
		CREATE Windows AS QMenuItem
			Caption = "Windows"
			'OnClick=WindowsOnClick
			
			CREATE CloseWin AS QMenuItem
				Caption = "Close Window#" 'CloseWin'
				OnClick=CloseWinOnClick
			END CREATE
			
			CREATE DelEmptyWin AS QMenuItem
				Caption = "Delete disabled Windows" 
				OnClick=DelEmptyWinOnClick
			END CREATE
			CREATE CheckEmptyWin AS QMenuItem
				Caption = "Check wrong file names" 
				OnClick=CheckEmptyWinOnClick
			END CREATE
			
			CREATE FindDisabledFilesMnu AS QMenuItem
				Caption = "Try to find disabled files" 'CloseWin'
				enabled=1
				OnClick=FindDisabledFilesOnClick
			END CREATE
			
			CREATE DelWinList AS QMenuItem
				Caption = "Del WinList" 'CloseWin'
				OnClick=DelWinListOnClick
				enabled=1
			END CREATE
			
			CREATE Sep1324 AS QMenuItem
				Caption = "-"
			END CREATE
			
		END CREATE
		CREATE LastWindMnu AS QMenuItem
			Caption = "LastW"
			OnClick=LastWindClick
			enabled=0
			'Hint="Окно" ''Окно
			'ShowHint=1
			
			
		END CREATE
		
		''    CREATE BackWindMnu AS QMenuItem
		''    Caption = "<< Back"
		''    END CREATE
		''    CREATE FwdWindMnu AS QMenuItem
		''    Caption = "Forward >>"
		''    END CREATE
		
		CREATE BMarkMain AS QMenuItem
			Caption = "BookMarks"
			
			CREATE AddBM AS QMenuItem
				Caption = "Add BookMark"
				Onclick=AddBMark
			END CREATE 
			
			CREATE DelBM AS QMenuItem
				Caption = "Delete BookMark"
				Onclick=DelBMark
			END CREATE 
			
			CREATE SetBM AS QMenuItem
				Caption = "Set BookMark"
				Onclick=SetBMark
			END CREATE 
			
			CREATE SortBM AS QMenuItem
				Caption = "Sort BookMark"
				Onclick=SortBMark
			END CREATE 
			
			CREATE ClearBM AS QMenuItem
				Caption = "Clear List"
				Onclick=ClearBMark
				'enabled=0
			END CREATE 
			
			CREATE Sep1376 AS QMenuItem
				Caption = "-"
			END CREATE
			
		END CREATE 
		
		CREATE LastBMarkMnu AS QMenuItem
			Caption = "LastBM"
			OnClick=LastBMarkClick
			enabled=0
		END CREATE
		
		CREATE Help AS QMenuItem
			Caption = "Help"
			CREATE HelpMainR AS QMenuItem
				Caption = "Help Russian"
				OnClick=HelpRus
			END CREATE
			CREATE HelpMainE AS QMenuItem
				Caption = "Help English"
				OnClick=HelpEn
			END CREATE
			CREATE MnuSep1 AS QMenuItem
				Caption = "-"
			END CREATE
			CREATE ArduinoCLIHelp AS QMenuItem
				Caption = "ArduinoCLI Help"
				OnClick=ArduinoCLIHelpClick
			END CREATE
			CREATE MnuSep2 AS QMenuItem
				Caption = "-"
			END CREATE
			CREATE HelpAbout AS QMenuItem
				Caption = "About"
				OnClick=Help1
			END CREATE
			
			'CREATE ReadMe AS QMenuItem
			'Caption = "ReadMe"
			'OnClick=Help2
			'END CREATE
			
		END CREATE
		
	END CREATE '--- main menu'
	
	
	
	'-- -----------------'SrcDebugEdit ---------------------------------
	CREATE EditPanelPar AS QPANEL
		'Left =2'00
		'Top = 75 '98
		'Width = RQForm.ClientWidth
		'Height =RQForm.ClientHeight-EditPanelPar.top-15'390'0'0 
		align=alclient
		'color=clred
		BorderStyle=bpNone
		BevelWidth=0
		
		CREATE EditPanel AS QPANEL
			Align = alClient
			BorderStyle=bpNone
			BevelWidth=0
			
			CREATE RichPanel AS QPANEL
				Align = altop'Client            '-- Fill top portion of form
				Height =EditPanel.ClientHeight*0.7
				'color=&ha2cfce 'clMaroon
				caption="RichPanel"
				BorderStyle=bpNone
				BevelWidth=0
				
				CREATE MarginEdit AS QRICHEDIT '=== редактор номеров строк ===========
					Align = alLeft  
					'Left =1
					'Top = 10
					Width = 40
					'Height =RichPanel.Height 'SrcEdit
					ScrollBars = 0
					TabOrder = 11
					Wordwrap=false
					color=&HfAB9A8'clAqua
					Font.Color=clBlue
					Font.Name="FixedSys"
					HideSelection=0
					plaintext=1
					visible=0
					readonly=1
					Enabled=0
					BorderStyle=0
					
				END CREATE
				
				CREATE BorderRich AS QRICHEDIT
					Align = alLeft  
					width=5
					color=0
					BorderStyle=0
					
					Left = 100
					
				END CREATE
				
				CREATE SrcEdit AS QRICHEDIT '!!!=== редактор исходника =============================
					Align = alclient'Top  
					ScrollBars = 3
					TabOrder = 10
					'AddStrings "--- source ---"
					Wordwrap=false
					color=&ha2cfce'cl
					HideSelection=false
					OnMouseDown=RichEditOnMouseDown
					OnKeyDown=SrcEditOnKeyDown
					OnKeyUp=SrcEditOnKeyUp
					WantTabs=1
					'tag=33
					plaintext=1
					visible=1
					Font.Name="FixedSys"
					Font=StrFont1
					BorderStyle=0
				END CREATE
			END CREATE '- RichPanel'
			CREATE Splitter2 AS QSPLITTER '-- Note ????????
				Align = alTop            '-- Fill middle
				Cursor = crVSplit
				Height = 5
				color=clRed
				'OnMoved=Splitter2Moved
			END CREATE
			
			CREATE LogPanel AS QPANEL
				Align = alClient            '-- Fill top portion of form
				'color=clgreen
				caption="LogPanel"
				BorderStyle=bpNone
				BevelWidth=0
				CREATE LogTopPanel AS QPANEL
					Align = altop            '-- Fill top portion of form
					color=&HD2D3D3
					caption=""
					'BorderStyle=bpNone
					'BevelWidth=0
					height=22
					CREATE LogTopLeftPanel AS QPANEL
						Align = alleft            '-- Fill top portion of form
						color=&HD2D3D3
						caption=""
						BorderStyle=bpNone
						BevelWidth=0
						width=LogTopPanel.width/2 '27
						
						CREATE LogBtnLeft AS QButtonXP
							XP=1
							Caption = "<-"
							align=alright
							left=111
							Width = 20:
							'Spacing=2:'bmpHandle = sOpen 
							font.name= "system"
							OnClick = LogBtnLeftOnClick
						END CREATE
						CREATE LogBtnCntr AS QButtonXP
							XP=1
							Caption = "||"
							align=alright
							left=220
							Width = 20:
							'Spacing=2:'bmpHandle = sOpen 
							font.name= "system"
							OnClick = LogBtnCntrOnClick
						END CREATE
						CREATE LogBtnRight AS QButtonXP
							XP=1
							Caption = "->"
							align=alright
							left=100
							'Spacing=2:'bmpHandle = sOpen 
							Width = 20:
							font.name= "system"
							OnClick = LogBtnRightOnClick
						END CREATE
						
						
					END CREATE
					
					
				END CREATE
				
				CREATE ArdLogEdit AS QRICHEDIT '=== редактор лога
					Align =alright'Top   alTop '
					Wordwrap=false
					width=30
					text="Arduino log"
					ScrollBars = 3
					Font.Color=cly'clRed
					HideSelection=false
					TabOrder=11
					visible=1
					'font.name="Courier New"
					font.name="JetBrains Mono"
					BorderStyle=0
					color=0
					
				END CREATE
				CREATE SplitterV3 AS QSPLITTER '-- Note position
					Align = alRight            '-- Fill middle
					Cursor = crHSplit
					Height = 5
					width=5
					color=clGreen
					OnMoved=SplitterV3Moved
					
				END CREATE
				
				
				CREATE LogEdit AS QRICHEDIT '=== редактор лога
					Align =alclient'Top   alTop '
					Wordwrap=false
					ScrollBars = 3
					Font.Color=clDGreen'clRed
					HideSelection=false
					TabOrder=11
					visible=1
					font.name="Courier"
					BorderStyle=0
					
				END CREATE
			END CREATE ' !!! logpanel
		END CREATE '-!!! EditPanel'
		
		CREATE SplitterV2 AS QSPLITTER '-- Note position
			Align = alRight            '-- Fill middle
			Cursor = crHSplit
			Height = 5
			color=clGreen
			OnMoved=SplitterV2Moved
		END CREATE
		
		CREATE FilePanel AS QPANEL '!!! --------------
			Align = alRight            '-- Fill portion of form
			Caption = "FilePanel"
			visible=1
			'borderstyle= bvLowered
			BevelOuter=bvNone
			BorderStyle=bpNone
			
			'color=&h887E56
			width=210
			
			CREATE FilePanelHead AS QPANEL
				Align = alTop            '-- Fill top portion of form
				Height=20
				color=clY
				BorderStyle=bpNone
				BevelWidth=0
				
				
				CREATE TabRight AS QTabControl
					
					AddTabs "Sub list","ObjTree","IncTree" ',"ArduinoCli Help" '"Search Result",
					
					'align=alclient
					OnChange = TabRightChange
					showhint=1
					'hint="Hint!"
					MultiLine=1
					'HotTrack = 1 'True   
					'ScrollOpposite=1
					POPUPMENU=TabPopUp
					tabIndex=3
					TabInactiveColor=CLsILVER 'FilePanelHead.COLOR
					color=&HFFFFC7
					'font.size=8
					'TabInactiveFont.size=8
					'TabHeight=55
					'height=50
				END CREATE
				
				
				CREATE ExpandRightPanel AS QButtonXP
					XP=1
					Caption = ""
					font.bold=0
					font.size=12
					Align=alRight 'alLeft
					font.name="Tahoma"
					Width=20
					OnClick=ExpandRightPanelOnClick
				END CREATE
				CREATE CLoseRightPanel AS QButtonXP
					XP=1
					Caption = "X"
					font.bold=1
					Align=alRight
					Width=20
					OnClick=CLoseRightPanelOnClick
				END CREATE
				CREATE MinRightPanel AS QButtonXP
					XP=1
					Caption = "_"
					font.bold=1
					font.name="Tahoma"
					Align=alRight
					Width=20
					OnClick=MinRightPanelOnClick
				END CREATE
				
				
			END CREATE
			
			'---------- сюда вставить панель-контейнер ------ 
			
			
			CREATE ObjTreePanel AS QPANEL '!!! ------
				Align = alClient
				visible=1
				BorderStyle=bpNone
				BevelWidth=0
				
				CREATE ObjTreeView AS QTREEVIEW
					Align = 5
					'AddItems "1","2","3","4","5","6"
					'AddChildItems 2, "Sub 81", "Sub 82", "Sub 83"
					'FullExpand
					OnClick=ObjTreeViewClick
					OnDblClick=ObjTreeViewDblClick
					showhint=1
					hint=""
					POPUPMENU=ObjTreePopUp
					'ObjTreeView.POPUPMENU.autopopup=1
					BorderStyle=0
					
				END CREATE
				CREATE IncTreeView AS QTREEVIEW
					Align = 5
					'AddItems "1","2","3","4","5","6"
					'AddChildItems 2, "Sub 81", "Sub 82", "Sub 83"
					'FullExpand
					OnClick=IncTreeViewClick
					OnDblClick=IncTreeViewDblClick
					showhint=1
					hint=""
					visible=0
					BorderStyle=0
					
					POPUPMENU=IncTreePopUp
				END CREATE
				CREATE SrcEditRight2 AS QRICHEDIT
					align=alclient 
					TabOrder = 8
					HideScrollBars=0
					HideSelection=0
					ScrollBars=3 'ssBoth = 3  
					Wordwrap=0
					visible=0
					
				END CREATE
				
				
				
			END CREATE
			
			CREATE SplitterHFP AS QSPLITTER '-- Note position
				Align = alBottom            '-- Fill middle
				Cursor = crVSplit
				Height = 5
				color=clY
				'OnMoved=SplitterV2Moved
			END CREATE
			
			
			CREATE FileListPanel AS QPANEL '!!! --------------
				visible=1
				Align = alBottom 'alTop 'Client
				height=250
				'color=cldr
				BorderStyle=bpNone
				BevelWidth=0
				
				CREATE FileListPanelHead AS QPANEL
					align=alTop
					height=50
					'color=cllb
					BorderStyle=bpNone
					BevelWidth=0
					
					CREATE DirPopUp AS QPOPUPMENU
						OnPopup=OnDirPopUp
						CREATE AddExt AS QMENUITEM
							Caption ="This extension only"
							OnClick = AddExtOnClick
						END CREATE
						CREATE FileOnly AS QMENUITEM
							Caption ="This file only"
							OnClick =  FileOnlyOnClick
						END CREATE
						CREATE RefreshDir AS QMENUITEM
							Caption = "Refresh list"
							OnClick = RefreshDirTree
						END CREATE
					END CREATE
					
					CREATE DirBox AS QEdit '==== директория
						Text =StartDir 'curdir1$' DirTree.Directory
						Left = 3
						Top = 5
						TabOrder = 2
						color=&HFFE4BD
						Width =FilePanel.Width-40
						OnChange=check1
						BorderStyle=0
						''                AddItems "*.txt", "*.*htm*", "*.", "*.*", "*.ctl"
					END CREATE
					
					CREATE DirBtn AS QButtonXP
						XP=1
						Caption = "..."
						Left =DirBox.Width+6
						Top = 3
						Spacing=2
						Width = 25
						Height = 25
						tag=222 'DirBtn
						OnClick =DirTreeOnClick ' ChangeDirectory1 'btnClick
						bmpHandle = sOpen 
					END CREATE
					CREATE MaskBox AS QComboBox '==== маска
						Text = "*.bas"
						Left = 3
						Top = 30
						TabOrder = 2
						Width =FilePanel.Width-16
						OnChange=check1
						AddItems "*.*",_
						"*.bas;*.inc;*.tpl",_
						"*.ini;*.prj",_
						"*.*htm*", "*.txt" ,"*.log" ,_
						"*.ino;*.pde;*.h;.c;*.cpp"
					END CREATE
				END CREATE
				
				CREATE FileListBox1 AS QFILELISTBOX 'FileListBox1.Directory
					align=alclient
					Left = 3
					OnDblClick=FileListDblClick 'FileLoad
					OnClick=FileClick
					mask=MaskBox.Text'"*.*"
					'AddFileTypes (ftDirectory )
					AddFileTypes (ftNormal,ftReadOnly ,ftHidden ,ftArchive  )
					DelFileTypes(ftDirectory )
					Directory=StartPath$
					'Color=&HcdFFFF'clYellow
					TabOrder=3
					'OnChange=FileListBox1OnChange
					ShowIcons=true
					font.Charset=OEM_CHARSET
					'MultiSelect=True
				END CREATE
			END CREATE '   FileListPanel
			
		END CREATE ' FilePanel
	END CREATE '- EditPanelPar'
	
	CREATE PanelSt as QPANEL 
		Width = 800
		'color=clgreen
		BorderStyle=bpNone
		BevelWidth=0
		
		CREATE StatusPanel5 AS QLABEL
			Align=AlClient
			Caption = "New":
			font.name="FixedSys"
			font.color=cldBlue
			'Left =:Top = 0:
			Transparent = 0
			'Width = 250
			color=clp '&HB5E4A8'&HAE8FA3'clred
			'BorderStyle=bpNone
			'BevelWidth=0
			
		END CREATE
		CREATE Gauge1 AS QGAUGE
			Left =1
			Top = 1
			Width = 320
			Height = 15
			Kind =1
			BorderStyle=0
			visible=0
			ForeColor=clred
			
		END CREATE
		
	END CREATE 
	
	CREATE PanelErr as QPANEL 
		Width = 1500
		Alignment=taLeft
		'Font.color=
		font.name="FixedSys"
		color=&H6764FF'&H00A9FF'clRed
		caption="" 
		OnClick=GoToLineError
		BorderStyle=bpNone
		BevelWidth=0
		
	END CREATE
	
	create  ErrStatusBar AS QSTATUSBAREX 
		AddPanels ""
		Panel(0).Width = 1800
		AddOptPanel PanelErr,0 
		visible=0
	END CREATE
	
	create  StatusBar AS QSTATUSBAREX 'QStatusBar
		AddPanels "Row ", "Col ", "Pos ","Asc ", "EOL ",""
		Panel(0).Width = 105
		Panel(1).Width = 60  
		Panel(2).Width = 120  
		Panel(3).Width = 45  
		Panel(4).Width = 100  
		'Panel(4).Width = 135  
		Panel(5).Width = 800  
		'Panel(0).Alignment = taCenter
		AddOptPanel PanelSt,5 
	END CREATE
	
END CREATE '=== форма'  

call  AddClrString ("5217:OpenDialog.Filter="+(OpenDialog.Filter), clm, LogEdit)
call  AddClrString ("5217:OpenDialog.initialdir="+(OpenDialog.initialdir), clm, LogEdit)


'!!! ---Insert your initialization code here ===================
'!!! RapidQ Tpl
'print 2815 ," ",timer
'DirTree.InitialDir = curdir1$
'print 2819 ," ",timer

call LoadRqTpl
'print 2822," ",timer

'!!! --- ObjBar
declare sub ObjClick (Sender as QMenuItem) '(Sender as QCOOLBTN)
'call  AddClrString ("HOMEDIR+\stdObjbar.inc="+HOMEDIR+"\stdObjbar.inc", clred, LogEdit)

'$INCLUDE "stdObjbar.inc" '
$INCLUDE "stdObjMnu.inc" '


'print 2830," ",timer

'!!!-- ----- добавление меню  EditPopUp ---'
DIM BreakMenuItem(200) AS QMenuItem
for i=0 to 199
	BreakMenuItem(i).Caption = "-"
next i

DIM EditMenuItemPop(50) AS QMenuItem ' popup menu'
Declare Sub Win2DOS
DIM LogMenuItemPop(50) AS QMenuItem ' popup menu'
defint EditHandle ' focused RichEdit handle
'-------------------------------

'- для RichEdit -------------- 
EditMenuItemPop(4).Caption = "To Upper Case"
EditMenuItemPop(4).OnClick = ToUpperCAse

EditMenuItemPop(5).Caption = "To Lover Case"
EditMenuItemPop(5).OnClick = ToLoverCAse

EditMenuItemPop(6).Caption = "Clear"
EditMenuItemPop(6).OnClick = ClearAllRich

EditMenuItemPop(7).Caption = "Search"
EditMenuItemPop(7).OnClick = SearchBlock

EditMenuItemPop(8).Caption = "Debug (LogEdit)"
EditMenuItemPop(8).OnClick = LogValue
'EditMenuItemPop(8).ShortCut="Alt+E"

EditMenuItemPop(9).Caption = "Debug (print)"
EditMenuItemPop(9).OnClick = PrintValue
'EditMenuItemPop(9).ShortCut="Alt+p" 'ShortCut = "Ctrl+N"

EditMenuItemPop(10).Caption = "Add Sub"
EditMenuItemPop(10).OnClick = AddDeclaration 'PrintValue
EditMenuItemPop(10).tag=10

EditMenuItemPop(11).Caption = "GoTo Declaration"
EditMenuItemPop(11).OnClick = PrintValue

EditMenuItemPop(12).Caption = "GoTo Sub/Function"
EditMenuItemPop(12).OnClick = PrintValue

EditMenuItemPop(13).Caption = "Open file"
EditMenuItemPop(13).OnClick = FileLoad '1

EditMenuItemPop(14).Caption = "Add Function"
EditMenuItemPop(14).OnClick = AddDeclaration
EditMenuItemPop(14).tag=14

EditMenuItemPop(15).Caption = "Win->DOS"
EditMenuItemPop(15).OnClick = Win2DOS

EditMenuItemPop(16).Caption = "DOS->Win"
EditMenuItemPop(16).OnClick = DOS2Win

'- -------------------
SrcEdit.MaxMenu.Insert(0,EditMenuItemPop(7))
SrcEdit.MaxMenu.Insert(1,BreakMenuItem(45))
SrcEdit.MaxMenu.Insert(2,EditMenuItemPop(8))
SrcEdit.MaxMenu.Insert(3,EditMenuItemPop(9))
SrcEdit.MaxMenu.Insert(4,EditMenuItemPop(10))
SrcEdit.MaxMenu.Insert(5,EditMenuItemPop(14))
SrcEdit.MaxMenu.Insert(6,EditMenuItemPop(11))
SrcEdit.MaxMenu.Insert(7,EditMenuItemPop(12))
SrcEdit.MaxMenu.Insert(8,BreakMenuItem(47))
SrcEdit.MaxMenu.Insert(9,EditMenuItemPop(13))
SrcEdit.MaxMenu.Insert(10,BreakMenuItem(48))

SrcEdit.MaxMenu.AddItems BreakMenuItem(43),EditMenuItemPop(6),_
BreakMenuItem(44),EditMenuItemPop(4),EditMenuItemPop(5),EditMenuItemPop(15),EditMenuItemPop(16)



SrcEdit.MaxMenu.OnPopup=PopupMe

'!!!-- ----- добавление меню  LogPopUp --------'
'              - сохранить лог файл
'              - поиск
'              - открыть файл

LogMenuItemPop(0).Caption = "Save LogFile"
LogMenuItemPop(0).OnClick = SAveLog
LogMenuItemPop(1).Caption =  "Search"        
LogMenuItemPop(1).OnClick =  SearchBlock    
LogMenuItemPop(2).Caption = "Open File"
LogMenuItemPop(2).OnClick =  OpenFileCurs

LogEdit.MaxMenu.OnPopup=PopupMe
LogEdit.MaxMenu.AddItems BreakMenuItem(46),LogMenuItemPop(0),LogMenuItemPop(1),LogMenuItemPop(2)
'!!! - ------  конец меню popup editor -------------------------- 

'!!! Include files menu--------'
declare sub IncFilesChoose (Sender as qmenuitem)
IncFilesCount=48 '32
'dim IncFilesNAME(IncFilesCount-1) AS STRING
dim IncFilesIndex as integer

dim IncFilesMnu(IncFilesCount-1) AS QMENUITEM
for i%=0 to IncFilesCount-1
	IncFilesMnu(i%).OnClick=IncFilesChoose
	IncFiles.AddItems IncFilesMnu(i%)
	IncFilesMnu(i%).visible=0
	'WindMnu(i%).checked=1
next i%
IncFilesIndex=0
IncFilesFreeIndex=0
IncFilesBeg=0 ' смещение от начала меню
VisibleIncFiles=0
lastIncFiles=1


'!!! Projects menu  -------------------'
declare sub PrjChoose (Sender as qmenuitem)
PrjItemCount= 48 '32
dim PrjNAME(PrjItemCount-1) AS STRING
dim PrjIni(PrjItemCount-1) AS STRING
dim PrjIndex as integer

dim PrjMnuSep AS QMENUITEM
PrjMnuSep.Caption="-"
ProjectsMnu.AddItems PrjMnuSep

dim PrjMnuEdit AS QMENUITEM
PrjMnuEdit.Caption="Edit project file"
'ProjectsMnu.AddItems PrjMnuSep


dim PrjMnu(PrjItemCount-1) AS QMENUITEM ' PrjItemCount =48 

for i%=0 to PrjItemCount-1
	PrjMnu(i%).Caption="***"
	PrjMnu(i%).OnClick=PrjChoose
	'PrjMnu(i%).AddItems PrjMnuEdit
	ProjectsMnu.AddItems PrjMnu(i%)
	PrjMnu(i%).visible=1
	'WindMnu(i%).checked=1
next i%
PrjIndex=-1
PrjFreeIndex=-1
PrjBeg=3 ' смещение от начала меню
VisiblePrj=0
lastPrj=1


'!!! Windows menu  -------------------'
'declare sub WindChoose
WindowsItemCount=48 '32
dim CmdParam(WindowsItemCount) as string
dim CursWin(WindowsItemCount-1) as int
dim Modif(WindowsItemCount-1) as int ' признаки изменения файла
dim Window(WindowsItemCount-1) as string ' массив текстовых окон
dim WindowsIndex as int ' номер текущего окна
dim WindowsFreeIndex as int ' номер свободного окна

defint PrevWind, NextWind, Lastwind, VisibleWindows

VisibleWindows=0
Lastwind=-1
PrevWind=-1
NextWind=-1
WindowsIndex=-1
WindowsFreeIndex=0
declare sub WindChoose  (Sender as qmenuitem)

dim WindMnu(WindowsItemCount-1) AS QMENUITEM
for i%=0 to WindowsItemCount-1
	WindMnu(i%).OnClick=WindChoose
	Windows.AddItems WindMnu(i%)
	WindMnu(i%).visible=0
	'WindMnu(i%).checked=1
next i%
defint InsIdx,ReplFlg: 
InsIdx=6 '2 ' смещение в меню Windows - число первых строк

'!!! Bookmark menu  -------------------'
BMarkItemCount=48 '32
BMarkCtl=6
dim BMark(BMarkItemCount-1,WindowsItemCount-1) as string ' массив строк закладок
dim BMarkPos(BMarkItemCount-1,WindowsItemCount-1) as integer ' массив положений курсора
dim BMarkIndex(WindowsItemCount-1) as int ' номер текущей закладки для текущего окна
dim BMarkFreeIndex (WindowsItemCount-1) as int ' номер свободной закладки для текущего окна
defint LastBMarkIdx(WindowsItemCount-1) ' индекс предыдущей закладка в данном окне
defint VisibleBMarkCnt(WindowsItemCount-1) ' всего определено закладок в данном окне
'defint PrevBMark, NextBMark, 

for i=0 to WindowsItemCount-1
	VisibleBMarkCnt(i)=0
	LastBMarkIdx(i)=-1
	BMarkIndex(i)=-1
	BMarkFreeIndex(i)=0
next i

'declare sub BMarkChoose (Sender as qmenuitem)
'declare sub AddBMark 
'declare sub DelBMark 

dim BMarkMnu(BMarkItemCount-1) AS QMENUITEM ' определяем BMarkItemCount пунктов меню под закладки
for i%=0 to BMarkItemCount-1
	BMarkMnu(i%).OnClick=BMarkChoose
	BMarkMain.AddItems BMarkMnu(i%)
	BMarkMnu(i%).visible=0
	BMarkMnu(i%).Caption=""'+str$(i%)
next i%

'!!! ------ SubsListBox ----------------'
dim SubsListBox (WindowsItemCount-1) AS  QListView

for i%=0 to WindowsItemCount-1
	SubsListBox (i%).parent=ObjTreePanel ' SubList
	'SubsListBox (i%).Left = 2
	'SubsListBox (i%).Top = 1
	'SubsListBox (i%).Height =SubList.ClientHeight -40
	'SubsListBox (i%).Width = SubList.ClientWidth-5
	SubsListBox (i%).Align = alclient
	SubsListBox (i%).AddColumns "Name "+str$(i%),"Begin", "End"
	SubsListBox (i%).Column(0).Width = 300
	SubsListBox (i%).Column(1).Width = 60
	SubsListBox (i%).GridLines=true
	SubsListBox (i%).ViewStyle = vsReport
	SubsListBox (i%).OnClick=GoSubs
	SubsListBox (i%).Tag=100+i%
	SubsListBox (i%).visible=0
next i%

'SubList.top=115
'SubList.left=510

'! ------ end  SubsListBox -----------'
AddWatchMnu.Caption = "Add Watch <"+SrcEdit.SelText+">"
AddWatchMnu.OnClick = AddWatchOnClick

SrcEdit.MaxMenu.AddItems mnuSep31, AddWatchMnu
SrcEdit.MaxMenu.OnPopup=PopupMe

AppName="DebuggerWnd"

DIM ReOpen1 AS QMenuItem, ReOpen2 AS QMenuItem, ReOpen3 AS QMenuItem 
'ObjectInspector.show

'RQForm.WndProc = FormWndProc' best if this is

DIM keyList AS QStringList
DIM LineNumList AS QStringList

result= SendMessageAPI (SrcEdit.handle,EM_EXLIMITTEXT,0,65535*32)
'call  AddClrString ("3835:EM_EXLIMITTEXT="+str$(EM_EXLIMITTEXT), clred, LogEdit)
'result= SendMessageAPI (SrcEdit.handle,EM_SETTABSTOPS,1,4)
SrcEdit.SelectAll
SetPARAFORMAT (SrcEdit) ' устанавливаем табуляции

'print 3067," ",timer

'!!! StartDir=========
'StartDir=curdir$+"\"
StartPath$ = COMMAND$(0)-Application.ExeName 
StartDir=StartPath$
print"StartDir=";StartDir

'call  AddClrString ("3842: StartDir="+(StartDir), clred, LogEdit)

if fileexists (StartDir+"RQdb.ini") then 
	RQdbini.FileName=StartDir+"RQdb.ini"  '!!! "c:\BAS\RAPIDQ\RQ IDE\RQdb.ini"
	'call  AddClrString ("3846:RQdbini.FileName============================================="+(RQdbini.FileName), clo, LogEdit)
else
	ShowMessage ("3851: Ini file not found "+StartDir+"RQdb.ini")
	call  AddClrString ("3042:Ini file not found "+StartDir+"RQdb.ini", clred, LogEdit)
	goto noini
end if


'!!! считываем  директории проекта '-------------
RQdbini.Section="Projects"
PrjPathEdit.text=RQdbini.get("PrjPath",StartDir+"projects\")  ' curdir$+"\projects\")

print"5514: PrjPathEdit.text=";PrjPathEdit.text

call  AddClrString ("3847 PrjPathEdit.text="+(PrjPathEdit.text), clb, LogEdit)

'if direxists(PrjPathEdit.text)=0 then mkdir PrjPathEdit.text

if direxists(PrjPathEdit.text)=0 then 
	call  AddClrString ("5486:PrjPathEdit.text="+(PrjPathEdit.text), clred, LogEdit)
	PrjPathEdit.text=StartDir+"projects\"
	print"5523: New PrjPathEdit.text=";PrjPathEdit.text
	call  AddClrString ("5487:PrjPathEdit.text="+(PrjPathEdit.text), cldg, LogEdit)
	mkdir PrjPathEdit.text
end if

if direxists(PrjPathEdit.text)=0 then 
	ShowMessage "5401: Can't create project path "+PrjPathEdit.text
	'call  AddClrString ("5401: Can't create project path "+PrjPathEdit.text, clred, LogEdit)
	goto noprj:
	'exit sub
end if


'!!! проекты  --
'call  AddClrString ("5409: Начальная загрузка проектов projects из файла RQDB.ini ----------------------", cldb, LogEdit)
VisiblePrj=0

for ip=0 to PrjItemCount-1
	'call  AddClrString ("3879:ip="+str$(ip), clred, LogEdit)
	paramPrj$=RQdbini.get("PrjName"+str$(ip),"") ' имя проекта видимое в меню
	
	if paramPrj$<>"" then 
		PrjNAME(ip)=RQdbini.get("PrjIni"+str$(ip),"")' имя файла проекта
		print"5545: PrjNAME(ip)=";PrjNAME(ip)
		
		if instr(PrjNAME(ip),":")=0 then ' если имя файла проекта не содержится полный путь, то
			PrjNAME(ip)=PrjPathEdit.text+PrjNAME(ip)
			
		end if
		
		'call  AddClrString ("5424:PrjNAME("+str$(ip)+")="+(PrjNAME(ip)), clm, LogEdit)
		
		if fileexists(PrjNAME(ip)-"††† ")=0 then 
			call  AddClrString ("5427:Файл не найден PrjNAME("+str$(ip)+")="+(PrjNAME(ip)), clred, LogEdit)
			if instr(PrjMnu(ip).caption,"†††")=0 then PrjMnu(ip).caption="††† "+paramPrj$ 
			'PrjMnu(ip).enabled=0 
		else
			PrjMnu(ip).caption=paramPrj$
		end if
		PrjMnu(ip).visible=1
	else
		PrjMnu(ip).visible=0
		PrjMnu(ip).caption=""
		if PrjFreeIndex=-1 then PrjFreeIndex= ip
	end if
next ip

PrjIndex=val(RQdbini.get("Index","0")) ' индекс текущего проекта
'print"5566 PrjIndex=";PrjIndex
'call  AddClrString ("2382 PrjIndex="+str$(PrjIndex), clblack, LogEdit)

'PrjMnu(PrjIndex).checked=1 '+PrjBeg

'call  AddClrString ("5446:PrjNAME("+str$(PrjIndex)+")="+(PrjNAME(PrjIndex)), clb, LogEdit)

'!!! PrjNAME(PrjIndex) - полное имя файла проекта -  C:\_F\bas\rapidq\RQIDE\Projects\RapidFRM.prj
'!!! PrjMnu(PrjIndex).caption - имя проекта -RapidFRM 
'ShowMessage("5453: Last project not found - "+PrjNAME(PrjIndex)+" Open another project " )
'call  AddClrString ("3893:Last project not found. Open another project" +PrjNAME(PrjIndex), clred, LogEdit)


if fileexists (PrjNAME(PrjIndex)) =0 then
	ShowMessage("5580: Last project not found - "+PrjNAME(PrjIndex)+" Open another project " )
	'print"PrjIdx=";PrjIdx
	PrjIdx=0
	'print"PrjIdx=";PrjIdx
	
	while fileexists (PrjNAME(PrjIdx)) =0
		
		PrjIdx=PrjIdx+1
		'print"PrjIdx=";PrjIdx
		if PrjIdx>PrjItemCount then
			ShowMessage("5461: No projects found " )
			goto noprj:
		end if
		
	wend
	PrjMnu(PrjIdx).checked=1
	'call  AddClrString ("5574:PrjMnu("+str$(PrjIdx)+"checked)="+str$(PrjMnu(PrjIdx).checked), clred, LogEdit)
else  
	PrjMnu(PrjIndex).checked=1 '+PrjBeg
	LabelPrj.caption="  Project "+PrjMnu(PrjIndex).caption
	
end if




' !!! вообще какой-то бред! настройки проекта хранятся в файле RQDB.ini и больше нигде
noprj:
'print"noprj1=";noprj1

'----------------------- PrjIndex - индекс последнего проекта с которым работали
if fileexists (PrjNAME(PrjIndex)) >0 then '-- если файл проекта существует
	ProjectIni.FileName=PrjNAME(PrjIndex) '======== ================================================
	print"5618: PrjNAME(PrjIndex)=";PrjNAME(PrjIndex)
	print"5619: ProjectIni.FileName=";ProjectIni.FileName
	'call  AddClrString ("5480:ProjectIni.FileName="+(ProjectIni.FileName), clred, LogEdit)
	'call  AddClrString ("5481:RQdbini.FileName="+(RQdbini.FileName), clm, LogEdit)
	'end if
	
	'brem 1
	'!========================
	
else
	CmpPathEdit.text=StartPath$ '+"\"
	'print"CmpPathEdit.text=";CmpPathEdit.text
	'call  AddClrString ("5571:CmpPathEdit.text="+(CmpPathEdit.text), clred, LogEdit)
	IncPathEdit.text=StartPath$+"INC\"
	LibPathEdit.text=StartPath$+"LIB\"
	PrjPathEdit.text=StartPath$+"PROJECTS\"
	TplPathEdit.text=StartPath$+"TEMPLATES\"
	IcoFileEdit.text=StartPath$+"DEFAULT.ICO"
	goto noini:
end if

'!!! считываем  директории компилятора. Может проекты написаны на разных языках.
ProjectIni.Section="Compiler"
'CmpPathEdit.text=ProjectIni.get("CompPath",StartPath$+"")
'IncPathEdit.text=ProjectIni.get("IncPath",StartPath$+"inc\")
'LibPathEdit.text=ProjectIni.get("LibPath",StartPath$+"lib\")
'TplPathEdit.text=ProjectIni.get("IcoFile",StartPath$+"templates\")
'IcoFileEdit.text=ProjectIni.get("TplFile",StartPath$+"default.ico")

FBCmpPathEdit.text=ProjectIni.get("FBCompPath",StartPath$+"FreeBASIC\")

if direxists (CmpPathEdit.text) =0 then CmpPathEdit.text=StartPath$+""
if direxists (IncPathEdit.text) =0 then IncPathEdit.text=StartPath$+"INC\"
if direxists (LibPathEdit.text) =0 then LibPathEdit.text=StartPath$+"LIB\"
if direxists (TplPathEdit.text) =0 then TplPathEdit.text=StartPath$+"TEMPLATES\"
if fileexists (IcoFileEdit.text) =0 then IcoFileEdit.text=StartPath$+"DEFAULT.ICO"

if direxists (FBCmpPathEdit.text) =0 then 
	FBCmpPathEdit.text=StartPath$+"freebasic\"
end if
'!!! ========================================

'erem

ProjectIni.Section="HotDirs" ' история открытых ранее папок - для каждого проекта своя
HotDirsItemCount = val(ProjectIni.get("HotDirsCount","1"))


'!!! Цветовые схемы =======================================


'!!! встроенные цветовые схемы
with RQdbini
	
	RQdbini.Section="IdeLight"  
	
	.write("SyntaxHighLight",str$(SyntaxHLBox.checked))
	.write("HotTabsHighLight",str$(HtHLBox.checked))
	
	
	'HLColor (0 TO 15)
	
	
	.write("RQForm.color",strl$(RQForm.color))
	.write("TabPanel.color",strl$(TabPanel.color))
	.write("ToolPanel.color",strl$(ToolPanel.color))
	.write("Toolbar.color",strl$(Toolbar.color))
	.write("RunDebugBar.color",strl$(RunDebugBar.color))
	.write("TabBAr.color",strl$(TabBAr.color))
	.write("TplListBox.color",strl$(TplListBox.color))
	.write("SectionListBox.color",strl$(SectionListBox.color))
	.write("FindGroupBox.color",strl$(FindGroupBox.color))
	.write("SearchCBox.color",strl$(SearchCBox.color))
	.write("ReplaceComBox.color",strl$(ReplaceComBox.color))
	.write("WhooleWordCheckBox.color",strl$(WhooleWordCheckBox.color))
	.write("EditPanelPar.color",strl$(EditPanelPar.color))
	.write("EditPanel.color",strl$(EditPanel.color))
	.write("RichPanel.color",strl$(RichPanel.color))
	.write("LogPanel.color",strl$(LogPanel.color))
	.write("FilePanel.color",strl$(FilePanel.color))
	.write("FilePanelHead.color",strl$(FilePanelHead.color))
	.write("ObjTreePanel.color",strl$(ObjTreePanel.color))
	.write("ObjTreeView.color",strl$(ObjTreeView.color))
	.write("IncTreeView.color",strl$(IncTreeView.color))
	.write("FileListPanel.color",strl$(FileListPanel.color))
	.write("FileListPanelHead.color",strl$(FileListPanelHead.color))
	.write("DirBox.color",strl$(DirBox.color))
	.write("MaskBox.color",strl$(MaskBox.color))
	.write("MaskBox.Font.color",strl$(MaskBox.Font.color))
	.write("FileListBox1.color",strl$(FileListBox1.color))
	.write("FileListBox1.Font.color",strl$(FileListBox1.Font.color))
	.write("StatusPanel5.color",strl$(StatusPanel5.color))
	
	.write("PrjPropert.color",strl$(PrjPropert.color))
	.write("PropertTab.color",strl$(PropertTab.color))
	.write("PropertTab.font.color",strl$(PropertTab.font.color))
	
	.write("PropPanelEditor.color",strl$(PropPanelEditor.color))
	.write("EditOptGrBox.color",strl$(EditOptGrBox.color))
	.write("SyntaxHLBox.font.color",strl$(SyntaxHLBox.font.color))
	.write("HtHLBox.font.color",strl$(HtHLBox.font.color))
	.write("PanelClrSch.color",strl$(PanelClrSch.color))
	.write("ClrSchLbl.color",strl$(ClrSchLbl.color))
	.write("StatusPanel5.color",strl$(StatusPanel5.color))
	.write("StatusPanel5.color",strl$(StatusPanel5.color))
	
	
	
	RQdbini.Section="IdeDark"  
	.write("darkcolor",darkcolor)
	.write("darkcolor1",darkcolor1)
	.write("darkcolor2",darkcolor2)
	
	RQdbini.Section="Classic Windows"
	.write("BackGroung",str$(&HB5E6EC))
	.write("KeyWords",str$(&HFF0000))
	.write("Operators",str$(&HFF0082))
	.write("Directives",str$(&HFF0000))
	.write("Properties",str$(&HFF0000))
	.write("Types",str$(&HFF0000))
	.write("Comments",str$(&H00BE00))
	.write("Strings",str$(&H0077F9))
	.write("Numbers",str$(&H0000FF))
	.write("Text",str$(0))
	.write("GutterBG",str$(&HF9E7FF))
	.write("GutterTxt",str$(clDBlue))
	.write("LogEditBG",str$(&HFFF2F1))
	.write("IncEditBG",str$(&HE2ECf8))
	.write("FileMngEditBG",str$(&HE2ECB8))
	.write("Custom2",str$(&HE2ECB8))
	
	
	RQdbini.Section="Dark" ' встроенная цветовая схема, не редактируется
	
	'=============
	.write("BackGroung",strl$(1710618))
	.write("KeyWords",strl$(16760767))
	.write("Operators",strl$(16729763))
	.write("Directives",strl$(14013696))
	.write("Properties",strl$(7536870))
	.write("Types",strl$(65408))
	.write("Comments",strl$(39680))
	.write("Strings",strl$(30713))
	.write("Numbers",strl$(255))
	.write("Text",strl$(12632256))
	.write("GutterBG",strl$(275224))
	.write("GutterTxt",strl$(35980))
	.write("LogEditBG",strl$(16773873))
	.write("IncEditBG",strl$(14871800))
	.write("FileMngEditBG",strl$(14871736))
	'===========
	
	RQdbini.Section="NightVision" ' встроенная цветовая схема, не редактируется
	
	'=============
	.write("BackGroung",strl$(3840))
	.write("KeyWords",strl$(16760767))
	.write("Operators",strl$(16729763))
	.write("Directives",strl$(12338033))
	.write("Properties",strl$(7536870))
	.write("Types",strl$(128))
	.write("Comments",strl$(39680))
	.write("Strings",strl$(30713))
	.write("Numbers",strl$(255))
	.write("Text",strl$(45144))
	.write("GutterBG",strl$(16384))
	.write("GutterTxt",strl$(4227200))
	.write("LogEditBG",strl$(16773873))
	.write("IncEditBG",strl$(14871800))
	.write("FileMngEditBG",strl$(14871736))
	'===========
	
	
	
end with


'!!! загружаем последнюю тему и шрифт -----------надо все запихнуть в RQdbini ---------
'dim ClrSchComBoxList as QStringList см выше
ClrSchComBoxList.clear

dim TmpList as QStringList
TmpList.clear


'if fileexists ("ClrSchComBox.lst")> 0 then
'ClrSchComBoxList.LoadFromfile ("ClrSchComBox.lst")
'end if 

RQdbini.Section="ColorSchems"
SchemeCount=val(RQdbini.get("SchemeCount","10"))
'call  AddClrString ("5678:SchemeCount="+str$(SchemeCount), cldp, LogEdit)

for i=0 to SchemeCount-1
	clrsc$=(RQdbini.get("Scheme_"+str$(i),"Classic Windows"))
	'call  AddClrString ("5682:clrsc$="+(clrsc$), clred, LogEdit)
	ClrSchComBoxList.AddItems clrsc$
next i


'call  AddClrString ("5689:ClrSchComBoxList.ItemCount="+str$(ClrSchComBoxList.ItemCount), clred, LogEdit)

'AddItems "Classic Windows", "Dark","NightVision"

for i=0 to ClrSchComBoxList.ItemCount-1
	'call  AddClrString ("5693:ClrSchComBoxList.Item("+str$(i)+")="+(ClrSchComBoxList.Item(i)), cldg, LogEdit)
	
	if ClrSchComBoxList.Item(i)<>"Classic Windows" and _
	ClrSchComBoxList.Item(i)<>"Dark" and _
	ClrSchComBoxList.Item(i)<>"NightVision" then
		ClrSchComBox.AddItems (ClrSchComBoxList.Item(i))
		'call  AddClrString ("5693:ClrSchComBoxList.Item("+str$(i)+")="+(ClrSchComBoxList.Item(i)), cldg, LogEdit)
	end if
next i

'!!! --------- ClrSchComBox.text
'call  AddClrString ("------------------ 4417:ClrSchComBox.text="+(ClrSchComBox.text), clp, LogEdit)

RQdbini.Section="Color Schem"
ClrSchEdit.Text=(RQdbini.get("Color Schem","Classic Windows"))' загружаем последнюю использованную схему

ClrSchComBox.ItemIndex=0

for i=0 to ClrSchComBox.ItemCount-1
	if ClrSchComBox.Item(i)=ClrSchEdit.Text then
		ClrSchComBox.ItemIndex=i
		'call  AddClrString ("5569:ClrSchComBox.ItemIndex="+str$(ClrSchComBox.ItemIndex), clm, LogEdit)
		exit for
	end if
	
next i


if ClrSchEdit.Text="" then 
	ClrSchEdit.Text="Classic Windows"
end if

'call  AddClrString ("4431: Заргужаем схему ClrSchEdit.Text="+(ClrSchEdit.Text)+"<<<", clred, LogEdit)

RQdbini.Section=ClrSchEdit.Text '

with RQdbini
	
	HLColor(0)=val(.get("BackGroung",str$(&HB5E6EC)))
	HLColor(1)=val(.get("KeyWords",str$(&HFF0000)))
	HLColor(2)=val(.get("Operators",str$(&HFF0082)))
	HLColor(3)=val(.get("Directives",str$(&HFF0000)))
	HLColor(4)=val(.get("Properties",str$(&HFF0000)))
	HLColor(5)=val(.get("Types",str$(&HFF0000)))
	HLColor(6)=val(.get("Comments",str$(&H00BE00)))
	HLColor(7)=val(.get("Strings",str$(&H0077F9)))
	HLColor(8)=val(.get("Numbers",str$(&H0000FF)))
	HLColor(9)=val(.get("Text",str$(0)))
	HLColor(10)=val(.get("GutterBG",str$(&HF9E7FF)))
	HLColor(11)=val(.get("GutterTxt",str$(clDBlue)))
	HLColor(12)=val(.get("LogEditBG",str$(&HFFF2F1)))
	HLColor(13)=val(.get("Custom1",str$(&HE2ECf8)))
	HLColor(14)=val(.get("Custom2",str$(&HE2ECB8)))
	HLColor(15)=val(.get("Custom3",str$(&HE2ECB8)))
	
	
	
	
	srcedit.color=HLColor(0)
	BorderRich.color=srcedit.color
	MarginEdit.color=HLColor(10)
	MarginEdit.font.color=HLColor(11)
	LogEdit.color=HLColor(12)
	StrFont.color=HLColor(9)
	srcedit.Font.color=StrFont.color'HLColor(9)
	
	StrFont.Name=.get("srcedit.Font.Name","FixedSys")
	StrFont.Size=val(.get("srcedit.Font.Size","10"))
	
	' проверяем установлен ли шрифт
	DIM CheckFont AS QFONT
	flFontExists=0
	for i=0 to CheckFont.FontCount-1
		if CheckFont.FontName(i)=StrFont.Name then
			flFontExists=1
		else
		end if
		
	next i
	
	if flFontExists=0 then
		StrFont.Name="Courier New"
	end if
	
	HiLiteFont.Name=StrFont.Name
	HiLiteFont.Size=StrFont.Size
	
	srcedit.Font.Name=HiLiteFont.Name '.get("srcedit.Font.Name","FixedSys")
	srcedit.Font.Size=HiLiteFont.Size 'val(.get("srcedit.Font.Size","10"))
	'call  AddClrString ("5733:srcedit.Font.Name="+(srcedit.Font.Name), clp, LogEdit)
	'call  AddClrString ("5735:srcedit.Font.Size="+str$(srcedit.Font.Size), clp, LogEdit)
	
	MarginEdit.font.Name=StrFont.Name
	MarginEdit.Font.Size=StrFont.Size 'val(.get("srcedit.Font.Size","10"))
	
	SrcEdit.visible=0
	SrcEdit.visible=1
	
	'print"HLColor(9)=";HLColor(9)
	
end with 


noini:
IF fileExists("keyword.lst") = 0 THEN 
	'PRINT "ERROR can't find file keyword.lst "+"keyword.lst" 'fullPath$+
	showmessage ("2370 ERROR. Can't find file keyword.lst "+"keyword.lst")
	keyList.text="DECLARE" +crlf+"BIND"+crlf+"CALLFUNC"+crlf +"FOR"+crlf+ "TO"+crlf+ "NEXT"+crlf+"STEP"
	'END
ELSE
	'PRINT "Load file keyword.lst "+"keyword.lst" - загрузка ключевых слов для подсветки
	keyList.LoadFromFile("keyword.lst") 
END IF

ss$=keyList.text 
defint strptr
strptr=sadd(ss$)

'srcedit.color=HLColor(0)

DefInt OldWndProc1, OldWndProc2, OldWndProc3


'!!!--определение новой процедуры RichEdit'
'OldRichEditWndProc = SetWindowLongAPI(SrcEdit.Handle, GWL_WNDPROC, CODEPTR(RichEditWndProc)) 

'brem 0
'!!! ----------------
DefInt iTmpBind
Bind iTmpBind To RichEditWndProc
OldRichEditWndProc = SetNewWndProc (SrcEdit.Handle, iTmpBind)
'
'Bind iTmpBind To RichEditCallback2
'OldWndProc2 = SetNewWndProc (RichEdit2.Handle, iTmpBind)

Bind iTmpBind To StringGridWndProc 'StringGridCallback
OldWndProc3 = SetNewWndProc (LibMngListGrid.Handle, iTmpBind)
'OldWndProc3 = SetNewWndProc (Sender.Handle, iTmpBind)
checkIni=1
SetWindowLong(ObjTreeView.Handle, GWL_STYLE, GetWindowLong(ObjTreeView.Handle,GWL_STYLE) or TVS_NOTOOLTIPS)
SetWindowLong(IncTreeView.Handle, GWL_STYLE, GetWindowLong(IncTreeView.Handle,GWL_STYLE) or TVS_NOTOOLTIPS)
'!!! ----------------
'erem 
checkIni=1

'!!! ---- Arduino -----
ArduinoCliFullName$=CliPathEdit.text

brem 0
'!!! команды cli
erem 

'SortLabelCount=64
'dim SortLabel(SortLabelCount) as QLabel

'goto nolbl
'for i=0 to SortLabelCount-1
'SortLabel(i).parent=LangMngGridPanel
'SortLabel(i).OnClick=SortLabelOnClick
'SortLabel(i).Color=clLB
'SortLabel(i).visible=0
'SortLabel(i).caption="Sort"
'next i

'nolbl:





'LangGrid1.parent=LangMngGridPanel

' Timer1.Enabled = 1 'True !!!

SrcEdit.visible=0
SrcEdit.visible=1
if fileexists("ArduinoCliCmd.txt") then
	SrcEditRight2.LoadFromFile ("ArduinoCliCmd.txt")
end if

'TabRightChange
'TabRight.TabIndex=1
'TabRight.TabIndex=0
'SrcEditRight2.VISIBLE=1 
'call  AddClrString ("5144:SrcEditRight2.VISIBLE="+str$(SrcEditRight2.VISIBLE), clB, LogEdit)
'SubsListOnClick

'SetParent(Forms(i).Handle, FormsPanel.Handle)
'SetParent(SearchListForm.Handle, FilePanel.Handle) ' DesignPan as parent for Forms(i)
'SetParent(SearchListForm.Handle, RichPanel.Handle) ' DesignPan as parent for Forms(i)


wStyle=GetWindowLong(SearchListForm.Handle, GWL_STYLE)
SetWindowLong(SearchListForm.Handle, GWL_STYLE, (wStyle OR WS_CHILD)) ' set CHILD style for window 

SetParent(SearchListForm.Handle, RQForm.Handle) ' DesignPan as parent for Forms(i)
SearchListForm.width=300
SearchListForm.Left=RichPanel.width-SearchListForm.width+5
SearchListForm.Top=RichPanel.Top

wStyle=GetWindowLong(DirTreeForm.Handle, GWL_STYLE)
SetWindowLong(DirTreeForm.Handle, GWL_STYLE, (wStyle OR WS_CHILD)) ' set CHILD style for window 

SetParent(DirTreeForm.Handle, RQForm.Handle) ' DesignPan as parent for Forms(i)
DirTreeForm.width=350
DirTreeForm.Left=RichPanel.width-DirTreeForm.width+5
DirTreeForm.Top=RichPanel.Top


RQFormShowed=0
ArdLogWidth=300
uploadSketch=0
print"uploadSketch=";uploadSketch

'Timer1.Enabled = 1 'True 
'Gauge2.Font.NAme="Arial"
'Gauge2.Font.size=62

'Gauge2.Font=G2Font


SetMenuItemBitmaps(EditMenu.Handle,0,MF_BYPOSITION, hUndo_BMP,hUndo_BMP)
SetMenuItemBitmaps(EditMenu.Handle,2,MF_BYPOSITION, hCut_BMP,hCut_BMP)
SetMenuItemBitmaps(EditMenu.Handle,3,MF_BYPOSITION, hCopy_BMP,hCopy_BMP)
SetMenuItemBitmaps(EditMenu.Handle,4,MF_BYPOSITION, hPaste_BMP,hPaste_BMP)
SetMenuItemBitmaps(EditMenu.Handle,5,MF_BYPOSITION, hDelete_BMP,hDelete_BMP)
SetMenuItemBitmaps(EditMenu.Handle,6,MF_BYPOSITION, hSelectAll_BMP,hSelectAll_BMP)
'SetMenuItemBitmaps(EditMenu.Handle,8,MF_BYPOSITION, hCut_BMP,hCut_BMP)
'SetMenuItemBitmaps(EditMenu.Handle,9,MF_BYPOSITION, hCut_BMP,hCut_BMP)


'$Include  "RQSV105b2023-13Inc-1.bas"


RQForm.ShowModal '!!! ==================================

'!****************************************************
SUB TimerOver 
'print 6184
'Timer1.Interval = 200 
'call  AddClrString ("6039:TimerOver Timer1.Interval="+str$(Timer1.Interval), clred, LogEdit)
'RQForm.Caption = "RQ Debugger                              "+dateR+"   "+Time$

'SubLabel.caption=str$(SubCount)
' SubCount++
' !!! ShowFreePhysicalMemory


'!!! if right$(DirBox.text,1)<>"\" then DirBox.text=DirBox.text+"\"

'1785000
'if SrcEdit.changed=1 or onceflg=0 then 
LSrcEditText$=lcase$(SrcEdit.Text)
onceflg=1
'end if
SubNamePos=SrcEdit.SelStart

nl11:
'SubNamePos=rInstr(SubNamePos, LSrcEditText$," sub")
SubNamePos1=rInstr(SubNamePos, LSrcEditText$,"sub ")
FuncNamePos=rInstr(SubNamePos, LSrcEditText$,"function ")

if FuncNamePos> SubNamePos1 then 
	BegSub=FuncNamePos 
	SubNamePos=FuncNamePos
else 
	
	BegSub=SubNamePos1 
	SubNamePos=SubNamePos1
end if

'SubNamePos=SubNamePos1

SubLineIdx=SendMessageA (SrcEdit.handle,EM_LINEFROMCHAR,BegSub,0)
LSrcEditLine$=Ltrim$(lcase$(SrcEdit.Line(SubLineIdx)))

if Left$(LSrcEditLine$, 4)="sub " or Left$(LSrcEditLine$, 9)="function " then 
	RQForm.Caption = "RQ Debugger_"+ver$+"    "+ SrcEdit.Line(SubLineIdx)
	'RQForm.Caption = "RQ Debugger                  "+ SrcEdit.Line(SubLineIdx)
	NextSubNamePos=SubNamePos+1
	'call  AddClrString ("4057:NextSubNamePos="+str$(NextSubNamePos), clred, LogEdit)
else
	SubNamePos=SubNamePos-1
	'call  AddClrString ("4060:SubNamePos="+str$(SubNamePos), clred, LogEdit)
	
	if SubNamePos>0 then 
		goto nl11
	else
		RQForm.Caption = "RQ Debugger "+ver$+"                 "
	end if
end if

'ищем в SubList
goto noFormPos

'if SubList.WindowState=wsMinimized and  OnceFlgS=0 then
'SetFormPos(SubList.handle, SLLeftM , SLTopM )
'OnceFlgS=1 
'else
'end if

if WatchList.WindowState=wsMinimized and  OnceFlgW=0 then
	SetFormPos(WatchList.handle, WLLeftM , WLTopM )
	OnceFlgW=1 
else
end if

if ObjectInspector.WindowState=wsMinimized and  OnceFlgObj=0 then
	SetFormPos(ObjectInspector.handle, ObjInLeftM , ObjInTopM )
	OnceFlgObj=1 
else
end if

'GetFormPos (SubList)
'if (MinLeft<>SLLeftM or MinTop<>SLTopM) and SubList.WindowState=wsMinimized then 
'SLLeftM=MinLeft:SLTopM=MinTop
'end if

GetFormPos (WatchList)
if (MinLeft<>WLLeftM or MinTop<>WLTopM) and WatchList.WindowState=wsMinimized then 
	WLLeftM=MinLeft:WLTopM=MinTop
end if

GetFormPos (ObjectInspector)
if (MinLeft<>ObjInLeftM or MinTop<>ObjInTopM) and ObjectInspector.WindowState=wsMinimized then 
	ObjInLeftM=MinLeft: ObjInTopM=MinTop
end if


if ObjectInspector.WindowState<> wsMinimized then
	ObjCBox.Width = ObjectInspector.ClientWidth -5
	ObjTab.Width = ObjectInspector.ClientWidth -5
	ObjTab.Height = ObjectInspector.ClientHeight -25
end if


if ObjTab.Height <> PropSGridH then
	PropertiesStringGrid.Height =ObjTab.Height-6
	PropSGridH =ObjTab.Height 
else
end if

if ObjTab.Width<> PropSGridW then
	PropertiesStringGrid.Width =ObjTab.Width-6' ObjectInspector.ClientWidth-8
	PropSGridW=ObjTab.Width
else
end if

noFormPos:
'print 6295

if LogEdit.Height<20 then LogEdit.Height=20 

if DebugSrcForm.Visible=1 then 
	DebugSrcEdit.Height =DebugSrcForm.ClientHeight -5
	DebugSrcEdit.Width = DebugSrcForm.ClientWidth  -5
end if


if ScreenWidth<>Screen.Width or ScreenHeight<>Screen.Height then
	
	ScreenWidth=Screen.Width
	ScreenHeight=Screen.Height
	
	'RQForm.Width =ScreenWidth-5
	'RQForm.Height =ScreenHeight*0.88
end if

'EditPanelPar.Width = RQForm.ClientWidth-4
'EditPanelPar.Height =RQForm.ClientHeight-EditPanelPar.top-15


if SrcEdit.Modified =1 then '
	StatusPanel5.color=&H9B94FF'&H306FFF 'HB5E4A8
	if EditFlg=0 then ' файл загружен из основного редактора TimerOver  !!!!!
		'wmnu$=WindMnu(WindowsIndex).caption
		'WindMnu(WindowsIndex).caption=chr$(149)+str$(WindowsIndex)+ chr$(160)+SrcFileName
		Modif(WindowsIndex)=1
	end if
else
	StatusPanel5.color=&HB5E4A8
	if EditFlg=0 then ' файл загружен из основного редактора
		'WindMnu(WindowsIndex).caption=str$(WindowsIndex)+ chr$(160)+SrcFileName
		Modif(WindowsIndex)=0
	end if
end if


'brem 0
if checkIni=1 then 
	'call  AddClrString ("3267: timer checkIni="+str$(checkIni), clred, LogEdit)
	checkIni=0: 'call RQFormShow: 
end if
'erem

Timer1.Interval = 400 

'print 6341
END SUB


'!***************************************************************************************************
SUB RichShowXY
'StatusBar.Panel(0).Caption = STR$(SrcEdit.WhereY+1)+" : "+STR$(SrcEdit.WhereX+1)
END SUB


'!***************************************************************************************************
SUB  SaveAsOnClick        '' SaveAs item clicked
dim StrUtf8List as QStringList


SaveDialog.InitialDir=StartPath$
'SaveDialog.Filter ="*.bas|*.bas|*.rqb|*.rqb|_
'*.txt|*.txt|*.*|*.*"
brem 0
=====================
SaveDialog.Filter ="1.All Files *.*|*.*|_
2.Bas Files  *.bas;*.rqb;*.vbs|*.bas;*.rqb;*.vbs|_
3.Arduino Files *.ino;*.pde;*.h;*.c;*.cpp|*.ino;*.pde;*.h;*.c;*.cpp|_
4.Config Files *.yml;*.json|*.yml;*.json|_
5.Ini Files *.ini;*.prj|*.ini;*.prj|_
6.Inс Files *.inc;*.tpl|*.inc;*.tpl|_
7.Log Files *.log|*.log|_
8.Txt Files *.txt|*.txt|_
9.Html Files *.*htm*;*.js;*.vbs;*.css|*.*htm*;*.js;*.vbs;*.css|_
10.Hex Files *.Hex|*.Hex|_
11.Graphics Files *.ico;*.bmp|*.ico;*.bmp|_
12.Media Files *.*wav;*.mid;*.mp3|*.*wav;*.mid;*.mp3|" 
=======================
erem
SaveDialog.FilterIndex = 1    '' Use "All Files" as our default
SaveDialog.FileName=SrcFileName '+".bas"
call  AddClrString ("6365:SrcFileName="+(SrcFileName), clb, LogEdit)

fext$=StripFileExt (SrcFileName)
call  AddClrString ("6210:fext$="+(fext$), clred, LogEdit)

fpath$=StripPath (SrcFileName)
call  AddClrString ("6212:fpath$="+(fpath$), clred, LogEdit)

'SaveDialog.FileName=SrcFileName +fext$

pntpos=instr(SaveDialog.Filter, ".")
DelimPos=instr(SaveDialog.Filter, "|")

xTFilter$=left$(SaveDialog.Filter, DelimPos-1) -"."-"*"

SaveDialog.Filter =fext$+"|"+fext$
'call  AddClrString ("4212:xTFilter$="+(xTFilter$), cldr, LogEdit)

IF SaveDialog.Execute THEN
	
	call  AddClrString ("4209:SaveDialog.Filter="+(SaveDialog.Filter), clred, LogEdit)
	call  AddClrString ("4223:SaveDialog.FileName="+(SaveDialog.FileName), clred, LogEdit)
	
	
	if instr(SaveDialog.FileName,".")=0 then
		'SaveDialog.FileName=SaveDialog.FileName+"."+fext$ 'xTFilter$
		SaveDialog.FileName=SaveDialog.FileName+fext$ 'xTFilter$
	end if
	
	'Showmessage SaveDialog.FileName
	
end if

call  AddClrString ("6231:SaveDialog.FileName="+(SaveDialog.FileName), clred, LogEdit)


IF FileExists(SaveDialog.FileName) <> FALSE THEN 
	IF MessageBox("Replace file"+chr$(10)+SaveDialog.FileName +" ?", "Warning!", 1) = 1 THEN
		
	else
		call  AddClrString ("6162:not save FileName="+str$(FileName), cllb, LogEdit)
		exit sub
	END IF
	'elseif fext$=".ino" then
end if

SrcFileName=SaveDialog.FileName
call  AddClrString ("6345:SrcFileName="+(SrcFileName), clred, LogEdit)

'!!SrcFileName - это имя файла выбранного в SaveDialog !!!!

fext$=StripFileExt (SrcFileName)
call  AddClrString ("6252:fext$="+(fext$), clred, LogEdit)

fpath$=StripPath (SrcFileName)
call  AddClrString ("6255:fpath$="+(fpath$), clred, LogEdit)


'! если файл это скетч ino и если он лежит не в своей папке, то создаем для него новую папку 
call  AddClrString ("6235:fext$="+(fext$), clred, LogEdit)

if fext$=".ino" then
	'! для нового скетча создаем новую папку
	fname$=StripFileName (SrcFileName) ' имя файла без пути
	call  AddClrString ("6238:fname$="+(fname$), cldb, LogEdit)
	
	fpath$=StripPath (SrcFileName)
	call  AddClrString ("6242:fpath$="+(fpath$), clred, LogEdit)
	
	if instr(fpath$,(fname$-fext$))=0 then ' если в пути нет имени файла, то добавляем
		fpath$=fpath$+fname$-fext$+"\"
		fpath$=sketchbookPathEdit.text++fname$-fext$+"\"
		mkDir fpath$
		'SaveDialog.InitialDir=fpath$
		SrcFileName=fpath$+fname$
	end if
	call  AddClrString ("6248:fpath$="+(fpath$), clз, LogEdit)
end if

call  AddClrString ("5533:SrcFileName="+(SrcFileName), clo, LogEdit)
StatusPanel5.Caption=SrcFileName

' если файл из списка окон, то при переименовании меняем название в списке
if EditFlg=0 then WindMnu(WindowsIndex).caption=str$(WindowsIndex)+ chr$(160)+SrcFileName

'!!!--- переименовываем закладку и пункт в TabListCmbox
Tab25.Tab(Tab25.TabIndex)=(StripFileName(SrcFileName))
TabListCmbox.Item(Tab25.TabIndex)= str$(WindowsIndex)+chr$(160)+(SrcFileName)
'TabListCmbox.Item(TabListCmbox.ItemIndex)= str$(WindowsIndex)+chr$(160)+(SrcFileName)
'call  AddClrString ("6011:TabListCmbox.ItemIndex="+str$(TabListCmbox.ItemIndex), clb, LogEdit)



'---!!! если скетч Ардуино, то сохраняем в UTF8 


brem 0
=============
dim CP1251Sample as string
CP1251Sample="укенгапролдясмитб"

for i=1 to len (CP1251Sample)
	if instr (Window(WindowsIndex),CP1251Sample[i])>0 then
		'ShowMessage ("12169:Code page is not UTF8")
		'exit sub
		fext$=".NoUTF8"
	end if
	
next i
=================
erem


' C:\_F\bas\rapidq\RQIDE\arduino\portable\sketchbook\BalA-50-1\BalA-50-1.ino

if fext$=".ino" or fext$=".h" or fext$=".cpp" or fext$=".c"   then
	StrUtf8List.Text=SrcEdit.text 'Window(WindowsIndex)
	call  AddClrString ("7262:StrUtf8List.ItemCount="+str$(StrUtf8List.ItemCount), clred, LogEdit)
	
	for i=0 to StrUtf8List.ItemCount-1
		StrUtf8List.Item(i)=ConvertCodePage(StrUtf8List.Item(i), cp_win,cp_utf8)
	next i
	Window(WindowsIndex)=StrUtf8List.text 
	
	'!!! трендец
	'!! файл скетча надо запихивать в папку которая имеет такое же имя
	'!! если текущий скетч уже был в одноименной папке, то при переименовании надо создать переллельную папку с новым именем и туда запихнуть
	'!!переименованный скетч
	'!! а если при переименовании скетч переписывается вообще в другое место, то там нужно в выбранном месте создать эту папку для скетча и
	'!! запихнуть туда скетч 
	
	
	
	
	
	if SaveString(StrUtf8List.text ,SrcFileName  )< 1 then 
		showmessage ("4816: Can't save source into "+SrcFileName)
		exit sub
	end if
	
	
else
	if SaveString(SrcEdit.text ,SrcFileName  )< 1 then 
		showmessage ("4823: Can't save source into "+SrcFileName)
		exit sub
	end if
	
end if


SrcEdit.Modified=0 


END SUB

'!***************************************************************************************************
SUB SaveFileOnClick  
'call  AddClrString ("3334:SaveFileOnClick="+str$(SaveFileOnClick), clred, LogEdit)
' 
dim StrUtf8List as QStringList

if EditFlg=0 then FNSave$=SrcFileName else FNSave$=FNtmp$

'call  AddClrString ("3336:EditFlg="+str$(EditFlg), clred, LogEdit)
'call AddClrString ( "3336: save FileName= "+FNSave$, clb, LogEdit)   
ErrStatusBar.visible=0

if FNSave$="" or FNSave$="New Window.bas" then call SaveAsOnClick: exit sub

IF FileExists(FNSave$) > 0 THEN 
	IF MessageBox("Replace file"+chr$(10)+FNSave$ +" ?", "Warning!", 1) = 1 THEN
	else
		exit sub
	END IF
END IF

call AddClrString ( "saved FileName= "+FNSave$+" "+date$+" "+Time$+" "+str$(len(SrcEdit.text))+" byte", cldgreen, LogEdit)   
'LogEdit.AddStrings (SrcFileName)

'---!!! если скетч Ардуино, то сохраняем в UTF8 
fext$=StripFileExt (FNSave$)
call  AddClrString ("6269:fext$="+(fext$), clred, LogEdit)


brem 0
'!!!!!!!!!
dim CP1251Sample as string
CP1251Sample="укенгапролдясмитб"

for i=1 to len (CP1251Sample)
	if instr (Window(WindowsIndex),CP1251Sample[i])>0 then
		'ShowMessage ("12169:Code page is not UTF8")
		'exit sub
		fext$=".NoUTF8"
	end if
	
next i
'!!!!!!!!!!!
erem


if fext$=".ino" or fext$=".h" or fext$=".cpp" or fext$=".c"    then
	StrUtf8List.Text=SrcEdit.text 'Window(WindowsIndex)
	call  AddClrString ("7262:StrUtf8List.ItemCount="+str$(StrUtf8List.ItemCount), clred, LogEdit)
	
	for i=0 to StrUtf8List.ItemCount-1
		StrUtf8List.Item(i)=ConvertCodePage(StrUtf8List.Item(i), cp_win,cp_utf8)
	next i
	'Window(WindowsIndex)=StrUtf8List.text  
	if SaveString(StrUtf8List.text ,FNSave$  )< 1 then 
		showmessage ("4849: Can't save source into "+SrcFileName)
		exit sub
	end if
	
	
else
	if SaveString(SrcEdit.text ,FNSave$  )< 1 then 
		showmessage ("4856: Can't save source into "+SrcFileName)
		exit sub
	end if
	
end if


if EditFlg=0 then 
	SrcEdit.Modified=0
	Modif(WindowsIndex)=SrcEdit.Modified
end if

END SUB

'!***************************************************************************************************
sub SaveFile '-- ------------------------'
'call AddClrString ( "save SrcFileName= "+SrcFileName, clred, LogEdit)   
'LogEdit.AddStrings (SrcFileName)
if SaveString(SrcEdit.text ,SrcFileName  )=0 then 
	showmessage ("6297: Can't save source into "+SrcFileName)
	exit sub
end if
call AddClrString ( "saved SrcFileName= "+SrcFileName, cldred, LogEdit)   

end sub

'!***************************************************************************************************
sub DeleteFile
IF FileExists(SrcFileName) <> FALSE THEN 
	IF MessageBox("Delete file"+chr$(10)+SrcFileName +" ?", "‚Warning!", 1) = 1 THEN
	else
		exit sub
	END IF
	kill SrcFileName
ELSE
	ShowMessage "4323: File not found "+ SrcFileName
	exit sub
END IF
end sub


'!***************************************************************************************************
sub FileLoad '!!! ===========================

'call  AddClrString ("5445:FileLoad '!!! -- FMngOpenFlg="+str$(FMngOpenFlg), clred, LogEdit)

if FMngOpenFlg<>0 then 
	call  AddClrString ("FileLoad 5448:--!!! ---загрузка без диалога окрытия не из основного редактора =========!!!!", clo, LogEdit)
	goto justload1 ' загрузка без диалога окрытия не из основного редактора
end if

'OpenDialog.Filter="*.*"
call  AddClrString ("6485:OpenDialog.Filter="+(OpenDialog.Filter), clp, LogEdit)

OpenDialogFilter=OpenDialog.Filter ' сохраняем прежний фильтр

call  AddClrString ("6503:OpenDialogFilter="+(OpenDialogFilter), clb, LogEdit)

call  AddClrString ("6505:SrcFileName="+str(SrcFileName), clred, LogEdit)

'!!! OpenDialogFileName="" 'SrcFileName ' OpenDialog.FileName
'call  AddClrString ("5458:OpenDialogFileName="+(OpenDialog.FileName), cldg, LogEdit)
'call  AddClrString ("5459:SrcFileName="+(SrcFileName), clb, LogEdit)
call  AddClrString ("6508:OpenDialog.InitialDir="+str$(OpenDialog.InitialDir), clred, LogEdit)

OpenDialogInitialDir=OpenDialog.InitialDir
call  AddClrString ("5462:OpenDialogInitialDir="+(OpenDialogInitialDir), clb, LogEdit)
'!!! OpenDialog.FileName="" 'SrcFileName 
'call  AddClrString ("5464:OpenDialog.FileName="+(OpenDialog.FileName), clred, LogEdit)

curline$="" '!!! убрали !!! trim$(SrcEdit.SelText-qt) ' выделенный текст пытаемся использовать в качестве имени файла

justload: ' -- открываем диалог открытия файла --
'OpenDialog.FileName=""

FMngOpenFlg=OpenDialog.Execute '!!!---
call  AddClrString ("5471:FMngOpenFlg="+str$(FMngOpenFlg), clred, LogEdit)
OldFilter$=OpenDialog.Filter
filname$=OpenDialog.FileName

'call  AddClrString ("5476:filname$="+(filname$), clred, LogEdit)


justload1: ' ' загрузка из файл-менеджера
'call  AddClrString ("5477:FMngOpenFlg="+str$(FMngOpenFlg), clred, LogEdit)
'call  AddClrString ("5477:FMngOpenFlg=", clred, LogEdit)

IF FMngOpenFlg>0 THEN '!!!--- если был открыт диалог загрузки файла
	FMngOpenFlg=0
	for iw=0 to WindowsItemCount-1 ' проверяем открыт ли уже этот файл
		wfname$=field$(WindMnu(iw).caption,chr$(160),2)
		'call  AddClrString ("5480:wfname$="+(wfname$), clred, LogEdit)
		if wfname$ = filname$ then 
			call  AddClrString ("6519:File already Load filname$="+(filname$), clred, LogEdit)
			if  NoReload=1 then 
				NextWind=iw
				call WindChoose (WindMnu(NextWind) )
				'call  AddClrString ("FileLoad 4116:NextWind="+str$(NextWind), clred, LogEdit)
				
				exit sub
			end if
			DlgRes=MessageDlg("File already loaded! Yes-reload  Cancel-current Ignore-new instance", mtWarning, mbYes OR mbCancel  or mbIgnore , 0)
			IF  DlgRes= mrCancel THEN 'current
				NextWind=iw
				call WindChoose (WindMnu(NextWind) )
				exit sub
				'DlgRes=MessageDlg("File already loaded! Reload? ", mtWarning, mbYes OR mbNo or mbIgnore , 0)
				
			elseif DlgRes= mrYes then ' reload
				reload=1
				NewWindowsIndex=iw: ' если мы перегружаем файл не из текщего окна, то нужно корректно закрыть текущее окно. См. WindChoose.
				exit for
			elseif DlgRes= mrIgnore then ' new instance
				reload=0
			END if
		else
			reload=0
		end if
	next iw
	call  AddClrString ("5510:!!! текущий файл в редакторе был загружен из файл-менеджера EditFlg="+str$(EditFlg), clred, LogEdit)
	
	if EditFlg<>0 then goto justload2 '!!! текущий файл в редакторе был загружен из файл-менеджера
	if WindowsIndex > -1 then 
		'---- сохраняем параметры окна ---
		WindMnu(WindowsIndex).checked=0
		Lastwind=WindowsIndex
		CmdParam(WindowsIndex)=CmdLineEdit.text
		CursWin(WindowsIndex)=SrcEdit.SelStart 'запоминаем положение курсора
		WindMnu(WindowsIndex).checked=0
		SubsListBox(WindowsIndex).visible=0 '!!!----
		'FrmMinimized (SubList) ' закрываем форму SubsList
		Window(WindowsIndex)=SrcEdit.text
		LastBMarkIdx(WindowsIndex) =BMarkIndex (WindowsIndex) ' закладки текущего окна , см. BMChoose
		if BMarkIndex (WindowsIndex) > -1 then BMarkMnu(BMarkIndex (WindowsIndex)).checked=0
	end if
	
	justload2:
	srcedit.color=HLColor(0)
	BorderRich.color=srcedit.color
	
	
	'!!! - определяем  индекс пустого окна -----------
	if reload=1 then 'загружаем в то же окно, где находится файл
		WindowsIndex=NewWindowsIndex
		reload=0 
	else ' загружаем в новое окно
		
		'- ищем первый от начала свободный номер
		for iv=0 to WindowsItemCount-1
			if WindMnu(iv).caption = "" then  
				WindowsFreeIndex=iv
				exit for
			end if
		next iv
		
		if iv=WindowsItemCount then 
			showmessage ("6425: Can't open more then "+str$(WindowsItemCount)+ " windows. Close one."):exit sub
		else
		end if
		
		WindowsIndex=WindowsFreeIndex
		call  AddClrString ("3378:WindowsIndex="+str$(WindowsIndex), clred, LogEdit)
	end if
	
	SrcFileName=FilName$  'OpenDialog.FileName
	call  AddClrString ("4192:SrcFileName="+(SrcFileName), cldg, LogEdit)
	
	if fileexists(SrcFileName)=0 then
		showmessage ("4459:File not found "+SrcFileName)
	else
		call  AddClrString ("6506:SrcFileName="+(SrcFileName), cldb, LogEdit)
		
	end if
	
	call  AddClrString ("4191:SrcFileName="+(SrcFileName), clred, LogEdit)
	SrcFilePath$=lcase$(StripPAth (bsPath(SrcFileName)))
	call  AddClrString ("4056:SrcFilePath$="+(SrcFilePath$), clred, LogEdit)
	call  AddClrString ("6600:CursWin("+str$(WindowsIndex)+")="+str$(CursWin(WindowsIndex)), clred, LogEdit)
	SrcEdit.SelStart=CursWin(WindowsIndex) '!!! ???
	'call  AddClrString ("3647:SrcEdit.SelStart="+str$(SrcEdit.SelStart), clred, LogEdit)
	
	'-- ----- процедура загрузки файла по имени SrcFileName в окно -------------------
	NOPAINT=1
	NOPAINT1=0 '!!! ???
	LoadFile2Window 
	EditFlg=0
	'-- ----- конец процедуры загрузки файла в окно ----------------------
ELSE
END IF

Modiflg=0
NeedSubRefresh=0 ' sub list нуждается в обновлении
SendMessage SrcEdit.handle,EM_EXLIMITTEXT,0,65535*32

'-- обновляем статусбар
call R2Change
SrcEdit.Modified=0

curline$=""
call  AddClrString ("6631:OpenDialogFilter="+str$(OpenDialogFilter), clred, LogEdit)
if len(OpenDialogFilter)>0 then
	'OpenDialog.Filter=OpenDialogFilter ' восстанавливаем  параметры OpenDialog
end if

OpenDialog.FileName="" 'OpenDialogFileName
if len(OpenDialogInitialDir)>0 then
	OpenDialog.InitialDir=OpenDialogInitialDir
end if

FMngOpenFlg=0
EditFlg=0



'call WindChoose (WindMnu(NextWind) )


end sub

'!***************************************************************************************************
sub exitProg
IF MessageBox("Exit?", "OK?", 1) = 1 THEN
	end
else
	exit sub
END IF

end sub
'!***************************************************************************************************
sub Help1
ShowMessage "RQ Debugger IDE v0.709b   2004-2021 г." +chr$(10)+ _
"Andrew Shelkovenko diakin@yandex.com             " +chr$(10)+ chr$(10)+ _
"Special thanks to William Yu  for Rapid-Q Basic programming language"+lf+_
"Special thanks to Andre Victor T. Vicentini for FreeBasic compiler"

end sub

'!***************************************************************************************************
sub Help2
PID=ShellExecute (0,"open",StartPath$+"RQIDE.txt","","",1)
end sub

'!***************************************************************************************************
sub FrmClose (Action as integer)
call SavePrjFileOnClick
'RQdbini.FileName="c:\BAS\RAPIDQ\RQ IDE\RQdb.ini"
RQdbini.FileName=StartDir+"RQdb.ini"
RQdbini.Section="Projects"
RQdbini.write("Index",str$(PrjIndex))

RQdbini.write("PrjPath",PrjPathEdit.text)
RQdbini.write("PrjName"+str$(PrjIndex),PrjNameEdit.text)
RQdbini.write("PrjIni"+str$(PrjIndex),PrjNameEdit.text+".prj") 'MMPathEdit.text)


'IF MessageBox("Close this form?", "Close", 1) = 1 THEN
'-- Close form
Application.terminate
'else
'   Action=0
'  RQForm.ModalResult = 0
'END IF

END  SUB 
'!************************************************************************'
SUB FindText (Sender as QButtonXP)'

if SearchCBox.Text="" then exit sub

dim SearchList as QStringList ' строки из текстового редактора
SearchList.text=SrcEdit.Text

for i=0 to SearchCBox.ItemCount-1 ' проверяем добавлять ли в список текст поиска
	if SearchCBox.Item(i)=SearchCBox.Text then 
		goto notadd
	else
		
	end if
next i
SearchCBox.AddItems SearchCBox.Text ' если новый - то добавляем

notadd:
' проверяем, добавлять ли в список поиска текст из поля замены
for i=0 to SearchCBox.ItemCount-1
	if SearchCBox.Item(i)=ReplaceComBox.text or ReplaceComBox.text="" then 
		goto notadd1
	else
		
	end if
next i
SearchCBox.AddItems ReplaceComBox.text

notadd1:

' проверяем, добавлять ли в список замены  текст из поля замены

for i=0 to ReplaceComBox.ItemCount-1
	if ReplaceComBox.Item(i)=ReplaceComBox.text or ReplaceComBox.text="" then 
		goto notadd2
	else
		
	end if
next i

ReplaceComBox.AddItems ReplaceComBox.text

notadd2:

if ListOfSearchChBox.checked=1 then ' формируем список строк с вхождениями
	SEarchListGrid.rowCount=2
	Gauge1.visible=1
	Gauge1.position=10
	Index=1
	if CAseSensCheckBox.checked=0 then ' без  учета регистра
		'SearchStr$=LCase$(SearchCBox.Text)
		SearchStr$=CharLower(SearchCBox.Text)
		
	end if
	
	Select case codp$
	case "win"
		
	case "dos"
		chartooem SearchStr$,SearchStr$
		'oemtochar SearchStr$,SearchStr$
	case "koi"
	case else
	end select
	WWordtext$=WhooleWordCheckBox.text
	'call  AddClrString ("4328:WWordtext$="+(WWordtext$), clred, LogEdit)
	oldi=0
	Lsep$=",./\| !@#$%^&*()_+=-?><~`;:[]{}"+cr+lf+ht+qt+"'"
	Rsep$=",./\| !@#$%^&*()_+=-?><~`;:[]{}"+cr+lf+ht+qt+"'"
	
	'Rsep$="[()]=><, :'+-*\/;"
	
	for i=0 to SearchList.ItemCount-1 
		if CAseSensCheckBox.checked=0 then ' без  учета регистра
			LSrcEditLine$=CharLower(SearchList.Item(i))
		else
			LSrcEditLine$=SearchList.Item(i)
		end if
		
		Select case WWordtext$
			'"Normal","WholeWord", "Sub/Functions", "Object","Go to line" .htm"
			'<a href="order.htm">Order</a></font></b></td>
			
		case "Normal"
			if instr(LSrcEditLine$,SearchStr$)>0 then 
				'SEarchList.AddItems trim$(SrcEdit.Line(i))-ht
				SEarchListGrid.rowCount=SEarchListGrid.rowCount+1
				SEarchListGrid.cell(2,Index)=trim$(SearchList.Item(i))-ht
				SEarchListGrid.cell(0,Index)=Str$(i+1)
				inc Index
			else
			end if
			
		case "WholeWord"
			bzzz=instr(LSrcEditLine$,SearchStr$) ' первая позиция
			if bzzz>0 then 
				ezzz=bzzz+len(SearchStr$)-1 ' последняя позиция 
				'pwpos=instr(LSrcEditLine$,SearchStr$)
				
				if bzzz=1 then 
					lch$=cr
				else
					lch$=LSrcEditLine$[bzzz-1]
				end if
				
				rch$=LSrcEditLine$[ezzz+1]     
				if  instr(Lsep$,lch$ )>0 and instr(Rsep$,rch$ )>0 then 
					SEarchListGrid.rowCount=SEarchListGrid.rowCount+1
					SEarchListGrid.cell(2,Index)=trim$(SearchList.Item(i))-ht
					SEarchListGrid.cell(0,Index)=Str$(i+1)
					inc Index
					
					
				end if
			end if
			'case "Sub/Functions"
		case "Object"
			gvp=GetVarPos(LSrcEditLine$,SearchStr$)
			if gvp>0 then 
				SEarchListGrid.rowCount=SEarchListGrid.rowCount+1
				SEarchListGrid.cell(2,Index)=trim$(TMPList.Item(i))-ht
				SEarchListGrid.cell(0,Index)=Str$(i+1)
				inc Index
			end if
		case else
			
		end select
		
		
		
		if i>oldi+150 then 
			oldi=i
			Gauge1.position=i/(TMPList.ItemCount+1)*100
		end if
		doevents
	next i
	ListGridReady=1
	Gauge1.visible=0
	exit sub
end if

dim LowSrcTxt as string
dim LowLogTxt as string
defstr LSelText
Modiflg=SrcEdit.modified
SelSt =SrcEdit.SelStart

SearchStr$=SearchCBox.Text
LowSrcTxt=SrcEdit.Text+" "
LowLogTxt =LogEdit.Text+" "

if len(SearchStr$)>50 then SearhMsg$=left$(SearchStr$, 50)+"..." else SearhMsg$=SearchStr$
if EditHandle=0 then EditHandle=SrcEdit.Handle

if CAseSensCheckBox.checked=0 then ' без  учета регистра
	CharLower SearchStr$
	CharLower LowSrcTxt
	CharLower LowLogTxt 
end if


nopres:
Select case codp$
case "win"
case "dos"
	chartooem SearchStr$,SearchStr$
	'oemtochar SearchStr$,SearchStr$
case "koi"
case else
end select

select case EditHandle
	
case  SrcEdit.Handle ', GridEdit.Handle
	with SrcEdit
		selB=SelSt '.SelStart
		LSelText=.SelText
		if CAseSensCheckBox.checked=0 then ' без  учета регистра
			CharLower LSelText
		end if
		select case Sender.tag
		case 21, 24 'Поиск вперед в RightText
			IF SelSt > 0 THEN
				if Sender.tag=24 then '!!! -- Replace---
					if LSelText=SearchStr$ then 
						NOPAINT=1
						SrcEdit.Text=delete$(SrcEdit.Text, selB+1,LEN(SearchStr$))
						SrcEdit.Text=insert$(ReplaceComBox.text, SrcEdit.Text, selB+1)
						NOPAINT=0
						LowSrcTxt=SrcEdit.Text
						if CAseSensCheckBox.checked=0 then ' без  учета регистра
							CharLower LowSrcTxt
						end if  
					end if
					SelSt=selB+len(ReplaceComBox.text)
				end if
			ELSEif SelSt=Len (.Text) then
				SHOWMESSAGE ("4732:"+SearhMsg$+" not found.")
			END IF
			if LSelText=SearchStr$ then selB=SelSt +.SelLength+1
			SelSt = INSTR(selB, LowSrcTxt, SearchStr$)-1
			doevents
			select case WhooleWordCheckBox.text
			case "Normal"
				.SelStart=SelSt
				.SelLength = LEN(SearchStr$)
				.SelAttributes.Color =clRed
			case "WholeWord" ' nolist
				chidx=SelSt+LEN(SearchStr$)+1
				linz=SendMessageA (.handle,EM_LINEFROMCHAR,SelSt,0)
				FirstChar=.GetLINEIdx(linz)
				Lsep$=",./\| !@#$%^&*()_+=-?><~`;:[]{}"+cr+lf+crlf+ht+qt+"'"
				Rsep$="[()]=><, :'+-*\/;"
				
				if (instr(Lsep$,LowSrcTxt[SelSt] )>0 or FirstChar=SelSt) and instr(Lsep$,LowSrcTxt[chidx] )>0 then
					.SelStart=SelSt
					.SelLength = LEN(SearchStr$)
					.SelAttributes.Color =clRed
				else
					SelSt = INSTR(selB+LEN(SearchStr$), LowSrcTxt, SearchStr$)-1
					if SelSt<1 then ''Len (.Text)
						SHOWMESSAGE ("All <"+chr$(10)+ SearhMsg$+chr$(10)+ "> done")
						exit sub
					end if
					inc SelSt
					goto nopres
				end if
			case "Sub/Functions"
				
			case "Object"
			case "Go to line"
				LineNumber=val(SearchCBox.text)
				Gotoline (LineNumber)
			case else
			end select
			
			Pres:
			if SelSt=0 then ''Len (.Text)
				SHOWMESSAGE ("All <"+chr$(10)+ SearhMsg$+chr$(10)+ "> done")
				.SelStart=0
				SelSt=.SelStart
			END IF
			
			
		case 22 'Поиск назад
		case else
		end select
		call R2Change
		if .SelStart=Len (.Text) then
			SHOWMESSAGE ("All <"+chr$(10)+ SearhMsg$+chr$(10)+ "> done")
			.SelStart=0
			SelSt=.SelStart
		END IF
	end with
	
case  LogEdit.Handle
	with LogEdit
		LSelText=.SelText
		CharLower LSelText
		if LSelText=SearchStr$ then .SelStart=.SelStart +.SelLength+1
		select case Sender.tag
		case 21 'Поиск вперед в RightText
			.SelStart = INSTR(.SelStart, LowLogTxt, SearchStr$)-1
			SelSt=.SelStart
			
			iF .SelStart > 0 THEN
				.SelLength = LEN(SearchStr$)
				.SelAttributes.Color =clRed
			ELSEif .SelStart=Len (.Text) then
				SHOWMESSAGE ("4804:"+SearhMsg$+" not found.")
			END IF
			
		case 22 'Поиск назад
		case else
		end select
		
		RichRow1=str$(SrcEdit.WhereY+1)
		RichCol1=str$(SrcEdit.WhereX+1)
		StatusBar.Panel(0).caption = "Row "+RichRow1+":"+  str$(SrcEdit.LineCount)
		StatusBar.Panel(1).caption = "Col "+RichCol1  
		StatusBar.Panel(3).caption = "Asc "+char1$
		StatusBar.Panel(2).caption = "Pos "+str$(pos1)+":"+  str$(SrcEditLen1)
		
		if .SelStart=Len (.Text) then
			SHOWMESSAGE ("All <"+chr$(10)+ SearhMsg$+chr$(10)+ "> done")
			.SelStart=0
			SelSt=.SelStart
		END IF
	end with
case else
end select

SrcEdit.modified=Modiflg'Modif(WindowsIndex)
'call  AddClrString ("1332 SrcEdit.modified="+str$(SrcEdit.modified), clred, LogEdit)

END SUB

'!***************************************************************************************************
SUB Check(key AS BYTE)
Handle = GetFocus():'if Handle = MaskBox.Handle then 
IF key = 13 THEN  'call  MaskChange'' EXIT SUB
	select case handle
	case MaskBoxHdl
		''   for i=0 to MaskBox.ItemCount-1
		''     if MaskBox.Text=MaskBox.Item(i) then goto 123
		''   next
		
		''  MaskBox.AddItems  MaskBox.Text: 'sound 100, 1
	case SearchEditHdl
		'' for i=0 to SearchEdit.ItemCount-1
		''   if SearchEdit.Text=SearchEdit.Item(i) then goto 123
		'' next
		'SearchEdit.AddItems  SearchEdit.Text: 'sound 100, 1
	case else
	end select
else
end if


'FileListBox1.mask=MaskBox.Text
end sub 

'!***************************************************************************************************
sub ObjTabChange
SELECT CASE ObjTab.TabIndex
CASE 0
	PropertiesStringGrid.Visible = True
	EventsStringGrid.Visible = False
CASE 1
	PropertiesStringGrid.Visible = False
	EventsStringGrid.Visible =  True
end select
END SUB

'!***************************************************************************************************
sub ObjInspectorOnClick
if ObjectInspector.visible=false then ObjectInspector.show
END SUB

'!***************************************************************************************************
sub OptionsPrj
'if PrjPropert.visible=false then PrjPropert.show
PrjPropert.show
END SUB

'!***************************************************************************************************
sub DirTreeOnClick (Sender as QButtonXP)
DirTag=Sender.tag
call  AddClrString ("6864:DirTag="+str$(DirTag), clb, LogEdit)

if dirtag=0 then exit sub



Select case DirTag
case 300 ' !!! IDEPath
	
	if CreateDirIfNotExists (IDEPathEdit.text)>0 then DirBox.text=IDEPathEdit.text
	
case 301' !!! CompPath
	
	if CreateDirIfNotExists (CmpPathEdit.text)>0 then DirBox.text=CmpPathEdit.text
	
case 302' !!! IncPath
	if CreateDirIfNotExists (IncPathEdit.text)>0 then DirBox.text=IncPathEdit.text
case 303
	if CreateDirIfNotExists (LibPathEdit.text)>0 then DirBox.text=LibPathEdit.text
case 304
	if CreateDirIfNotExists (PrjPathEdit.text)>0 then  DirBox.text=PrjPathEdit.text
case 305
	if CreateDirIfNotExists (TplPathEdit.text)>0 then  DirBox.text=TplPathEdit.text
case 306
	
case 307
	if CreateDirIfNotExists (IcoPathEdit.text)>0 then  DirBox.text=IcoPathEdit.text
	
	
case 308 
	if CreateDirIfNotExists (FBLibPathEdit.text)>0 then  DirBox.text=FBLibPathEdit.text
	
case 309
	if CreateDirIfNotExists (FBPrjPathEdit.text)>0 then  DirBox.text=FBPrjPathEdit.text
case 310
	if CreateDirIfNotExists (FBTplPathEdit.text)>0 then  DirBox.text=FBTplPathEdit.text
case 311
	if CreateDirIfNotExists (FBIcoFileEdit.text)>0 then  DirBox.text=FBIcoFileEdit.text
case 312
	if CreateDirIfNotExists (ArdCmpPathEdit.text)>0 then  DirBox.text=ArdCmpPathEdit.text
case 313
	'DirBox.text=FBLibPathEdit.text
case 314
	'DirBox.text=FBLibPathEdit.text
case 315
	'DirBox.text=FBLibPathEdit.text
case 316
	'DirBox.text=FBLibPathEdit.text
case 317
	'DirBox.text=FBLibPathEdit.text
case 318
	'DirBox.text=FBLibPathEdit.text
case 319
	'DirBox.text=FBLibPathEdit.text
case 320
	if CreateDirIfNotExists (sketchbookPathEdit.text)>0 then  DirBox.text=sketchbookPathEdit.text
case 321
	'DirBox.text=FBLibPathEdit.text
case 322
	if CreateDirIfNotExists (BuildPathEdit.text)>0 then  DirBox.text=BuildPathEdit.text
case 323
	if CreateDirIfNotExists (ArdLibraryPathEdit.text)>0 then  DirBox.text=ArdLibraryPathEdit.text
case 324
	'DirBox.text=FBLibPathEdit.text
	
case else
	
end select


'if DirBox.text="" then DirBox.text=StartPath$ '"c:\"
DirTree.Directory=DirBox.text

DirTreeForm.show

END SUB
'!***************************************************************************************************

'!***************************************************************************************************
sub WatchListOnClick
'if WatchList.visible=false then 
WatchList.show
WatchListFormResize

END SUB
'!***************************************************************************************************
sub SubsListOnClick
'print"7150 SubsListOnClick="
'exit sub
'call  AddClrString ("6052:SubsListOnClick="+str$(SubsListOnClick), clred, LogEdit)
'if SubList.visible=false then 
AddSubs
'SubList.show
'SubListFormResize
'sublist.show
FilePanel.visible=1
'call  AddClrString ("6058:FilePanel.visible="+str$(FilePanel.visible), clred, LogEdit)
SubsListBox(WindowsIndex).visible=1
ObjTreePanel.visible=1
END SUB

'!***************************************************************************************************
sub OpenPrjFrmOnClick
if NewForm.visible=false then NewForm.show
END SUB
'!***************************************************************************************************
sub  OpenDebugSrcOnClick
if DebugSrcForm.visible=false then DebugSrcForm.show
END SUB

'!***************************************************************************************************
sub RunItOnClick1
if dbgflg=1 then dbgflg=0:goto dbglbl1

lcext$=lcase$(StripFileExt (SrcFileName))
'call  AddClrString ("4016:lcext$="+(lcext$), clred, LogEdit)

if lcext$ <> ".bas" and lcext$ <> ".rqb" and lcext$ <> ".rq" then
	ShowMessage ("Sourcefile possibly not bas file: "+SrcFileName)
	EXIT Sub
else
end if

ErrStatusBar.visible=0
'call AddClrString ( "save SrcFileName= "+SrcFileName, clred, LogEdit)   
rz =SaveString(SrcEdit.text ,SrcFileName  )

if rz=0 then 
	showmessage ("Can't save source into "+SrcFileName)
	exit sub
end if
Modiflg=0
SrcEdit.Modified=0

dbglbl1:

CHDIR  startdir

'сall AddClrString ( "compile SrcFileName= "+SrcFileName, cldblue, LogEdit)   
SrcFileNameQT$=qt+SrcFileName+qt 

ExeFileName$=FullPathNoExt(SrcFileNameQT$)+".exe"+qt  :
'call AddClrString ( "ExeFileName$= "+ExeFileName$, cldred, LogEdit)   
end sub


'!***************************************************************************************************
SUB Runclick
dim exename as string
dim retval as long
'Compileclick  

ExeName = replace$( ActiveFile,".exe",len(ActiveFile)-3)
showmessage "File "+exename+" Compiled, proceed to run..."
'retval = shell(ExeName , 1)
RUN "rundll32.exe url.dll,FileProtocolHandler " & ExeName
'Run "Start " + ExeName
END SUB



'!***************************************************************************************************
sub RunItOnClick
'call  AddClrString ("RunItOnClick="+str$(RunItOnClick), clred, LogEdit)
'call  AddClrString ("dbgflg="+str$(dbgflg), cldg, LogEdit)
'call  AddClrString ("3910:SrcFileName="+(SrcFileName), clred, LogEdit)
'call  AddClrString ("4958:CmdLineEdit.text="+(CmdLineEdit.text), cldb, LogEdit)

if dbgflg=1 then dbgflg=0:goto dbglbl

lcext$=lcase$(StripFileExt (SrcFileName))

'call  AddClrString ("4841:mbNo="+str$(mbNo), clred, LogEdit)
'call  AddClrString ("4841:mbYes="+str$(mbYes), clred, LogEdit)

if lcext$ <> ".bas" and lcext$ <> ".rqb" and lcext$ <> ".rq" then
	
	zzzx=MessageDlg("Sourcefile is not basic file: "+SrcFileName+" Save it as "+SrcFileName+".bas ?", mtWarning, mbYes OR mbNo, 0)
	'call  AddClrString ("4842:zzzx="+str$(zzzx), clred, LogEdit)
	IF  zzzx = mrYes THEN
		
		SaveAsOnClick
	else
		ShowMessage ("7231:Sourcefile not compiled and saved: "+SrcFileName)
		EXIT Sub
		
	end if
	
end if

ErrStatusBar.visible=0
call AddClrString ( "7239:Save SrcFileName= "+SrcFileName, clred, LogEdit)   
rz =SaveString(SrcEdit.text ,SrcFileName  )

if rz=0 then 
	showmessage ("7243:Can't save source into "+SrcFileName)
	exit sub
end if
Modiflg=0
SrcEdit.Modified=0

dbglbl:
'call  AddClrString ("startdir="+(startdir), clred, LogEdit)
'CHDIR  startdir
SrcDir$=StripPath(SrcFileName)
if SrcDir$="" then SrcDir$=StartPath$

call  AddClrString ("7255:SrcDir$="+(SrcDir$), clred, LogEdit)
CHDIR SrcDir$

'сall AddClrString ( "compile SrcFileName= "+SrcFileName, cldblue, LogEdit)   
SrcFileNameQT$=qt+SrcFileName+qt 

ExeFileName$=FullPathNoExt(SrcFileNameQT$)+".exe"+qt  :
call AddClrString ( "7262:ExeFileName$= "+ExeFileName$, cldred, LogEdit)   

msgFile$=qt+PrjPathEdit.text+"rc.msg"+qt: ''  print "msgFile$=" ,msgFile$
'call AddClrString ( "msgFile$= "+msgFile$, cldred, LogEdit)   

cmpFileName$=qt+CmpPathEdit.text+"rc.exe"+qt '' : print "cmpFileName$=" ,cmpFileName$
'call AddClrString ( "cmpFileName$= "+cmpFileName$, cldred, LogEdit)   

'call AddClrString ( "before kill fileexists (msgFile$-qt)= "+str$(fileexists (msgFile$-qt)), clred, LogEdit)   
if fileexists (msgFile$-qt)>0 then kill msgFile$-qt
'call AddClrString ( "after kill fileexists (msgFile$-qt)= "+str$(fileexists (msgFile$-qt)), clred, LogEdit)   

'--USAGE: RC [options] filename [filename.exe]

'C:\BAS\RAPIDQ\rc.exe -IC:\BAS\RAPIDQ\INC -LC:\BAS\RAPIDQ\LIB "C:\BAS\RAPIDQ\RQ IDE\RQdebugger093.bas" "C:\BAS\RAPIDQ\RQ IDE\RQdebugger193.exe" > "C:\BAS\RAPIDQ\RQ IDE\rc.msg"

if direxists(IncPathEdit.text)>0 then 
	IncPathOpt$=C_Style (" -I"+qt+IncPathEdit.text +qt)
	call  AddClrString ("7176 IncPathOpt$="+(IncPathOpt$), clred, LogEdit)
else 
	showmessage ("3135 Path not present "+IncPathEdit.text)
	IncPathOpt$=""
end if

if direxists(LibPathEdit.text)>0 then 
	LibPathOpt$=C_Style (" -L"+qt+LibPathEdit.text +qt)
	'call  AddClrString ("3146 LibPathOpt$="+(LibPathOpt$), clred, LogEdit)
else 
	showmessage ("3144 Path not present "+LibPathEdit.text)
	LibPathOpt$=""
end if

if fileexists(IcoFileEdit.text)>0 then 
	IcoPathOpt$=C_Style (" -G"+qt+IcoFileEdit.text +qt)
	'call  AddClrString ("3151 IcoPathOpt$="+(IcoPathOpt$), clred, LogEdit)
	
else 
	showmessage ("3149 Ico file not found "+IcoFileEdit.text)
	IcoPathOpt$=""
end if

fin11$=" echo fin1>fin1"

CompileStr$=cmpFileName$+" -r "+_
IncPathOpt$+_
LibPathOpt$+_
IcoPathOpt$+_ 
" "+SrcFileNameQT$+_ 
" "+ExeFileName$+ crlf+fin11$

'call  AddClrString ("CompileStr$="+(CompileStr$), clB, LogEdit)

kill (ExeFileName$-qt)
sleep 0.5
zzx=SaveString(CompileStr$,"rc.bat")

msgFile$="DUMP.$$$": ''  print "msgFile$=" ,msgFile$
kill msgFile$


'PID=SHELL ("command.com /c rc.bat ",0)
'PID=SHELL (CompileStr$,0)
'PID=RUN (CompileStr$)
RUN "rc.bat"


wi=0
kill "fin1"

waitforexefile:
'zzz=WaitForSingleObject (PID, 5000)
'sleep (0.100)
'call  AddClrString ("7244:fileexists("+(ExeFileName$-qt)+")="+str$(fileexists(ExeFileName$-qt)), clred, LogEdit)
sleep 0.3
'if fileexists(ExeFileName$-qt)=0 then 
if fileexists("fin1")=0 then 
	inc wi
	
	if wi>80 then 
		call  AddClrString ("7245: Exe not compiled="+ExeFileName$, 0, LogEdit)
		print"7245: Exe not compiled=";ExeFileName$
		exit sub
	end if
	
	sleep 0.3
	goto waitforexefile
	
end if



if fileexists(msgFile$)=0 then
	
	Showmessage("7243: File not found "+msgFile$)
	exit sub
end if

rezcmp$=LoadString(msgFile$-qt) 
'call  AddClrString ("rezcmp$="+(rezcmp$), clred, LogEdit)
MsgList.LoadFromFile(msgFile$-qt) 

call AddClrString ( "7201: MsgList.Text"+MsgList.Text, clDgreen, LogEdit)   

if rezcmp$="0" then
	call AddClrString ( "Can't open file "+msgFile$-qt, clred, LogEdit) 
	exit sub  
else
	'StrFontA.Name="FixedSys"
	StrFontA.Color=clBlue
	'call AddFontString ( "------------------compiler message -------------- "+, StrFontA, LogEdit)   
	LogEdit.HideSelection=1
	call  AddClrString ("RunItOnClick 7210:MsgList.ItemCount-1="+str$(MsgList.ItemCount-1), clred, LogEdit)
	MsgLItemCount=MsgList.ItemCount
	errlineNumb=0
	filName$=""
	for izz=0 to MsgLItemCount-1 ' !!!--- определяем вид ошибки ----
		'call  AddClrString ("RunItOnClick 4732:izz="+str$(izz), clred, LogEdit)
		if MsgList.Item(izz)="" then goto nexti47
		'call  AddClrString ("4734:MsgList.Item("+str$(i)+")="+(MsgList.Item(i)), clred, LogEdit)
		errp=instr(ucase$(MsgList.Item(izz)),"LINE ")
		expp=instr(ucase$(MsgList.Item(izz)),"EXPECTED")
		warnp=instr(ucase$(MsgList.Item(izz)),"WARNING")
		fileerr=instr(ucase$(MsgList.Item(izz)),"FILE IN ERROR: ")
		
		if errp>0 or expp>0 or warnp>0 then '
			LogEdit.HideSelection=1
			if errp>0 or warnp>0 then
				errlineNumb=val(field$(MsgList.Item(izz),":",1)-"Line ")
				'call AddFontString ( "4735: errlineNumb="+str$(errlineNumb) , StrFontA, LogEdit)
			elseif expp>0 then
				'Line 11, 4: Expected = but got "/"
				errlineNumb=val(field$(MsgList.Item(izz),",",1)-"Line ")
				'call AddFontString ( "4739: errlineNumb="+str$(errlineNumb) , StrFontA, LogEdit)
			else
			end if
			'ErrStatusBar.visible=1
			PanelErr.caption=MsgList.Item(izz)
			StrFontA.Color=clRed
			'call AddFontString ( MsgList.Item(izz), StrFontA, LogEdit)   
			StrFontA.Color=clBlue
			' переходим на строку с ошибкой
			'GoToLineError
		elseif fileerr>0 then
			'File in error: C:\BAS\RAPIDQ\RQIDE\RQdebugger454.bas
			filName$=MsgList.Item(izz)-"File in error: "
			
		else
			call AddFontString ( MsgList.Item(izz)+, StrFontA, LogEdit)   
		end if 
		nexti47:
	next izz
	if errlineNumb>0 then    
		
		'call  AddClrString ("RunItOnClick 4751:filName$="+(filName$), clred, LogEdit)
		NoReload=1
		'call  AddClrString ("RunItOnClick 4758:NoReload="+str$(NoReload), clred, LogEdit)
		FMngOpenFlg=1
		'call  AddClrString ("RunItOnClick 4760:FMngOpenFlg="+str$(FMngOpenFlg), clb, LogEdit)
		call FileLoad '!!! --------
		NoReload=0
		FMngOpenFlg=0
		'call  AddClrString ("RunItOnClick 4764:FMngOpenFlg="+str$(FMngOpenFlg), clred, LogEdit)
		ErrStatusBar.visible=1
		
		'call  AddClrString ("RunItOnClick 4769:errlineNumb="+str$(errlineNumb), clred, LogEdit)
		' переходим на строку с ошибкой
		GoToLineError
	end if
	'LogEdit.Font.Name="FixedSys"
	'LogEdit.AddString "------------------compiler message -------------- "
	'call AddFontString ( crlf+rezcmp$, StrFontA, LogEdit)   
	'call AddFontString ( "------------------end compiler message -------------- "+, StrFontA, LogEdit)   
	
end if

StrFontA.Color=clDBlue
StrFontA.Name="Courier"
call AddFontString ( "7276: run ExeFileName$= "+ExeFileName$, StrFontA, LogEdit)   
call  AddClrString ("8129:CmdLineEdit.text="+(CmdLineEdit.text), clred, LogEdit)

'sleep (2)

if fileexists(ExeFileName$-qt)>0 then
	'PID = SHELL(ExeFileName$+" "+CmdLineEdit.text +" > CmpOutput.txt", 1)
	'RUN (ExeFileName$)
	WinExec ExeFileName$,1
	call AddClrString ( "7281: run PID= "+str$(PID), clred, LogEdit)   
	
else
	'sleep (2)
	'PID = SHELL(ExeFileName$+" "+CmdLineEdit.text +" > CmpOutput.txt", 1)
	call  AddClrString ("7355:fileexists("+(ExeFileName$)+")="+str$(fileexists(ExeFileName$)), clred, LogEdit)
	
end if
'call  AddClrString ("5157:ExeFileName$ CmdLineEdit.text + CmpOutput.txt="+ExeFileName$+" "+CmdLineEdit.text +" > CmpOutput.txt", cldg, LogEdit)

call AddClrString ( "7283: run PID= "+str$(PID), clred, LogEdit)   
LogEdit.HideSelection=0
LogEdit.Font.Name="Courier"
END SUB

'!***************************************************************************************************
sub WatchListFormResize
WatchEdit.visible=0
WatchEdit.Height =WatchList.ClientHeight -23
WatchEdit.Width = WatchList.ClientWidth
WatchEdit.visible=1

end sub
'!***************************************************************************************************
sub SubListFormResize
'SubList.visible=0

'SubsListBox(WindowsIndex).visible=0

'SubsListBox(WindowsIndex).Height =SubList.ClientHeight -23
'SubsListBox(WindowsIndex).Width = SubList.ClientWidth-5
''SubList.visible=1
'SubsListBox(WindowsIndex).visible=1
'SubList.repaint

end sub

'!***************************************************************************************************
sub ObjInspectorFormResize
'DirTree.Width =DirTreeForm.ClientWidth/2
'DirTree.Height =DirTreeForm.ClientHeight -65

'if ObjectInspector.WindowState=wsNormal then 

'ObjTab.Width = ObjectInspector.ClientWidth -5
'ObjTab.Height = ObjectInspector.ClientHeight -25

'ObjTab.visible=0
'ObjTab.visible=1
'ObjectInspector.repaint
'end if

end sub

'!!!------ кнопки дебаггера ------------'
'!***************************************************************************************************
sub PauseOnClick
pauseOn=1
strSend="PauseOnClick"
'call SendData 
end sub

'!***************************************************************************************************
sub ContinueOnClick
pauseOn=0
strSend="ContinueOnClick"
'call SendData 

end sub

'!***************************************************************************************************
sub StepOverOnClick
strSend="StepOverOnClick"
'call SendData 

end sub

'!***************************************************************************************************
sub StepInToOnClick
strSend="StepInToOnClick"
'call SendData 

end sub

'!***************************************************************************************************
sub RunToCursOnClick
strSend="RunToCursOnClick"
'call SendData 
end sub

'!***************************************************************************************************
sub AddWatchOnClick '-- --------------'
dim il as long

if SrcEdit.Modified=1 then ' обновляем SubList
	call AddSubs
	Modiflg=SrcEdit.Modified
end if

WatchList.visible=1
inside=0
inside1=0
'заменяем в ВЫДЕЛЕННОМ тексте EOL
AdVar$=REPLACESUBSTR$( SrcEdit.SelText , chr$(10), " ")
AdVar$=Rtrim$(Ltrim$(REPLACESUBSTR$( AdVar$ , chr$(13), " ")))
AdVar$=AdVar$-ht '!!! ???? <<<<<<<<<<<<<<<<<------------------------------------

AddWatchMnu.Caption = "Add Watch <"+AdVar$+">"

AdVarProv$=lcase$(AdVar$)
Lskob=instr(AdVar$, "(")
Rskob=instr(AdVar$, ")")

if Lskob>0   then 'наверное массив' dF(a, b,c )
	AdVarIdx$=lcase$(mid$(AdVar$,Lskob+1, Rskob-Lskob-1))
	'!!! добавить обработку двойных и т.д индексов 
	AdVarProv$=lcase$(left$(AdVar$,Lskob-1 ))
	'call  AddClrString ("4333:AdVarProv$="+(AdVarProv$), clo, LogEdit)
	'выделяем индексы'
	'- --------------------------------
	sp2=instr(AdVarIdx$, ",")
	while sp2>0
		Idx$=rtrim$(ltrim$(left$(AdVarIdx$, sp2-1)))
		AdVarIdx$=ltrim$(rtrim$(mid$(AdVarIdx$, sp2+1,len (AdVarIdx$)-sp2+1 )))
		sp2=instr(AdVarIdx$, ",")
	wend
	'- --------------------------------
else
end if

'!!! --- определяем имя процедуры
AdSub$=""


' проверяем не является ли наша переменная глобальной, то есть не определена ли она до функции
' и если - да, то будем подсвечивать и те операторы вне функции, где переменная меняется
'!!! -- собачья чушь! Имя процедуры указывает на то, откуда эта переменная пришла в WatchList !!
'! дебагится будут только те процедуры, в которых выделялась переменная.

'!!! выделяем название sub из строки sublist  
'call  AddClrString ("SubsListBox(WindowsIndex).ItemCount-1="+str$(SubsListBox(WindowsIndex).ItemCount-1), clred, LogEdit)
SlIC=SubsListBox(WindowsIndex).ItemCount

for i=0 to SlIC-2
	if SrcEdit.WhereY+1 >=val(SubsListBox(WindowsIndex).SubItem(i, 0)) and SrcEdit.WhereY+1<val(SubsListBox(WindowsIndex).SubItem(i+1,0)) then
		Sp1=instr(SubsListBox(WindowsIndex).Item(i).Caption, "(")
		Sp2=instr(SubsListBox(WindowsIndex).Item(i).Caption, ":")
		Sp3=instr(SubsListBox(WindowsIndex).Item(i).Caption, "'")
		if Sp1>0 then
			AdSub$=Rtrim$(ltrim$(left$(SubsListBox(WindowsIndex).Item(i).Caption, sp1-1)))
		elseif  Sp2>0 then
			AdSub$=Rtrim$(ltrim$(left$(SubsListBox(WindowsIndex).Item(i).Caption, sp2-1)))
		elseif  Sp3>0 then
			AdSub$=Rtrim$(ltrim$(left$(SubsListBox(WindowsIndex).Item(i).Caption, sp3-1)))
		else
			AdSub$=Rtrim$(ltrim$(SubsListBox(WindowsIndex).Item(i).Caption))
			
		end if
		exit for
	else
	end if
next i

if SrcEdit.WhereY+1 >val(SubsListBox(WindowsIndex).SubItem(i, 0)) then
	Sp1=instr(SubsListBox(WindowsIndex).Item(i).Caption, "(")
	Sp2=instr(SubsListBox(WindowsIndex).Item(i).Caption, ":")
	Sp3=instr(SubsListBox(WindowsIndex).Item(i).Caption, "'")
	if Sp1>0 then
		AdSub$=Rtrim$(ltrim$(left$(SubsListBox(WindowsIndex).Item(i).Caption, sp1-1)))
	elseif  Sp2>0 then
		AdSub$=Rtrim$(ltrim$(left$(SubsListBox(WindowsIndex).Item(i).Caption, sp2-1)))
	elseif  Sp3>0 then
		AdSub$=Rtrim$(ltrim$(left$(SubsListBox(WindowsIndex).Item(i).Caption, sp3-1)))
	else
		AdSub$=Rtrim$(ltrim$(SubsListBox(WindowsIndex).Item(i).Caption))
	end if
else
end if


Glob: '---------------------------------------
LenSub=Len(AdSub$)
LenVar=Len(AdVar$)
AdVarName$=AdVar$
'call  AddClrString ("4403:AdVarName$="+(AdVarName$), clred, LogEdit)
AdVar$=AdSub$+"•"+AdVar$  'строка добавления в Watchedit

with WatchEdit '---- добавляем  переменные в WatchEdit -------
	for i=0 to .ItemCount-1 '- ------проверка дупа
		var$=Ltrim$(.Line(i))
		equal=instr(var$, "=")
		if   equal>0 then
			if AdVar$ =  left$(var$, equal-1) then jz=99
		else
			if AdVar$ = var$  then jz=99
		end if
	next i '- -------- end  проверка дупа '
	if jz <>99 then  
		WatchEdit.AddItems AdVar$
		' формируем строку добавления
		VAlScope(i)=AdSub$ ' имя  процедуры 
		VAlName(i)=AdVarName$ ' имя переменной
		VAlType(i)=0         ' тип переменной
	else
		jz=0
	end if
	'- сортируем
	.Sorted=1
end With'--- -------------------------
end sub

'!***************************************************************************************************
sub DelWatchOnClick
WatchEdit.DelItems WatchEdit.ItemIndex
'strSend="DelWatchOnClick"
'call SendData 
end sub

'!***************************************************************************************************
sub DelAllWatchOnClick
WatchEdit.Text=""
'strSend="DelAllWatchOnClick"
'call SendData 

end sub
'!***************************************************************************************************
sub ToggleBreakPointOnClick
'strSend="ToggleBreakPointOnClick"
'call SendData 

end sub
'!***************************************************************************************************
sub ClearAllBreakPointsOnClick
'strSend="ClearAllBreakPointsOnClick"
'call SendData 

end sub
'!***************************************************************************************************
sub  FormCloseOnClick
'strSend="FormCloseOnClick"
'call SendData 

end sub

'!***************************************************************************************************
sub CompDebugOnClick1
'--- 1. просматриваем WatchList и все функции и процедуры, что там имеются, включая функции в include файлах,  копируем
' в ~SrcD - операторы в нем мы будем подсвечивать при пошаговой отладке.
'sub CompDebugOnClick1•SubName$
WatchLine$= "" 
j=0 ' номер AddWatch j к которой обращается в данной sub'
SubNameOld$=""
dim FakeList as QStringList

~SrcD.clear
wecnt=WatchEdit.ItemCount
'call  AddClrString ("4474:wecnt="+str$(wecnt), clred, LogEdit)
with WatchEdit 
	for i1=0 to .ItemCount-1 
		WatchNames$ =lcase$(field$(.Item(i1),"•",1)):'call  AddClrString ("WatchNames$="+(WatchNames$), clred, LogEdit)
		VAlName(i1)=lcase$(field$(.Item(i1),"•",2)):'call  AddClrString ("4478-------VAlName("+str$(i1)+")="+VAlName(i1), clDPurple, LogEdit)
		SubName1$=field$(.Item(i1),"•",1)
		'call  AddClrString ("4483:.Item("+str$(i1)+")="+(.Item(i1)), clgr, LogEdit)
		'call  AddClrString ("4484:SubName1$="+(SubName1$), clred, LogEdit)
		isub=0
		SubName$=field$(SubName1$," ",2) '4484:SubName1$=sub  changesel
		'call  AddClrString ("4487:SubName$="+(SubName$)+"=", clred, LogEdit)
		while SubName$=" " or SubName$=""
			inc isub
			SubName$=field$(SubName1$," ",2+isub)
			'call  AddClrString ("4491:SubName$="+(SubName$), clb, LogEdit)
		wend
		
		'call  AddClrString ("4494:SubName$="+(SubName$), clred, LogEdit)
		SubVAlName(i1)=SubName$+"."+VAlName(i1)
		'call  AddClrString ("SubName$="+(SubName$), clred, LogEdit)
		'call  AddClrString ("SubNameOld$="+(SubNameOld$), clred, LogEdit)
		
		' ищем в SubList начальную и конечную строки процедуры WatchNames$=sub  changesel
		
		if SubName$<> SubNameOld$ then 
			inc j: SubNameOld$=SubName$:dob=0 
		else 
			goto nexti1
		end if
		'перебираем по процедурам
		SubsCnt=SubsListBox(WindowsIndex).ItemCount
		for i=0 to SubsCnt -1
			if GetWord (lcase$(SubsListBox(WindowsIndex).Item(I).Caption), lcase$(SubName$))>0 then ' есть такая функция
				if i< SubsCnt -1 then
					ibw=val(SubsListBox(WindowsIndex).SubItem(I, 0))-1'+63 начальная позиция функции
					'call  AddClrString ("ibw="+str$(ibw), clred, LogEdit)
					iew=val(SubsListBox(WindowsIndex).SubItem(I+1, 0))'-1'+63 конечная позиция функции
					'call  AddClrString ("iew="+str$(iew), clred, LogEdit)
					exit for
				else
					ibw=val(SubsListBox(WindowsIndex).SubItem(I, 0))-1'+63 начальная позиция функции
					iew=(SrcEdit.linecount-1)
				end if
			end if
		next i
		' для последней функции
		
		'!!! -- формируем файл с исследуемой процедурой
		
		$ifdef 0 '----------------------------
		'for i=0 to SrcList.ItemCount-1
		'if ins
		'SrcList.Item(i)=rtrim$(SrcList.Item(i))
		'next i
		'SrcList.text=replacesubStr$(SrcList.text,"_"+crlf,"")
		'SrcTxt$=SrcList.text
		'SrcTxt$=replacesubStr$(SrcTxt$,"_"+crlf,"")
		'SrcTxt$=replacesubStr$(SrcTxt$,crlf," :")
		$endif '--------------------------------
		srcl$="---"
		srcl11$="==="
		srcl22$="+++"
		for i=ibw to iew
			srcl$=SrcEdit.Line(i)-ht
			srcl11$=srcl$
			ReplaceComments (@srcl11$)
			srcl22$=rtrim$(srcl11$)
			ss$=right$(SrcL22$,1)
			if ss$="_" then ~SrcD.AddStrings( srcl22$) else ~SrcD.AddStrings  (srcl$)
			'~SrcD.AddStrings  srcl$' переписываем в исходник подсветки тело процедуры
		next i
		~SrcD.text=replacesubStr$(~SrcD.text,"_"+crlf,"") ' убираем подчеркивание
		
		nexti1:
	next i1 
end with 
' сформировали ~SrcD с функциями, которые будут дебагиться и подсвечиваться

' формируем дебаг-функцию вида 'DebugFunc$="RQDebug("x", x,"y", y,"z", z,    1)"
vl$=""

for i2=0 to WatchEdit.ItemCount-1 ' в каждой строчке - своя переменная
	vl$=vl$+qt+"~$#"+SubVAlName(i2)+qt+","+VAlName(i2)+"," '~DbgValName
	'call  AddClrString ("4523 vl$="+(vl$), clgr, LogEdit)
next i2
vl$="RQDebug1("+vl$
'vl$=DebugProcName$+"("+vl$
'call  AddClrString ("4695:DebugProcName$="+(DebugProcName$), clred, LogEdit)

'call  AddClrString ("4527 vl$="+(vl$), clDBlue, LogEdit)

' теперь заполняем масcив начальных и конечных позиций операторов для подсветки при перемещении по шагам.
'-------- operators positions ----------------------
~SrcDLineCount=~SrcD.LineCount ' число строк

'DobOperCnt=tally(~SrcD.text,":") ' число разделителей операторов
DobOperCnt=2*GetCharNum(~SrcD.text,":") ' число разделителей операторов

AllOper=~SrcDLineCount+DobOperCnt
'call  AddClrString ("3558 AllOper="+str$(AllOper), cblue, LogEdit)

reDim ~Op(AllOper) as string
redim ~OpPos(AllOper) as integer
redim ~OpLen(AllOper) as integer

' позиции операторов для подсветки !!! ------------------------------
call GetOperators (~Op() ,~OpPos(), ~SrcD.text, @opercnt)
'call  AddClrString ("~SrcD.text="+(~SrcD.text), clred, LogEdit)
'call  AddClrString (" -- 3546 operators count="+str$(opercnt), clgreen, LogEdit)

for i=0 to opercnt-1
	~OpLen(i)=len(~Op(i))
	'~OpPos(i)
	'call  AddClrString ("~OpPos("+str$(i)+")="+str$(~OpPos(i)), clblue, LogEdit)
	'~Op(i)
	'call  AddClrString ("Оператор ~Op("+str$(i)+")="+(~Op(i)), cldb, LogEdit)
next i

SrcFilePath$=lcase$(StripPAth (SrcFileName))
'call  AddClrString ("4557:SrcFilePath$="+(SrcFilePath$), clred, LogEdit)

~SrcD.SaveToFile(SrcFilePath$+"$SrcDisplay.bas")
'$~OpPos.dat $~OpLen.dat
DIM File AS QFileStream 
File.Open(SrcFilePath$+"$~OpPos.dat", fmCreate) 
File.SaveArray(~OpPos(1), opercnt) 
File.Close

File.Open(SrcFilePath$+"$~OpLen.dat", fmCreate) 
File.SaveArray(~OpLen(1), opercnt) 
File.Close


'--- 2. Формируем полный DbgSrcEdit - исходник с добавленными функциями DebugWindow - просмотр переменных и пошаговый проход.
if DebugSrcForm.Visible=0 then DebugSrcForm.Visible=1'
'копируем исходник'

'2 '! --- вставляем процедуру дебаггера ----------'
'вставляем в начало'
DebugSrcEdit.Text="$include "+qt+StartDir+"debugWindow35.bas"+qt +crlf '+DebugSrcEdit.Text
'call  AddClrString ("4578:DebugSrcEdit.Text="+(DebugSrcEdit.Text), cldred, LogEdit)

FakeList.text=SrcEdit.Text
fllen=len(FakeList.text)
'call  AddClrString ("4582:fllen="+str$(fllen), clred, LogEdit)
for i=0 to FakeList.ItemCount-1
	FakeList.Item(i)=FakeList.Item(i)-ht
next i
fllen=len(FakeList.text)
'call  AddClrString ("4582:fllen="+str$(fllen), clred, LogEdit)

DebugSrcEdit.Text=DebugSrcEdit.Text+FakeList.text
'htnum1=instr(DebugSrcEdit.Text,ht)
'call  AddClrString ("4580:htnum1="+str$(htnum1), 0, LogEdit)

'--------- DebugSrcEdit.Text добавляем дебаг-функции перед нужными операторами
' ищем функцию, которая дебагится

' снова перебираем WatchList и находим имена функций
SubNameOld$=""
dr1=1
for i1=0 to WatchEdit.ItemCount-1 
	WatchNames$ =lcase$(field$(WatchEdit.Item(i1),"•",1))
	'call  AddClrString ("2 WatchNames$="+(WatchNames$), cldred, LogEdit)
	SubName1$=field$(WatchEdit.Item(i1),"•",1)
	SubName$=field$(SubName1$," ",2)
	isub=0
	while SubName$=" " or SubName$=""
		inc isub
		SubName$=field$(SubName1$," ",2+isub)
		'call  AddClrString ("4491:SubName$="+(SubName$), clb, LogEdit)
	wend
	
	'call  AddClrString ("4623:SubName$="+(SubName$), clo, LogEdit)
	'call  AddClrString ("4627:SubNameOld$ ="+(SubNameOld$ ), clred, LogEdit)
	
	if SubName$<> SubNameOld$ then  SubNameOld$=SubName$:dob=0 else goto nexti3683
	' нашли имя процедуры
	' теперь ищем позиции начала и конца этой процедуры в DebugSrcEdit.Text, чтобы добавить дебаг-функцию
	'call  AddClrString ("3631 SubName$="+(SubName$), clblue, LogEdit)
	for i=0 to DebugSrcEdit.LineCount-1
		lDbgline$=lcase$(DebugSrcEdit.Line(i))
		lSubName$=lcase$(SubName$)
		if GetWord (lDbgline$,lSubName$ )>0 and (GetWord(lDbgline$,"sub")>0 or GetWord(lDbgline$,"function")>0) and GetWord(lDbgline$,"declare")=0 then ' есть такая функция
			'call  AddClrString ("DebugSrcEdit.Line("+str$(i)+")="+(DebugSrcEdit.Line(i)), clDGreen, LogEdit)
			SPosB=i+1
			'call  AddClrString ("4636:SPosB="+str$(SPosB), cldgrey, LogEdit)
			for j=i to DebugSrcEdit.LineCount-1 '- ищем end sub'
				'call  AddClrString ("4637:j="+str$(j), clp, LogEdit)
				lDbgline1$=lcase$(DebugSrcEdit.Line(j))-" "
				'call  AddClrString ("4638:lDbgline1$="+(lDbgline1$), clred, LogEdit)
				if instr( lDbgline1$, "end")>0 then
					'call  AddClrString ("4639:lDbgline1$="+(lDbgline1$), cldred, LogEdit)
					if getword(lDbgline1$,"endsub")>0  or getword(lDbgline1$,"endfunction")>0  then
						SPosE=j+1
						'call  AddClrString ("4645:SPosE="+str$(SPosE), cldgrey, LogEdit)
						'goto getoperatorslbl:
						exit for
					end if
				end if
			next j 
		else
		end if
		doevents
	next i
	
	' нашли начальную и конечную строки процедуры, теперь внутри ее найдем операторы и вставим перед ними дебаг-функцию
	getoperatorslbl:
	opercnt=0:
	
	for i=SPosB to SPosE 'найдем операторы и вставим перед ними дебаг-функцию
		dr=0
		DSrcLine$=DebugSrcEdit.Line(i)-ht
		DelComments(@DSrcLine$)
		DebugSrcEditLine$=trim$(DSrcLine$) ' запихиваем строку в переменную убрав HotTabs
		if DebugSrcEditLine$="" then goto nexti3682:
		ldb=len(DebugSrcEditLine$)
		if ldb>0 then
			if right$(DebugSrcEditLine$,1)="_" then '- обработка подчеркивания
				dobstr4=0
				while right$(DebugSrcEditLine$,1)="_"
					DebugSrcEditLine$=left$(DebugSrcEditLine$,len(DebugSrcEditLine$)-1) ' отнимаем подчеркивание
					inc dobstr4
					DSrcLine$=DebugSrcEdit.Line(i+dobstr4)-ht ' отняли ht у следующей строки
					DelComments(@DSrcLine$) ' удалили комментарии 
					DSrcLine$=trim$(DSrcLine$) ' обрезали пробелы
					DebugSrcEditLine$=DebugSrcEditLine$+DSrcLine$ 'и добавили к  текущей строке
				wend
				DebugSrcEdit.Line(i)=DebugSrcEditLine$ ' получили новую текущую строку
				for id=1 to dobstr4
					DebugSrcEdit.Line(i+id)="" ' следующие строки делаем пустыми
				next id
			end if
		end if
		if trim$(DebugSrcEditLine$)="" then goto nexti3682 ' если пустая строка
		GetOperators (~Op() ,~OpPos() , DebugSrcEditLine$,@opcnt)
		LenDbgF=0
		if opcnt>0 then ' если количество операторов больше 0
			if dobstr4=dobstr4 then '0
				for j1=0 to opcnt-1 
					posop$=~Op(j1) 
					OperPoz=~OpPos(j1) 
					OperLenth=len(posop$)
					' вставляем перед оператором дебаг-функцию
					DebugFunc$=vl$+ str$(dr1)+"):"'+qt
					DebugSrcEdit.Line(i) = INSERT$(DebugFunc$, DebugSrcEdit.Line(i), OperPoz+LenDbgF)'
					'call  AddClrString ("4649:DebugSrcEdit.Line("+str$(i)+")="+(DebugSrcEdit.Line(i)), clm, LogEdit)
					LenDbgF=LenDbgF+len(DebugFunc$)
					inc dr
					inc dr1
					nextj3671:
				next j1
				i=i+dobstr4
				dobstr4=0
			else ' есть подчеркивание
				for j1=0 to opcnt-1 
					posop$=DebugSrcEditLine$'DebugSrcEdit.Line(i)
					OperLenth=len(posop$)
					DebugFunc$=vl$+ str$(dr1)+"):"
					DebugSrcEdit.Line(i-dobstr4) = INSERT$(DebugFunc$, DebugSrcEdit.Line(i-dobstr4), 1)
					dobstr4=0
					inc dr
					inc dr1
				next j1
			end if
		else
		end if
		nexti3682:
	next i 
	nexti3683:
next i1 

opercnt=0:
DSLi$=""

' теперь коррекция переменных, которые не определены на момент выполнения дебаг-функции.
'call  AddClrString ("delete already defined vars..--------------------", cldeeppurple, LogEdit)

''call  AddClrString ("4753:SPosB="+str$(SPosB), clred, LogEdit)
'call  AddClrString ("4753:SPosE="+str$(SPosE), clred, LogEdit)

for i=SPosB to SPosE 'найдем операторы и найдем среди них dim def.. операторы 
	dr=0
	DSLi1$=DebugSrcEdit.Line(i)-ht
	'call  AddClrString ("4758:DSLi1$="+(DSLi1$), clb, LogEdit)
	DelComments (@DSLi1$)
	if DSLi1$<>"" then DSLi$=DSLi$+DSLi1$+":" ' запихиваем строку в переменную
	'call  AddClrString ("4761:DSLi$="+(DSLi$), clred, LogEdit)
next i 


OperCnt=GetCharNum(DSLi$,":") ' число разделителей операторов
'call  AddClrString ("4762:DSLi$="+(DSLi$), clm, LogEdit)
'call  AddClrString ("4710:------ Opers Count="+str$(OperCnt), cldg, LogEdit)


redim ~OpPos(OperCnt-1) as integer
redim ~OpLen(OperCnt-1) as integer

call GetOperators (~Op() ,~OpPos(), DSLi$, @opercnt)

for OpI=0 to opercnt-1
	'call  AddClrString ("4762:opercnt="+str$(opercnt), clred, LogEdit)
	'call  AddClrString ("4762:OpI="+str$(OpI), cldb, LogEdit)
	' проверяем есть ли в операторе определение переменной
	'call  AddClrString ("4765:~Op("+str$(Opi)+")="+(~Op(Opi)), clb, LogEdit)
	DSLi$=ucase$(~Op(Opi))
	'call  AddClrString ("4688:DSLi$="+(DSLi$), cldr, LogEdit)
	if instr(DSLi$,"DEF")>0 or instr(DSLi$,"DIM ")>0 then ' предварительно для скорости
		if GetVarPos(DSLi$,"DIM")    >0 then
			' определяем массивы
			Asn =instr(DSLi$," AS ")
			call ParseDim (VarN$(), VarType$() , DSLi$, @VarCount )
			'call  AddClrString ("4694:VarCount="+str$(VarCount), clbr, LogEdit)
			
			' перебираем по переменным
			for vc=0 to VarCount-1
				'перебираем по AddWatch переменных, проверяем нет ли в них этих переменных
				for i1=0 to WatchEdit.ItemCount-1 
					'call  AddClrString ("4699:VAlName("+str$(i1)+")="+(VAlName(i1)), cldp, LogEdit)
					'call  AddClrString ("4699:lcase$(VarN$("+str$(vc)+")="+(lcase$(VarN$(vc))), cldp, LogEdit)
					lVar$=field$(lcase$(VarN$(vc)),"(",1 )
					lval$=field$(VAlName(i1),"(",1 )
					
					'if lcase$(VarN$(vc))= VAlName(i1) then 
					if lVar$= lval$ then 
						' если наблюдаемая переменная определяется позже-корректируем дебаг-функции до момента определения
						for iij=0 to OpI step 2
							if VarType$(vc)="STRING" then
								NewDbgF$=replaceSubStr$(~Op(iij),","+VAlName(i1)+",",","+"~_S$"+",")
							else
								NewDbgF$=replaceSubStr$(~Op(iij),","+VAlName(i1)+",",","+"~_D"+",")
							end if
							'call  AddClrString ("4709:NewDbgF$="+(NewDbgF$), clo, LogEdit)
							idxnmb=instr(DebugSrcEdit.text,~Op(iij) )
							DebugSrcEdit.text=delete$( DebugSrcEdit.text,idxnmb ,len(~Op(iij) ) )
							DebugSrcEdit.text=insert$(NewDbgF$,DebugSrcEdit.text,idxnmb )
							~Op(iij)=NewDbgF$
						next iij
						
					end if
				next i1
			next vc
			
			elseif GetVarPos(DSLi$,"DEFBYTE") >0 or _
			GetVarPos(DSLi$,"DEFDBL")  >0 or _
			GetVarPos(DSLi$,"DEFDWORD")>0 or _
			GetVarPos(DSLi$,"DEFINT")  >0 or _
			GetVarPos(DSLi$,"DEFLNG")  >0 or _
			GetVarPos(DSLi$,"DEFSHORT")>0 or _
			GetVarPos(DSLi$,"DEFSNG")  >0 or _
			GetVarPos(DSLi$,"DEFSTR")  >0 or _
		GetVarPos(DSLi$,"DEFWORD") >0  then
			' корректируем дебаг функции
			call ParseDef (VarN$() ,VType$ , DSLi$ , @VarCount )
			' перебираем по переменным
			for vc=0 to VarCount-1
				'перебираем по AddWatch переменных, проверяем нет ли в них этих переменных
				for i1=0 to WatchEdit.ItemCount-1 
					'call  AddClrString ("4734: def VAlName("+str$(i1)+")="+(VAlName(i1)), clp, LogEdit)
					'call  AddClrString ("4735: def lcase$(VarN$("+str$(vc)+")="+(lcase$(VarN$(vc))), clp, LogEdit)
					if lcase$(VarN$(vc))= VAlName(i1) then 
						' если наблюдаемая переменная определяется позже-корректируем дебаг-функции до момента определения
						for iij=0 to OpI step 2
							if VarType$(vc)="STRING" then
								NewDbgF$=replaceSubStr$(~Op(iij),","+VAlName(i1)+",",","+"~_S$"+",")
							else
								NewDbgF$=replaceSubStr$(~Op(iij),","+VAlName(i1)+",",","+"~_D"+",")
							end if
							idxnmb=instr(DebugSrcEdit.text,~Op(iij) )
							DebugSrcEdit.text=delete$( DebugSrcEdit.text,idxnmb ,len(~Op(iij) ) )
							DebugSrcEdit.text=insert$(NewDbgF$,DebugSrcEdit.text,idxnmb )
							~Op(iij)=NewDbgF$
						next iij
						
					end if
				next i1
			next vc
			
		else
		end if
	end if
next OpI

SrcFileNameOrig$=SrcFileName
'call  AddClrString ("4779:SrcFileNameOrig$="+(SrcFileNameOrig$), cldA, LogEdit)
'SrcFilePath$=lcase$(StripPAth (SrcFileName))
'call  AddClrString ("4781:SrcFilePath$="+(SrcFilePath$), clgrey, LogEdit)

SrcFileName=SrcFilePath$+"$DbgDisplay.bas"
'call  AddClrString ("4690: SrcFileName="+(SrcFileName), clred, LogEdit)
DebugSrcEdit.SaveToFile(SrcFileName)

dbgflg=1
RunItOnClick
SrcFileName=SrcFileNameOrig$
reDim ~Op(1) as string
redim ~OpPos(1) as integer
redim ~OpLen(1) as integer

end sub


'!***************************************************************************************************
SUB FormWndProc (Hwnd&, uMsg&, wParam&, lParam&)
'IF uMsg& = WM_COPYDATA THEN
'inc NumMsg  
'DIM MStream AS QMEMORYSTREAM
'MStream.MemCopyFrom(lParam&, 12)
'MStream.Position = 0
'MStream.ReadUDT(DataStruct)
'MStream.Close
'strRecv = VARPTR$(DataStruct.lpData)
'LogEdit.Line(0)=str$(NumMsg )+" "+ strRecv
'watchEdit.Text=strRecv
'doevents
'END IF
END SUB

'!***************************************************************************************************
SUB SendData 

'DEFLNG wndhnd
'wndhnd=Findwindow("TForm",AppName)
'IF wndhnd=0 THEN
'SHOWMESSAGE "Could not find "+AppName
'EXIT SUB
'END IF
'DataStruct.lpData = VARPTR(strSend)
'DataStruct.cbData = LEN(strSend)
'SENDMESSAGE wndhnd, WM_COPYDATA, 0, DataStruct
'LogEdit.AddStrings strSend
END SUB

'!***************************************************************************************************
SUB FrmMinimized (Sender as QForm)
Sender.WindowState=1

END SUB

'!***************************************************************************************************
sub PropertTabChange
SELECT CASE PropertTab.TabIndex
CASE 0
	PropPanelEditor.visible=true
	PropPanel1.visible=false
	PropPanel2.visible=false
	PropPanel3.visible=false
	PropPanel4.visible=false
	PropPanel5.visible=false
	PropPanel6.visible=false
	PropPanel7.visible=false
CASE 1
	PropPanelEditor.visible=false
	PropPanel1.visible=true
	PropPanel2.visible=false
	PropPanel3.visible=false
	PropPanel4.visible=false
	PropPanel5.visible=false
	PropPanel6.visible=false
	PropPanel7.visible=false
CASE 2
	PropPanelEditor.visible=false
	PropPanel1.visible=false
	PropPanel2.visible=true
	PropPanel3.visible=false
	PropPanel4.visible=false
	PropPanel5.visible=false
	PropPanel6.visible=false
	PropPanel7.visible=false
CASE 3
	PropPanelEditor.visible=false
	PropPanel1.visible=false
	PropPanel2.visible=false
	PropPanel3.visible=true 
	PropPanel4.visible=false
	PropPanel5.visible=false
	PropPanel6.visible=false
	PropPanel7.visible=false
CASE 4
	PropPanelEditor.visible=false
	PropPanel1.visible=false
	PropPanel2.visible=false
	PropPanel3.visible=false
	PropPanel4.visible=true
	PropPanel5.visible=false
	PropPanel6.visible=false
	PropPanel7.visible=false
CASE 5
	PropPanelEditor.visible=false
	PropPanel1.visible=false
	PropPanel2.visible=false
	PropPanel3.visible=false
	PropPanel4.visible=false
	PropPanel5.visible=true 
	PropPanel6.visible=false
	PropPanel7.visible=false
CASE 6
	PropPanelEditor.visible=false
	PropPanel1.visible=false
	PropPanel2.visible=false
	PropPanel3.visible=false
	PropPanel4.visible=false
	PropPanel5.visible=false
	PropPanel6.visible=   true
	PropPanel7.visible=false
CASE 7
	PropPanelEditor.visible=false
	PropPanel1.visible=false
	PropPanel2.visible=false
	PropPanel3.visible=false
	PropPanel4.visible=false
	PropPanel5.visible=false 
	PropPanel6.visible=false   
	PropPanel7.visible=true
	
end select


END SUB

'!***************************************************************************************************
sub SearchItOnClick
FindText (FindBtnDn)
END SUB

'!***************************************************************************************************
sub PopUpMe '-- -------------------------'

EditHandle = GetFocus()

select case EditHandle
case SrcEdit.Handle
	EditHandle$="sub PopUpMe '-- -4977:SrcEdit focused========"
	AddInText=SrcEdit.SelText
case DebugSrcEdit.Handle
	EditHandle$="sub PopUpMe '-- -4977:DebugSrcEdit focused========"
	AddInText=DebugSrcEdit.SelText
case LogEdit.Handle
	EditHandle$="sub PopUpMe '-- -4977:LogEdit focused========"
	AddInText=LogEdit.SelText
case else
end select
'call  AddClrString ("4987:EditHandle$="+(EditHandle$), &H915EC7, LogEdit)

if  len(AddInText)>20 then 
	
	
	
	AddInTextI$= Left$(AddInText, 20)+"..."
else
	AddInTextI$= AddInText
end if


Select case codp$
case "win"
case "dos"
	oemtochar AddInTextI$,AddInTextI$
	'oemtochar SearchStr$,SearchStr$
case "koi"
case else
end select


AddWatchMnu.Caption = "AddWatch <"+AddInTextI$+">"

EditMenuItemPop(0).Caption = "To UpperCase {"+AddInTextI$+"} "
EditMenuItemPop(1).Caption = "To LowerCase {"+AddInTextI$+"} "
EditMenuItemPop(3).Caption = "Search {"+AddInTextI$+"} "

EditMenuItemPop(4).Caption = "To UpperCase {"+AddInTextI$+"} "
EditMenuItemPop(5).Caption = "To LowerCase {"+AddInTextI$+"} "

AddInTextWin$=AddInTextI$+" "
chartooem (AddInTextI$,AddInTextWin$)

EditMenuItemPop(15).Caption = "Win->DOS {"+AddInTextI$+"} "

EditMenuItemPop(7).Caption = "Search {"+AddInTextI$+"} "
EditMenuItemPop(10).Caption="Add Sub {"+AddInTextI$+"} "
EditMenuItemPop(13).Caption = "Open file{"+AddInTextI$+"} "


LogMenuItemPop(1).Caption = "Search {"+AddInTextI$+"} "
LogMenuItemPop(2).Caption = "Open File {"+AddInTextI$+"} "

AddInTextDos$=AddInTextI$+" "
oemtochar (AddInTextI$,AddInTextDos$)
EditMenuItemPop(16).Caption = "DOS->Win {"+AddInTextDos$+"} "

end sub
'-- ---------------------------------------'
'!***************************************************************************************************
sub AddSubs
print"8313 AddSubs==============="
'call  AddClrString ("4910:AddSubs", clred, LogEdit)

'exit sub

DIM ArrBeg(0 to 1023) AS integer:
DIM ArrEnd(0 to 1023) AS integer:


dim FakeList as QStringList
FakeList.Text=SrcEdit.Text
'Windows.enabled=0 !
'ProjectsMnu.enabled=0!
'BMarkMain.enabled=0 !

Gauge1.visible=1
Gauge1.position=15

SubsListBox(WindowsIndex).Clear
Index=0
Modiflg=0
'call  AddClrString ("4818:timer="+str$(timer), clred, LogEdit)
'tt1=timer
oldi=0

'SubsListBox(WindowsIndex).Visible=0
'ObjTreePanel.visible=0
'call  AddClrString ("8228:FakeList.ItemCount="+str$(FakeList.ItemCount), clred, LogEdit)

for i=0 to FakeList.ItemCount-1
	LSrcEditLine$=Ltrim$(LCase$(FakeList.Item(i)))-ht
	
	
	
	if Left$(LSrcEditLine$, 4)="sub " or Left$(LSrcEditLine$, 9)="function " or Left$(LSrcEditLine$, 10)="procedure "  then ' or Left$(LSrcEditLine$, 7)="create " 
		SubsListBox(WindowsIndex).AddItems LSrcEditLine$ 'trim$( SrcEdit.Line(i))
		SubsListBox(WindowsIndex).AddSubItem Index,  str$(i+1)
		inc Index
	end if
	
	
	if i>oldi+150 then 
		oldi=i
		Gauge1.position=i/(FakeList.ItemCount+1)*100
	end if
next i
'tt2=timer
tt3=tt2-tt1


regex$="(\bvoid\b|\bint\b|\bbool\b|\bboolean\b|\bchar\b|\bint8_t\b|\bbyte\b|\buint8_t\b|\bshort\b|\bint16_t\b|\buint16_t\b|\blong\b|\bint32_t\b|\buint32_t\b|\bfloat\b|\bdouble\b|\bString\b|\bint64_t\b|\buint64_t\b)\s+\**\w+\:*\w+\s*\("
SrcText$=SrcEdit.Text
print"8365 SrcText$="

if SrcText$="" then exit sub

Index=0

matchCnt=GetMatches( regex$,  SrcText$,  ArrBeg(0),  ArrEnd(0) )
call  AddClrString ("8397:matchCnt="+str$(matchCnt), clred, LogEdit)
if matchCnt<1 then goto nomatch

'AddClrString ("8346:matchCnt="+str$(matchCnt), clred, LogEdit)
abeg=0'ArrBeg(0)
'print"abeg=";abeg

for i=0 to matchCnt-1
	'call  AddClrString ("256:abeg="+str$(abeg), clred, LogEdit)
	'call  AddClrString ("248:ArrBeg("+str$(i)+")="+str$(ArrBeg(i)+abeg), clb, LogEdit)
	'call  AddClrString ("248:ArrEnd("+str$(i)+")="+str$(ArrEnd(i)+abeg), clred, LogEdit)
	'resultEdit.seltext=""
	'lineN=SrcEdit.LINEFROMCHAR(ArrBeg(i)+abeg)
	lineN=SendMessageA (SrcEdit.handle,EM_LINEFROMCHAR,ArrBeg(i)+abeg,0)
	
	'AddClrString ("8357:lineN="+str$(lineN), clred, LogEdit)
	LSrcEditLine$=Ltrim$(LCase$(SrcEdit.Line(lineN)))-ht
	'call  AddClrString ("8359:LSrcEditLine$="+(LSrcEditLine$), clred, LogEdit)
	SubsListBox(WindowsIndex).AddItems LSrcEditLine$ 'trim$( SrcEdit.Line(i))
	SubsListBox(WindowsIndex).AddSubItem Index,  str$(lineN+1)
	inc Index
	
	sSelStart=ArrBeg(i)+abeg
	
	sSelLength=ArrEnd(i)-ArrBeg(i)
	abeg=abeg+ArrEnd(i)
	
next i
'call  AddClrString ("8369:i="+str$(i), clred, LogEdit)

' SubsListBox(WindowsIndex).Visible=1

nomatch:
ObjTreePanel.visible=1


Windows.enabled=1
ProjectsMnu.enabled=1
BMarkMain.enabled=1
Gauge1.visible=0
print" 8408 Gauge1.visible=";Gauge1.visible
'call  AddClrString ("8378:Gauge1.visible="+str$(Gauge1.visible), clred, LogEdit)
'sublist.show

end  sub
'!***************************************************************************************************
sub GoSubs

NOPAINT=1
if SubsListBox(WindowsIndex).ItemIndex=-1 then SubsListBox(WindowsIndex).ItemIndex=SubsListBox(WindowsIndex).ItemCount-1
LinePos=val(SubsListBox(WindowsIndex).SubItem(SubsListBox(WindowsIndex).ItemIndex, 0))-1
call  AddClrString ("4783:LinePos="+str$(LinePos), clred, LogEdit)

LChar=SendMessageAPI (SrcEdit.handle,EM_LINEINDEX,SrcEdit.LineCount-2,0) 
SrcEdit.SelStart=lChar

FChar=SendMessageAPI (SrcEdit.handle,EM_LINEINDEX,LinePos,0) 
call  AddClrString ("8393:FChar="+str$(FChar), clred, LogEdit)
NOPAINT=0
SrcEdit.SelStart=FChar
call  AddClrString ("8406:SrcEdit.SelStart="+str$(SrcEdit.SelStart), clred, LogEdit)
SrcEdit.SelLength=1

if SrcEdit.Modified=1 then 
	'IF Modiflg=1 THEN
	call AddSubs
	Modiflg=0
	'exit sub 
end if

end sub

'!***************************************************************************************************
sub ChangeFontBtnClick 

FontDialog.GetFont(HiLiteFont)

IF FontDialog.Execute THEN
	''    ShowMessage "You selected "+FontDialog.Name
	'FontDialog.SetFont(Font)
	'FontLbl.Font.Name=FontDialog.Name
	'FontLbl.Font.Size=FontDialog.Size
	FontDialog.SetFont(StrFont)
	FontDialog.SetFont(HiLiteFont)
	
	nopaint=1
	'SrcEdit.visible=0
	MarginEdit.HideSelection=1
	SrcEdit.HideSelection=1
	'call  AddClrString ("HiLiteFont.Name= "+HiLiteFont.Name, clred, LogEdit)   
	CurPos=SrcEdit.SelStart
	SrcEdit.Font=StrFont
	SrcEdit.SelectAll
	SrcEdit.SelAttributes=StrFont
	SrcEdit.SelLength=0
	SrcEdit.SelStart=CurPos
	
	MarginEdit.Font=StrFont
	MarginEdit.SelectAll
	MarginEdit.SelAttributes=StrFont
	MarginEdit.SelLength=0
	'MarginEdit.visible=1
	'SrcEdit.visible=1
	nopaint=0
	SrcEdit.HideSelection=0
	MarginEdit.HideSelection=0
	
	'SrcEdit.HiLight
	FontLbl.Font=StrFont
	'SrcEdit.Font.Name=FontDialog.Name
	'SrcEdit.Font.Size=FontDialog.Size
	SrcEdit.RichFont.name=StrFont.name
END IF
end sub



'!***************************************************************************************************
FUNCTION  RichEditWndProc1 (hWnd AS LONG, uMsg AS LONG, wParam AS LONG, lParam AS LONG) AS LONG
Result =  CallWindowProc(OldRichEditWndProc, hWnd, uMsg, wParam, lParam)
end function




'--- New  WinProc RichEdit ---------------
FUNCTION  RichEditWndProc (hWnd AS LONG, uMsg AS LONG, wParam AS LONG, lParam AS LONG) AS LONG

'if NoPaint=2 then exit function
'SubCount++

if NoPaint=1 and  uMsg=WM_PAINT then exit function
Result =  CallWindowProc(OldRichEditWndProc, hWnd, uMsg, wParam, lParam)

SELECT CASE uMsg
case WM_MOUSEWHEEL
	
	'goto noMOUSEWHEEL1
	
	if wParam> 0 then
		result= SendMessageAPI (hWnd,EM_LINESCROLL,0,-2)
		'result= SendMessageAPI (MarginEdit.handle,EM_LINESCROLL,0,-5)
		
		
		'MarginEdit.text= LineNumList.text
		
	else
		result= SendMessageAPI (hWnd,EM_LINESCROLL,0,2)
		'result= SendMessageAPI (MarginEdit.handle,EM_LINESCROLL,0,5)
		
		'MarginEdit.text= LineNumList.text
		
	end if
	noMOUSEWHEEL1:
	
CASE  WM_PAINT '---- <--------------------
	FIRSTVISIBLELINE= SendMessageAPI (hWnd,EM_GETFIRSTVISIBLELINE,0,0)
	'call  AddClrString ("6237:FIRSTVISIBLELINE="+str$(FIRSTVISIBLELINE), clred, LogEdit)
	'FirstChar=SendMessageAPI (hWnd,EM_LINEINDEX,line_number,0) 
	
	
	if SrcEdit.LineCount<>SrcEditLineCount then
		SrcEditLineCount=SrcEdit.LineCount
		LineNumList.clear
		for i=0 to SrcEdit.LineCount+50' -1
			LineNumList.AddItems  str$(i+1)
		next i
		MarginEdit.text= LineNumList.text
		MarginEdit.visible=1
		lbzzz=1
	end if
	
	
	if  OldFirstVisLine <> FIRSTVISIBLELINE or lbzzz=1 then
		zzlen=len(str$(FIRSTVISIBLELINE+50))
		'call  AddClrString ("7909:zzlen="+str$(zzlen), clred, LogEdit)
		MarginEdit.Width=MarginEdit.Font.Size*(zzlen+1)
		'call  AddClrString ("7910:MarginEdit.Width="+str$(MarginEdit.Width), clred, LogEdit)
		
		MarginChar=SendMessageAPI (MarginEdit.handle,EM_LINEINDEX,FIRSTVISIBLELINE,0) 
		'call  AddClrString ("6264:MarginChar="+str$(MarginChar), clred, LogEdit)
		MarginEdit.SelStart=len(MarginEdit.Text)-4
		'call  AddClrString ("6256:len(MarginEdit.Text)-4)="+(len(MarginEdit.Text)-4), clred, LogEdit)
		MarginEdit.SelStart=MarginChar
		OldFirstVisLine = FIRSTVISIBLELINE
		lbzzz=0
	else
	end if
	
	
	if SyntaxHLBox.checked=0 then exit function
	'hWnd
	HiLight (SrcEdit.handle,HiLiteFont.handle,@HLColor(0),strptr,HotTAbHL)'-<<<<<<<<<<<<<<<<<<<<<<<<'
	'HiLight (SrcEdit.handle,HiLiteFont.handle,strptr,HotTAbHL)'-<<<<<<<<<<<<<<<<<<<<<<<<'
	'HiLight (hWnd,HiLiteFont.handle,strptr,HtHLBox.checked)'-<<<<<<<<<<<<<<<<<<<<<<<<'
	
end select

END FUNCTION

'!****************************************************************************
function ParseString (StrLine$ as string, VAlMPos as long) as string'= выделение  переменной под курсором
DIM ch      AS STRING * 1
DIM token   AS STRING
'----------    
dim CurPos  AS LONG  '-- char pos  from file beginning  
dim StrPos  AS LONG  '-- firs char in current line  position   (from file beginning)   
dim VAlBPos  AS LONG  '-- найденная позиция начала  переменной
dim VAlEPos  AS LONG  '-- найденная первая последнего символа переменной
'dim VAlMPos  AS LONG  '-- исходная позиция откуда ищем 
pStrLine$="qqqqqq"
pStrLine$=StrLine$

defstr pVal$=""  '-- переменная
defstr delimiter$="()[]+-=\/*^&<>: ',"+cr+lf+ht
with SrcEdit
	'search begin of value
	for i= VAlMPos to 1 step -1
		VAlBPos=instr(delimiter$, pStrLine$[i])
		'call  AddClrString (".line(WhereY)[i]="+(pStrLine$[i]), clred, LogEdit)
		if VAlBPos>0 then exit for
		pVal$=pStrLine$[i]+pVal$'.line(WhereY)[i]
		
	next i
	' search end of value
	for j= VAlMPos+1 to len(pStrLine$)
		VAlEPos=instr(delimiter$, pStrLine$[j])
		'call  AddClrString (".line(WhereY)[j]="+str$(asc(pStrLine$[j])), clred, LogEdit)
		if VAlEPos>0 then exit for
		pVal$=pVal$+pStrLine$[j]
	next j
	
	
	'if instr(pVal$,"(")=0 then pVal$=pVal$-")"
	'if instr(pVal$,")")=0 then pVal$=pVal$-"("
	
	'if pVal$[1]="(" then pVal$=mid$(pVal$,1,len(pVal$)-2)' убираем крайние скобки
	' убираем лишнюю последнюю скобку ))
	'if tally(pVal$,"(")<> tally(pVal$,")") and right$(pVal$,2)="))" then pVal$=mid$(pVal$,1,len(pVal$)-1)
	
	result =pVal$
end with
END function

'!*****************************************************************************************************************
sub WindChoose (Sender as qmenuitem) '--- выбор из списка окон 

'if WindowsIndex=Sender.MenuIndex-InsIdx then exit sub 'это же окно не перезагружать

'call  AddClrString ("8044: WindChoose :EditFlg="+str$(EditFlg), clred, LogEdit)
' если был файл не из списка окон, то его параметры не запоминаем
if EditFlg<>0 then EditFlg=0 : srcedit.color=HLColor(0):BorderRich.color=srcedit.color: goto noinc


BMarkMain.enabled=1

'---- сохраняем параметры окна ------------------
CmdParam(WindowsIndex)=CmdLineEdit.text
CursWin(WindowsIndex)=SrcEdit.SelStart 'запоминаем положение курсора
Lastwind=WindowsIndex
WindMnu(WindowsIndex).checked=0
'WindMnu(WindowsIndex).RadioItem=1
'LastWindMnu.Hint=WindMnu(WindowsIndex)
SubsListBox(WindowsIndex).visible=0
Window(WindowsIndex)=SrcEdit.text
'call  AddClrString ("---- 8065: сохраняем параметры окна Window("+str$(WindowsIndex)+")="+(str$(WindowsIndex)), cldp, LogEdit)
Modif(WindowsIndex)=SrcEdit.Modified
ErrStatusBar.visible=0

'---- -----------------------------------------
noinc:
WindowsIndex=Sender.MenuIndex-InsIdx
call  AddClrString ("8663:WindowsIndex="+str$(WindowsIndex), clp, LogEdit)
NoPaint=1
SrcEdit.text=Window(WindowsIndex)
SrcEdit.Modified=Modif(WindowsIndex)
call  AddClrString ("8667:SrcEdit.Modified="+str$(SrcEdit.Modified), clred, LogEdit)
CmdLineEdit.text=CmdParam(WindowsIndex)

'-----------
' номера строк в margin edit
LineNumList.clear
'MarginEdit.visible=0
for i=0 to SrcEdit.LineCount +50
	LineNumList.AddItems  str$(i+1)
next i
MarginEdit.text= LineNumList.text
'MarginEdit.visible=1

if errlineNumb>0 then
	MarginEdit.visible=0
	MarginEdit.Line(errlineNumb-1)=MarginEdit.Line(errlineNumb-1)-">>"
	MarginEdit.SelStart=LineErrChar
	MarginEdit.SelLength=len(MarginEdit.Line(errlineNumb-1))
	MarginEdit.SelAttributes.color=HLColor(11)'clBlue
	MarginEdit.visible=1
end if

CloseWin.Caption = "Close Window "+str$(WindowsIndex+1) 
WindMnu(WindowsIndex).checked=1
'call  AddClrString ("8358:WindMnu("+str$(WindowsIndex)+"checked)="+str$(WindMnu(WindowsIndex).checked), cldb, LogEdit)
LChar=SendMessageAPI (SrcEdit.handle,EM_LINEINDEX,SrcEdit.LineCount-2,0) 
SrcEdit.SelStart=lChar ' выводим вниз
SrcEdit.SelStart=CursWin(WindowsIndex) 'восстанавливаем позицию курсора
NoPaint=0

SrcFileName=field$(WindMnu(WindowsIndex).caption,chr$(160),2) '
SrcFilePath$=lcase$(StripPAth (SrcFileName))
call  AddClrString ("8699:SrcFilePath$="+(SrcFilePath$), clb, LogEdit)
StatusBar.Panel(5).caption =SrcFileName
call  AddClrString ("WindChoose 8701:SrcFileName="+(SrcFileName), clb, LogEdit)
StatusPanel5.Caption=SrcFileName


SubsListBox(WindowsIndex).visible=1
'call  AddClrString ("8140:WindowsIndex="+str$(WindowsIndex), clred, LogEdit)
SubListFormResize
WatchListFormResize

SubsListBox(WindowsIndex).Column(0).caption=str$(WindowsIndex)+" "+SrcFileName
if SubsListBox(WindowsIndex).ItemCount< 50 then call AddSubs
'call  AddClrString ("8118:WindowsIndex="+str$(WindowsIndex), clred, LogEdit)

call RefreshBMark
'call  AddClrString ("8122:end RefreshBMark=", clred, LogEdit)

if Lastwind=-1 then
	LastWindMnu.Enabled=0
else
	LastWindMnu.Enabled=1
end if
call R2Change
'call  AddClrString ("8130:end R2Change=", clred, LogEdit)

OpenDialog.InitialDir=StripPATH(SrcFileName)
'call  AddClrString ("8129:SrcFileName="+(SrcFileName), clred, LogEdit)

call CreateSectionList
'call  AddClrString ("WindChoose 5914:CreateSectionList=", clred, LogEdit)

if IncTreeView.visible=1 then
	'call CreateINCfilesTree
end if

call Tab25AddTab
HtHLBoxClick

end sub
'!*****************************************************************************************************************
sub LastWindClick

if EditFlg<>0 then srcedit.color=HLColor(0):BorderRich.color=srcedit.color: EditFlg=0:goto nosrc1 'если не listed файл то его параметры не запоминаем

if Lastwind=WindowsIndex or Lastwind=-1 then 'and FileMng.checked=0
	'call  AddClrString ("5303:WindowsIndex  ="+str$(WindowsIndex  ), clred, LogEdit)
	'call  AddClrString ("5303:Lastwind="+str$(Lastwind), clred, LogEdit)
	exit sub
end if


CmdParam(WindowsIndex)=CmdLineEdit.text
CursWin(WindowsIndex)=SrcEdit.SelStart 'запоминаем позицию курсора
SubsListBox(WindowsIndex).visible=0
Window(WindowsIndex)=SrcEdit.text
WindMnu(WindowsIndex).checked=0
swap Lastwind,WindowsIndex

nosrc1:

CloseWin.Caption = "Close Window "+str$(WindowsIndex+1) 
WindMnu(WindowsIndex).checked=1
call  AddClrString ("8428:WindMnu("+str$(WindowsIndex)+"checked=)="+str$(WindMnu(WindowsIndex).checked=), clm, LogEdit)
'SubsListBox(WindowsIndex).visible=1
NoPaint=1

SrcEdit.text=Window(WindowsIndex)
SrcEdit.Modified=Modif(WindowsIndex)
call  AddClrString ("8719:SrcEdit.Modified="+str$(SrcEdit.Modified), clred, LogEdit)
CmdLineEdit.text=CmdParam(WindowsIndex)
'SrcEditLineCount=SrcEdit.LineCount
LChar=SendMessageAPI (SrcEdit.handle,EM_LINEINDEX,SrcEdit.LineCount-2,0) 
SrcEdit.SelStart=lChar
NOPAINT=0
SrcEdit.SelStart=CursWin(WindowsIndex) 'восстанавливаем позицию курсора

SrcFileName=field$(WindMnu(WindowsIndex).caption,chr$(160),2) ''
StatusBar.Panel(5).caption =SrcFileName
StatusPanel5.Caption=SrcFileName
'call AddClrString ( "SrcFileName= "+SrcFileName, clred, LogEdit)   

call R2Change
call RefreshBMark
'SubListFormResize '!!! ???
WatchListFormResize
OpenDialog.InitialDir=StripPATH(SrcFileName)

SetTab25Index

end sub

'!*****************************************************************************************************************
sub CloseWinOnClick
call  AddClrString ("8210:CloseWinOnClick Tab25.TabCount="+str$(Tab25.TabCount), clred, LogEdit)


if Tab25.TabCount=1 then exit sub

'--- сохраняем текст в файл
	IF Modif(WindowsIndex)=1 THEN call SaveAsOnClick

FileName$ =field$(WindMnu(WindowsIndex).caption,chr$(160),2)'WindMnu(WindowsIndex).caption
DelWinFileName$=FileName$
call  AddClrString ("8846:WindowsIndex="+str$(WindowsIndex), clred, LogEdit)
call  AddClrString ("8463:FileName$="+(FileName$), clred, LogEdit)


Window(WindowsIndex)=""
WindMnu(WindowsIndex).visible=0
WindMnu(WindowsIndex).checked=0
WindMnu(WindowsIndex).enabled=1
WindMnu(WindowsIndex).caption=""
SubsListBox(WindowsIndex).visible=0
CursWin(WindowsIndex)=1
StatusBar.Panel(5).caption ="" 'WindMnu(WindowsIndex).caption
StatusPanel5.Caption=""
dec VisibleWindows
call AddClrString ( "8475: VisibleWindows= "+str$(VisibleWindows), clred, LogEdit)   
'WindowsIndex=1

call Tab25DelTab



end sub

'!************************************************************************'
sub R2Change



RichRow1=str$(SrcEdit.WhereY+1)
RichCol1=str$(SrcEdit.WhereX+1)
'call  AddClrString ("8293:RichCol1="+str$(RichCol1), clred, LogEdit)

if SrcEdit.SelStart+1< len(SrcEdit.Text) then 
	pos1=SrcEdit.SelStart+1
else
	pos1=SrcEdit.SelStart
end if

char1$ = str$(asc(mid$(SrcEdit.Text, pos1,1)))
'RichX1.Caption= RichRow1+":"+RichCol1 +"    "+char1$
'RichX1.Font.Color=clBlue
SrcEditLen1=len (SrcEdit.text)

'SrcEdit.SelStart=CursWin(WindowsIndex) 

''    AddPanels "Строка ", "Столбец ", "Код символа","Номер символа в тексте"
StatusBar.Panel(0).caption = "Row "+RichRow1+":"+  str$(SrcEdit.LineCount)
StatusBar.Panel(1).caption = "Col "+RichCol1  
StatusBar.Panel(3).caption = "Asc "+char1$
StatusBar.Panel(2).caption = "Pos "+str$(pos1)+":"+  str$(SrcEditLen1)

if SrcEditLen<>SrcEdit.LineCount then 'SrcEditLen1 только при изменении числа строк
	SrcEditLen=SrcEdit.LineCount
	Modiflg=1
end if
'call  AddClrString ("8317:Modiflg="+str$(Modiflg), clred, LogEdit)

END SUB
'AddPanels "Row ", "Col ", "Pos ","Asc ", "EOL ",""  

'!************************************************************************'
sub RichEditOnMouseDown
R2Change

END SUB

'!************************************************************************'
sub BMarkChoose (Sender as qmenuitem)
RefreshBMark
CursWin(WindowsIndex)=SrcEdit.SelStart 'запоминаем'
LastBMarkIdx(WindowsIndex) =BMarkIndex (WindowsIndex)
if BMarkIndex (WindowsIndex) > -1 then BMarkMnu(BMarkIndex (WindowsIndex)).checked=0
BMarkIndex (WindowsIndex)=Sender.MenuIndex-BMarkCtl
'call AddClrString ( "BMarkChoose BMarkIndex ("+str$(WindowsIndex)+")= "+str$(BMarkIndex (WindowsIndex)), cldpurple, LogEdit)   
DelBM.Caption = "Delete BookMark "+str$(BMarkIndex (WindowsIndex)) 
BMarkMnu(BMarkIndex (WindowsIndex)).checked=1
NOPAINT=1
SrcEdit.SelStart=len(SrcEdit.text)-2
NOPAINT=0
SrcEdit.SelStart=BMarkPos(BMarkIndex (WindowsIndex),WindowsIndex)
if LastBMarkIdx(WindowsIndex)=-1 then
	LastBMarkMnu.Enabled=0
else
	LastBMarkMnu.Enabled=1
end if
R2Change
'StatusBar.Panel(5).caption =WindMnu(WindowsIndex).caption
END SUB

'!************************************************************************'
sub AddBMark
if setbmf=1 then setbmf=0: goto setbmL

if BMarkIndex (WindowsIndex) > -1 then BMarkMnu(BMarkIndex (WindowsIndex)).checked=0
LastBMarkIdx(WindowsIndex)=BMarkIndex (WindowsIndex) ' устанавливаем индекс предыдущей закладки раный текущему

if LastBMarkIdx(WindowsIndex)=-1 then
	LastBMarkMnu.Enabled=0
else
	LastBMarkMnu.Enabled=1
end if

BMarkIndex (WindowsIndex)=BMarkFreeIndex (WindowsIndex)

inc VisibleBMarkCnt(WindowsIndex) 'VisibleWindows

setbmL:
BMarkPos(BMarkIndex (WindowsIndex),WindowsIndex)=SrcEdit.SelStart
BMark(BMarkIndex (WindowsIndex),WindowsIndex)=AddChrBefore(str$(SrcEdit.WhereY+1),5," ")+chr$(160)+SrcEdit.Line(SrcEdit.WhereY)'trim$(SrcEdit.Line(SrcEdit.WhereY)-ht)
'call  AddClrString ("4630 BMark("+str$(BMarkIndex (WindowsIndex))+str$(WindowsIndex)+")="+str$(BMark(BMarkIndex (WindowsIndex),WindowsIndex)), clred, LogEdit)

BMarkMnu(BMarkIndex (WindowsIndex)).Caption=BMark(BMarkIndex (WindowsIndex),WindowsIndex)-ht
BMarkMnu(BMarkIndex (WindowsIndex)).visible=1
BMarkMnu(BMarkIndex (WindowsIndex)).checked=1
'- ищем первый от начала свободный номер
for i=0 to BMarkItemCount-1
	'call AddClrString ( "BMark("+str$(i)+").caption= "+BMarkMnu(i).caption, clBlack, LogEdit)   
	if BMarkMnu(i).caption = "" then  
		BMarkFreeIndex(WindowsIndex)=i
		exit for
	end if
next i
if i=BMarkItemCount then 
	showmessage ("Can't add more then "+str$(BMarkItemCount)+ " bookmarks. Delete one."): exit sub
else
end if
end sub
'!************************************************************************'
sub DelBMark
BMarkMnu(BMarkIndex (WindowsIndex)).Caption= "" 
BMark(BMarkIndex (WindowsIndex),WindowsIndex)=""
BMarkIndex (WindowsIndex)=LastBMarkIdx(WindowsIndex)
RefreshBMark
R2Change
end sub
'!************************************************************************'
sub RefreshBMark
'call AddClrString ( "RefreshBMark WindowsIndex= "+str$(WindowsIndex), clblue, LogEdit)   
'call  AddClrString ("5291:BMarkItemCount="+str$(BMarkItemCount), cldred, LogEdit)
for i33=0 to BMarkItemCount-1
	'call  AddClrString ("5291:i="+str$(i33), clred, LogEdit)
	BMarkMnu(i33).Caption=trim$(BMark(i33,WindowsIndex)-ht)
	'call  AddClrString ("4661 BMarkMnu("+str$(i33)+"Caption)="+(BMarkMnu(i33).Caption), clred, LogEdit)
	BMarkMnu(i33).checked=0
	if BMarkMnu(i33).Caption= "" then
		BMarkMnu(i33).visible=0
	else
		BMarkMnu(i33).visible=1
		Cap$=field$(BMarkMnu(i33).Caption,chr$(160),2)
		
		if len(CAp$)<4 then goto nexti5315: ' не обновляем, а то очень долго
		
		lchar=instr(  SrcEdit.text,  Cap$   )  ' позиция  cap$ в тексте
		if lchar=0 then BMarkMnu(i33).Caption=BMarkMnu(i33).Caption+" ??"
		while lchar >0 '<len (SrcEdit.text) 
			line_number= SendMessageAPI (SrcEdit.handle,EM_LINEFROMCHAR,lchar,0 )
			if trim$(SrcEdit.line(line_number))-ht=Cap$ and Cap$ <>"" then 
				BMarkPos(i33,WindowsIndex)=lchar:goto nexti5315 'exit while
			end if
			lchar=instr(lchar+1,  SrcEdit.text,  Cap$   )
			doevents
		wend
	end if
	nexti5315:
next i33
'call  AddClrString ("5316:i33="+str$(i33), clred, LogEdit)
BMarkMnu(BMarkIndex (WindowsIndex)).checked=1

end sub

'!************************************************************************'
sub LastBMarkClick
CursWin(WindowsIndex)=SrcEdit.SelStart 'запоминаем'
'LastBMarkIdx(WindowsIndex) =BMarkIndex (WindowsIndex)
if BMarkIndex (WindowsIndex) > -1 then BMarkMnu(BMarkIndex (WindowsIndex)).checked=0
'BMarkIndex (WindowsIndex)=Sender.MenuIndex-BMarkCtl
swap LastBMarkIdx(WindowsIndex),BMarkIndex (WindowsIndex)

'call AddClrString ( "BMarkChoose BMarkIndex ("+str$(WindowsIndex)+")= "+str$(BMarkIndex (WindowsIndex)), cldpurple, LogEdit)   
DelBM.Caption = "Delete BookMark "+str$(BMarkIndex (WindowsIndex)) 
BMarkMnu(BMarkIndex (WindowsIndex)).checked=1
NOPAINT=1
SrcEdit.SelStart=len(SrcEdit.text)-2
NOPAINT=0
SrcEdit.SelStart=BMarkPos(BMarkIndex (WindowsIndex),WindowsIndex)
'StatusBar.Panel(5).caption =WindMnu(WindowsIndex).caption
R2Change
end sub

'!************************************************************************'
sub DirTreeFormResize
DirTree.Width =DirTreeForm.ClientWidth-5
DirTree.Height =DirTreeForm.ClientHeight -60
''       CBoxHotDir.Width = DirTreeForm.ClientWidth-40
''            CBoxHotDir.Width =DirTree.Width-40
end sub
'!*********************************************************************************
sub DirTreeFormOnClose
FilesOprFlg=0
'DirTree.Directory =FileListBox1.Directory
end sub
'!************************************************************************'
sub CheckDir (Key as Byte)
'call  AddClrString ("sub CheckDir begin ", cldred, LogEdit)
SearchEditHdl=getfocus()
SELECT CASE Key
CASE 27
	DirTreeForm.Close
CASE 13
	'CBoxHotDir.AddItems DirTree.Directory
	for i=0 to CBoxHotDir.ItemCount-1
		if DirTree.Directory = CBoxHotDir.Item(i) then jz=99
	next i
	if jz <>99 then  
		CBoxHotDir.InsertItem  (1, DirBox.Text) 
		CBoxHotDir.Repaint
	else
		jz=0
	end if
	DirTree.Directory=CBoxHotDir.Text
END SELECT
end sub

'!************************************************************************'
sub GotoHotDir
DirTree.Directory=CBoxHotDir.Text
end sub
'!************************************************************************'
sub BtnDelHotDirClick

for i=0 to CBoxHotDir.ItemCount-1
	if CBoxHotDir.text = CBoxHotDir.Item(i) then    
		CBoxHotDir.DelItems i
		CBoxHotDir.text=CBoxHotDir.Item(i)
	else
	end if
next i
end sub

'!************************************************************************'
sub BtnAddHotDirClick
'CBoxHotDir.AddItems CBoxHotDir.Text

for i=0 to CBoxHotDir.ItemCount-1
	if DirTree.Directory = CBoxHotDir.Item(i) then jz=99
next i
if jz <>99 then  
	'CBoxHotDir.InsertItem  (1, DirBox.Text) 
	'!!! CBoxHotDir.AddItems ConvertToTruePath(DirTree.Directory)
	'DirBox.Text
	CBoxHotDir.Repaint
	CBoxHotDir.text=CBoxHotDir.Item(i)
else
	jz=0
end if

'!************************************************************************'
'call SaveIni
end sub

'!************************************************************************'
sub SAveLog
exit sub
fnameLog$=FileNameNoExt(PrjFileName)+".log"
'call  AddClrString ("PrjFileName= "+PrjFileName, clred, LogEdit)   

SaveDialog.InitialDir=StartDir'DirTree.Directory
'call  AddClrString ("StartDir= "+StartDir, clred, LogEdit)   

SaveDialog.Filter ="Picture files *.BMP;*.ICO;*.jpg;*.gif;*.pcx|*.BMP;*.ICO;*.jpg;*.gif;*.pcx| _
All Files *.*|*.*| _
Html Files *.*htm*;*.js;*.vbs;*.css|*.*htm*;*.js;*.vbs;*.css| _
Txt Files  *.txt;*.doc;*.rtf;*.ini|*.txt;*.doc;*.rtf;*.ini| _
Bas Files  *.bas;*.rqb|*.bas;*.rqb|"
SaveDialog.FilterIndex = 2    '' Use "All Files" as our default
SaveDialog.FileName=fnameLog$

IF SaveDialog.Execute THEN
	IF FileExists(SaveDialog.FileName) <> FALSE THEN 
		IF MessageBox("Replace file"+chr$(10)+SaveDialog.FileName +" ?", "Warning!", 1) = 1 THEN
		else
			exit sub
		END IF
	END IF
	
	nn=SaveString(LogEdit.text ,SaveDialog.FileName  )'
	'RefreshDirTree
	
ELSE
END IF

end sub

'!************************************************************************'
sub OpenFileCurs

filName$=trim$(AddInText)
filName$=filName$-cr
filName$=filName$-lf

FMngOpenFlg=0

call FileLoad

end sub
'!******************************************************************************'
sub ToUpperCAse '(Sender as QRichEdit)
select case EditHandle
case  SrcEdit.Handle
	ttm$=SrcEdit.SelText
	CharUpper ttm$
	SrcEdit.SelText=ttm$
	
	''   case  GridEdit.Handle
	''       ttm$=GridEdit.SelText
	''       CharUpper ttm$
	''      GridEdit.SelText=ttm$
	
case else
end select
end sub

'!******************************************************************************'
sub ToLoverCAse '(Sender as QRichEdit)

select case EditHandle
case  SrcEdit.Handle
	ttm$=SrcEdit.SelText
	CharLower ttm$
	SrcEdit.SelText=ttm$
	'case  GridEdit.Handle
	''    ttm$=GridEdit.SelText
	''   CharLower ttm$
	''  GridEdit.SelText=ttm$
	
case else
end select
end sub

'!******************************************************************************'
sub ClearAllRich '(Sender as QRichEdit)
select case EditHandle
case  SrcEdit.Handle
	SrcEdit.Text=""
	''         case  GridEdit.Handle
	''         GridEdit.Text=""
	
case else
end select
end sub
'!******************************************************************************'
sub SearchBlock '(Sender as QRichEdit)
SearchCBox.Text=AddInText
FindText (FindBtnDn)
end sub

'!******************************************************************************'
sub LoadFile2Window 
call  AddClrString ("9093:LoadFile2Window=", clred, LogEdit)
dim StrUtf8List as QStringList

StrUtf8List.clear

inc VisibleWindows
call  AddClrString ("9117:LoadFile2Window VisibleWindows="+str$(VisibleWindows), clred, LogEdit)
Window(WindowsIndex)=LoadString(SrcFileName) 
'call  AddClrString ("7252:Window("+str$(WindowsIndex)+")="+(Window(WindowsIndex)), cldg, LogEdit)
fext$=StripFileExt (SrcFileName)
call  AddClrString ("8836:fext$="+(fext$), clred, LogEdit)

if fext$=".ino" or fext$=".h" or fext$=".cpp" or fext$=".c"    then
	
	dim CP1251Sample as string
	CP1251Sample="укенгапролдясмитб"
	
	for i=1 to len (CP1251Sample)
		if instr (Window(WindowsIndex),CP1251Sample[i])>0 then
			ShowMessage ("9147: SrcFileName="+(SrcFileName) +" Code page is not UTF8")
			
			'exit sub
			'fext$=".NoUTF8"
		end if
		
	next i
	
	
	
	call  AddClrString ("9123:SrcFileName="+(SrcFileName), clb, LogEdit)
	StrUtf8List.Text=Window(WindowsIndex)
	call  AddClrString ("9125:StrUtf8List.ItemCount="+str$(StrUtf8List.ItemCount), clred, LogEdit)
	
	for i=0 to StrUtf8List.ItemCount-1
		StrUtf8List.Item(i)=ConvertCodePage(StrUtf8List.Item(i),cp_utf8, cp_win)
	next i
	Window(WindowsIndex)=StrUtf8List.text  
	
end if

noutf:

WindMnu(WindowsIndex).visible=1
WindMnu(WindowsIndex).caption=str$(WindowsIndex+1)+ chr$(160)+SrcFileName
WindMnu(WindowsIndex).checked=1
call  AddClrString ("8892:WindMnu("+str$(WindowsIndex)+"checked)="+str$(WindMnu(WindowsIndex).checked), clred, LogEdit)

CloseWin.Caption = "Close Window "+str$(WindowsIndex+1) 
CloseWin.enabled=1
'- ищем первый от начала свободный номер
for i=0 to WindowsItemCount-1
	if WindMnu(i).caption = "" then  
		WindowsFreeIndex=i
		exit for
	end if
next i

if i=WindowsItemCount then 
	showmessage ("Can't open more then "+str$(WindowsItemCount)+ " windows. Close one."):exit sub
else
end if

NOPAINT=1
SrcEdit.text=Window(WindowsIndex)
'SrcEditLineCount=SrcEdit.LineCount
SrcEdit.Modified=Modif(WindowsIndex)
call  AddClrString ("9166:SrcEdit.Modified="+str$(SrcEdit.Modified), clred, LogEdit)
'SrcEdit.Modified=0
NOPAINT=0

if instr(SrcEdit.text ,crlf) > 0 then
	StatusBar.Panel(4).caption ="EOL=CRLF(13 10)"
elseif instr(SrcEdit.text ,cr) >0 then
	StatusBar.Panel(4).caption ="EOL=CR(13)"
elseif instr(SrcEdit.text ,lf) >0 then
	StatusBar.Panel(4).caption ="EOL=LF(10)"
else
	StatusBar.Panel(4).caption ="-"
end if
'doevents    
'SrcFileName=OpenDialog.FileName
StatusBar.Panel(5).caption =SrcFileName
StatusPanel5.Caption=SrcFileName
'SubList.visible=true:
'SubsListBox(WindowsIndex).visible=1
SubsListBox(WindowsIndex).Column(0).caption=WindMnu(WindowsIndex).caption'str$(WindowsIndex)+" "+SrcFileName

call  AddSubs
'call  AddClrString ("Сформировали список функций ", clDGreen, LogEdit)   
call RefreshBMark 'обновляем закладки'
SrcEdit.Modified=0
Modif(WindowsIndex)=SrcEdit.Modified



call Tab25AddTab


call HtHLBoxClick

end sub                   

'!******************************************************************************'
sub RQFormShow
'print"9226 RQFormShow="
call  AddClrString ("6696: RQFormShow ProjectIni.FileName="+ProjectIni.FileName, clred, LogEdit)

nopaint=1

Tab25.clear
TabListCmbox.clear
SrcEdit.clear

'print"9243: ProjectIni.FileName=";ProjectIni.FileName

'!!!------ восстановление окон и закладок------------------
'if RQdbini.exist>0 then

'call  AddClrString ("9248:ProjectIni.FileName="+(ProjectIni.FileName), clg, LogEdit)

if fileexists(ProjectIni.FileName) >0 then
	RQdbiniflg=1
	' восстанавливаем положения окон '-----------------------------------------------------
	ProjectIni.Section="WatchList"
	'val(ProjectIni.get("SyntaxHighLight","1"))
	
	WatchList.Top=val (ProjectIni.get("Top",str$(WatchList.Top)  ))
	WatchList.Left=val (ProjectIni.get("Left",str$(WatchList.Left)  ))
	WatchList.Width=val (ProjectIni.get("Width",str$(WatchList.Width)  ))
	WatchList.Height=val (ProjectIni.get("Height",str$(WatchList.Height)  ))
	WLTopM=val (ProjectIni.get("TopM",str$(WLTopM)  ))
	WLLeftM=val (ProjectIni.get("LeftM",str$(WLLeftM)  ))
	WatchList.Visible=val (ProjectIni.get("Visible",str$(WatchList.Visible)  ))
	WatchList.WindowState=val (ProjectIni.get("WindowState",str$(WatchList.WindowState)  ))
	
	
	ProjectIni.Section="SubList"
	'SubList.Top=val (ProjectIni.get("Top",str$(SubList.Top)  ))
	'SubList.Left=val (ProjectIni.get("Left",str$(SubList.Left)  ))
	'SubList.Width=val (ProjectIni.get("Width",str$(SubList.Width)  ))
	'SubList.Height=val (ProjectIni.get("Height",str$(SubList.Height)  ))
	'SLTopM=val (ProjectIni.get("TopM",str$(SLTopM)  ))
	'SLLeftM=val (ProjectIni.get("LeftM",str$(SLLeftM)  ))
	'SubList.Visible=val (ProjectIni.get("Visible",str$(SubList.Visible)  ))
	'SubList.WindowState=val (ProjectIni.get("WindowState",str$(SubList.WindowState)  ))
	
	'SearchFrm.visible=true
	
	ProjectIni.Section="ObjInspector"
	'ObjectInspector
	ObjInTopM=val (ProjectIni.get("TopM",str$(ObjInTopM)  ))
	ObjInLeftM=val (ProjectIni.get("LeftM",str$(ObjInLeftM)  ))
	ObjectInspector.Visible=val (ProjectIni.get("Visible",str$(ObjectInspector.Visible)  ))
	'call  AddClrString ("5752:ObjectInspector.Visible="+str$(ObjectInspector.Visible), clred, LogEdit)
	ObjectInspector.WindowState=val (ProjectIni.get("WindowState",str$(ObjectInspector.WindowState)  ))
	ObjectInspector.Top=val (ProjectIni.get("Top",str$(ObjectInspector.Top)  ))
	ObjectInspector.Left=val (ProjectIni.get("Left",str$(ObjectInspector.Left)  ))
	ObjectInspector.Width=val (ProjectIni.get("Width",str$(ObjectInspector.Width)  ))
	ObjectInspector.Height=val (ProjectIni.get("Height",str$(ObjectInspector.Height)  ))
	
	'ObjInTopM=80:ObjInLeftM=400:OnceFlgObj=0
	
	' считываем  настройки редактора '-----------------------------------------------------
	ProjectIni.Section="Editor"
	SyntaxHLBox.checked=val(ProjectIni.get("SyntaxHighLight","1"))
	HtHLBox.checked=val(ProjectIni.get("HotTabsHighLight","1"))
	
	RQdbini.Section="Arduino"
	sketchbookPathEdit.text=RQdbini.get("sketchbookPathEdit",StartPath$)
	
	if direxists (sketchbookPathEdit.text) =0 then 
		sketchbookPathEdit.text=StartPath$+"projects\sketchbook\"
	end if
	
	CliPathEdit.text=RQdbini.get("CliPathEdit",StartPath$)
	
	BuildPathEdit.text=RQdbini.get("BuildPathEdit",StartPath$)
	if direxists (BuildPathEdit.text) =0 then 
		BuildPathEdit.text=StartPath$+"arduino-cli\BuildPath\"
	end if
	
	
	ArdLibraryPathEdit.text=RQdbini.get("ArdLibraryPathEdit",StartPath$)
	
	if direxists (ArdLibraryPathEdit.text) =0 then 
		ArdLibraryPathEdit.text=StartPath$+"projects\sketchbook\libraries"
	end if
	
	
	ArdLogFileEdit.text=RQdbini.get("ArdLogFileEdit",StartPath$)
	if fileexists (ArdLogFileEdit.text) =0 then 
		ArdLogFileEdit.text=StartPath$+"CompileMsg.log"
	end if
	
	
	
	'RQdbini.write("BuildPathEdit",BuildPathEdit.text)
	'RQdbini.write("LibraryPathEdit",LibraryPathEdit.text)
	'RQdbini.write("ArdLogFileEdit",ArdLogFileEdit.text)
	
	
	CliPathEdit.text=RQdbini.get("CliPathEdit",StartPath$)
	CliPathEdit.text=RQdbini.get("CliPathEdit",StartPath$)
	CliPathEdit.text=RQdbini.get("CliPathEdit",StartPath$)
	
	
	
	ChkBox1.checked=val(RQdbini.get("ArdCompileDetail","0"))
	ChkBox2.checked=val(RQdbini.get("ArdLoadDetail","0"))
	ChkBox4.checked=val(RQdbini.get("ArdCheckCode","1"))
	ChkBox5.checked=val(RQdbini.get("ArdSaveScetch","1"))
	
	'Combo2.ItemIndex=val(RQdbini.get("ArdCompilerMsg","1"))
	Edit2.Text=RQdbini.get("ArdBoardMngLinks",Edit2.Text)
	
	'!!! считываем  директории компилятора  '-----------------------------------------------------
	'call  AddClrString ("9345:считываем  директории компилятора =", clred, LogEdit)
	RQdbini.Section="Compiler"
	
	' в IDEPathEdit.text -  сохраненная стартовая директория IDE на другом компьютере
	IDEPathEdit.text=RQdbini.get("IDEPath",StartPath$+"")
	'print"9347:-------------> IDEPathEdit.text=";IDEPathEdit.text
	'call  AddClrString ("============= 9352:RQdbini.FileName="+(RQdbini.FileName), cld, LogEdit)
	
	RQdbiniTxt$=LoadString(RQdbini.FileName)
	'call  AddClrString ("9352:RQdbiniTxt$="+(RQdbiniTxt$), clred, LogEdit)
	'call  AddClrString ("9354:RQdbini.FileName="+(RQdbini.FileName), clm, LogEdit)
	' заменяем в RQdbini старый путь IDE на новый
	RQdbiniTxt$=REPLACESUBSTR$(RQdbiniTxt$,IDEPath$,IDEPathEdit.text)
	
	'call  AddClrString ("9358:IDEPath$="+(IDEPath$), clred, LogEdit)
	SaveString(RQdbiniTxt$,RQdbini.FileName)
	
	'IDEPathEdit.text=StartPath$ ' загружаем текущую директорию IDE
	
	CmpPathEdit.text=RQdbini.get("CompPath",StartPath$+"")
	IncPathEdit.text=RQdbini.get("IncPath",StartPath$+"inc\")
	LibPathEdit.text=RQdbini.get("LibPath",StartPath$+"lib\")
	
	' PrjPathEdit.text=RQdbini.get("PrjPath",StartPath$+"projects\")
	
	IcoFileEdit.text=RQdbini.get("IcoFile",StartPath$+"default.ico")
	TplPathEdit.text=RQdbini.get("TplFile",StartPath$+"templates\")
	
	
	FBCmpPathEdit.text=RQdbini.get("FBCompPath",StartPath$)
	
	if direxists (CmpPathEdit.text) =0 then CmpPathEdit.text=StartPath$+""
	if direxists (IncPathEdit.text) =0 then IncPathEdit.text=StartPath$+"inc\"
	if direxists (LibPathEdit.text) =0 then LibPathEdit.text=StartPath$+"lib\"
	if direxists (TplPathEdit.text) =0 then TplPathEdit.text=StartPath$+"templates\"
	if fileexists (IcoFileEdit.text) =0 then IcoFileEdit.text=StartPath$+"default.ico"
	if direxists (FBCmpPathEdit.text) =0 then FBCmpPathEdit.text=StartPath$+"freebasic\"
	
	
	
	brem 0
	erem
	
	
	'!!! считываем  HotDirs '-----------------------------------------------------
	ProjectIni.Section="HotDirs"
	HotDirsItemCount = val(ProjectIni.get("HotDirsCount","1"))
	'print"9374 HotDirsItemCount=";HotDirsItemCount
	'call  AddClrString ("9134:HotDirsItemCount="+str$(HotDirsItemCount), cldg, LogEdit)
	
	'call  AddClrString ("RQFormShow CBoxHotDir.ItemCount-1="+str$(CBoxHotDir.ItemCount-1), clred, LogEdit)
	for i=0 to HotDirsItemCount-1
		
		HotDir$=ProjectIni.get(str$(i),"")
		
		if direxists(HotDir$)>0 then
			
			if i> CBoxHotDir.ItemCount-1 then
				
				CBoxHotDir.AddItems HotDir$ 'ProjectIni.get(str$(i),"") 
			else
				CBoxHotDir.Item(i)=HotDir$ 'ProjectIni.get(str$(i),"") 
			end if
		end if
	next i
	HotDirsIdx=val(ProjectIni.get(Index,"1"))
	CBoxHotDir.Text=CBoxHotDir.Item(HotDirsIdx)
	
	'!!! считываем  Windows '-----------------------------------------------------
	
	dim StrUtf8List as QStringList
	
	ProjectIni.Section="Windows" 
	'call  AddClrString ("9159: RQFormShow WindowsItemCount= "+str$(WindowsItemCount), cldblue, LogEdit)   
	
	VisibleWindows=0 '
	
	'!!! есть массив меню WindMnu(48) 
	'!!! сейчас мы его будем заполнять из  ProjectIni.Section="Windows" 
	
	'!iw - общий счетчик, VisibleWindows - счетчик окон с рабочими файлами, пустые файлы пропускаются
	
	' рабочие пунккты меню имеют индекс VisibleWindows !!!
	'call  AddClrString ("9277:WindowsItemCount="+str$(WindowsItemCount), clred, LogEdit)
	
	for iw=0 to WindowsItemCount-1
		WindowsIndex=iw
		'doevents
		'call  AddClrString ("9116:iw="+str$(iw), clb, LogEdit)
		ProjectIni.Section="Windows" 
		paramwin$=ProjectIni.get("win"+str$(iW),"") ' сохраняется menu caption, поэтому надо парсить
		call  AddClrString ("9464:paramwin$="+(paramwin$), clred, LogEdit)
		'2 C:\bas\RapidQ\RQIDE\projects\RapidFRMv1_43\DataParser15.bas
		
		SrcFileName=field$(paramwin$,chr$(160),2) ' имя файла с полным путем!!!!
		
		if SrcFileName="" then goto nextiw:
		'print"9447: SrcFileName=";SrcFileName
		
		
		
		if fileexists(SrcFileName)=0  then  
			'call  AddClrString ("9447:SrcFileName not exist="+(SrcFileName), clred, LogEdit)
			'RQdbiniTxt$=REPLACESUBSTR$(RQdbiniTxt$,IDEPath$,IDEPathEdit.text)
			call  AddClrString ("9472:Not found SrcFileName="+(SrcFileName), cldb, LogEdit)
			fpath$=StripPath (SrcFileName)
			call  AddClrString ("9480:fpath$="+(fpath$), clred, LogEdit)
			IdeFolderName$=RIGHT$(IDEPath$,rinstr(2,IDEPath$,"\"))
			
			zzz=rinstr(len(IDEPath$)-1,IDEPath$,"\")
			print"zzz=";zzz
			IdeFolderName$=RIGHT$(IDEPath$,len(IDEPath$)-zzz)-"\"   
			call  AddClrString ("9605:IdeFolderName$="+(IdeFolderName$), clred, LogEdit)
			
			
			if instr(fpath$,IdeFolderName$)>0 then
				fpathIDE$=left$((SrcFileName),instr(lcase$(SrcFileName),lcase$(IdeFolderName$))+len(IdeFolderName$))
			end if
			call  AddClrString ("9601:fpathIDE$="+(fpathIDE$), clm, LogEdit)
			
			'SrcFileName=REPLACESUBSTR$(SrcFileName,IDEPathEdit.text,IDEPath$)
			
			SrcFileName=REPLACESUBSTR$(SrcFileName,fpathIDE$,IDEPath$)
			call  AddClrString ("====  9606:SrcFileName="+(SrcFileName), cldb, LogEdit)
			
			
			call  AddClrString ("9479:IDEPath$="+(IDEPath$), clred, LogEdit)
			'call  AddClrString ("9479:IDEPathEdit.text="+(IDEPa thEdit.text), cldg, LogEdit)
			'!9479:IDEPath$=C:\balan\RQIDE\
			
			paramwin$=REPLACESUBSTR$(paramwin$,fpathIDE$,IDEPath$)
			call  AddClrString ("9454:paramwin$="+(paramwin$), clo, LogEdit)
			
			'print"9449: paramwin$=";paramwin$
			
			
			if fileexists(SrcFileName)=0  then 
				'SrcFileName="???"+SrcFileName  'goto nextiw: '!!!! обрабатываем пустые\несуществующие файлы
				'SrcFileName=+SrcFileName  'goto nextiw: '!!!! обрабатываем пустые\несуществующие файлы
				call  AddClrString ("9492:Not found SrcName="+(SrcFileName), cldp, LogEdit)
				
			end if
		end if
		
		'call  AddClrString ("9464:paramwin$="+(paramwin$), clo, LogEdit)
		CmdParam(VisibleWindows)=ProjectIni.get("CmdLine"+str$(iW),"") ' параметры командной строки
		
		if paramwin$<>"" then 'при этом убираем пустые индексы
			
			
			if instr(paramwin$,":")=0 then ' если имя файла не содержится полный путь, то
				
				'win1=2 C:\bas\RapidQ\RQIDE\projects\RapidFRMv1_43\DataParser15.bas,k    'paramwin$=field$(paramwin$,chr$(160),1)+chr$(160)+StartPath$+field$(paramwin$,chr$(160),2)
				
				paramwin$=str$(VisibleWindows+1)+chr$(160)+StartPath$+field$(paramwin$,chr$(160),2) '!!! новые номера окон
				
			else ' если полный путь
				paramwin$=str$(VisibleWindows+1)+chr$(160)+field$(paramwin$,chr$(160),2) 'строка меню
				paramwin$=ReplaceSubStr$(paramwin$,StartPath$,IdePath$) '!!! заменяем старый путь IDE на новый
				'call  AddClrString ("9138:paramwin$="+(paramwin$), clp, LogEdit)
				'paramwin$=str$(iW+1)+chr$(160)+StartPath$+field$(paramwin$,chr$(160),2) '!!! новые номера окон
				
				
			end if
			
			'call  AddClrString ("8968:paramwin$="+(paramwin$), cldr, LogEdit)
			
			WindMnu(VisibleWindows).caption=paramwin$
			'call  AddClrString ("9159:WindMnu("+str$(VisibleWindows)+")caption)="+(WindMnu(VisibleWindows).caption), clred, LogEdit)
			SrcFileName=field$(paramwin$,chr$(160),2)
			SrcFileNameNoPath$=FileNameNoExt(SrcFileName)
			'call  AddClrString ("9491:SrcFileNameNoPath$="+(SrcFileNameNoPath$), cllb, LogEdit)
			'call  AddClrString ("9492:SrcFileName="+(SrcFileName), clb, LogEdit)
			
			if fileexists(SrcFileName)<> 0  then   'goto nextiw: '!!! пропускаем пустые окна
				Window(VisibleWindows)=LoadString(SrcFileName)
				WindMnu(VisibleWindows).enabled=1
			else
				WindMnu(VisibleWindows).enabled=0
			end if
			'call  AddClrString ("9154:VisibleWindows="+str$(VisibleWindows), clred, LogEdit)
			
			fext$=StripFileExt (SrcFileName)
			'call  AddClrString ("9500:fext$="+(fext$), clred, LogEdit)
			
			
			dim CP1251Sample as string
			CP1251Sample="укенгапролдясмитб"
			
			for i=1 to len (CP1251Sample)
				if instr (Window(VisibleWindows),CP1251Sample[i])>0 then
					'ShowMessage ("12169:Code page is not UTF8")
					'exit sub
					fext$=".NoUTF8"
				end if
				
			next i
			
			if fext$=".ino" or fext$=".h" or fext$=".cpp" or fext$=".c" then ' RQFormShow !!!
				'sleep 0.9
				'call  AddClrString ("9424: ino SrcFileName="+(SrcFileName), cldg, LogEdit)
				
				'StrUtf8List.Text=Window(VisibleWindows)
				'for ik=0 to StrUtf8List.ItemCount-1
				'zzz$=StrUtf8List.Item(ik)
				'zzz$=ConvertCodePage(zzz$,cp_utf8, cp_win)
				'StrUtf8List.Item(ik)=zzz$ 'ConvertCodePage(zzz$,cp_utf8, cp_win)
				
				'next ik
				'Window(VisibleWindows)=StrUtf8List.text  
				'doevents
				
			end if
			
			
			WindMnu(VisibleWindows).visible=1
			CursWin(VisibleWindows)=val(ProjectIni.get("curposic"+str$(WindowsIndex),"1") )
			ProjectIni.Section="Bookmarks" 
			
			
			for bmi=0 to BMarkItemCount-1
				bmarkCaption$=ProjectIni.get("bmarkCaption"+str$(WindowsIndex)+","+str$(bmi) ,"") 
				bmarkPosic$=ProjectIni.get("bmarkPosic"+str$(WindowsIndex)+","+str$(bmi)  ,"") 
				if bmarkCaption$<>"" then 'если не пусто - формируем меню
					bMark(VisibleBMarkCnt(VisibleWindows),VisibleWindows)=bmarkCaption$
					'call  AddClrString ("5019 bmarkCaption$="+(bmarkCaption$), clred, LogEdit)
					bMarkPos(VisibleBMarkCnt(VisibleWindows),VisibleWindows)=val(bmarkPosic$)
					inc VisibleBMarkCnt(VisibleWindows)
					
				end if
			next bmi
			BMarkFreeIndex (VisibleWindows)=VisibleBMarkCnt(VisibleWindows)+1
			inc VisibleWindows
		end if
		
		nextiw:
	next iw
	WindowsFreeIndex=VisibleWindows '!!! ??? первый свободный индекс меню 
	'print" 9554 WindowsFreeIndex=";WindowsFreeIndex
	'call  AddClrString ("9555:VisibleWindows="+str$(VisibleWindows), clred, LogEdit)
	'WindMnu(VisibleWindows-1).checked=1
	'WindowsIndex=VisibleWindows-1
	'call  AddClrString ("9194:WindowsFreeIndex="+str$(WindowsFreeIndex), clred, LogEdit)
	
	'!!! WindowsIndex - индекс текущего окна, которое загружается в редактор SrcEdit 
	'exit sub 
	'call  AddClrString ("9565:WindowsItemCount="+str$(WindowsItemCount), cldb, LogEdit)
	
	'!! пытаемся convertpage  вставить сюда
	WaitForm.show
	for iw=0 to WindowsItemCount-1
		WaitLabel.caption=str$(iw)
		Gauge2.position=iw
		if Gauge2.position=Gauge2.max then Gauge2.position=0
		SrcFileName=field$(WindMnu(iw).caption,chr$(160),2)
		fext$=""
		
		if SrcFileName<>"" and fileexists(SrcFileName)<>0 then
			
			'call  AddClrString ("9570:SrcFileName="+(SrcFileName), cldg, LogEdit)
			SrcFileNameNoPath$=FileNameNoExt(SrcFileName)
			fext$=StripFileExt (SrcFileName)
			'call  AddClrString ("9573:fext$="+(fext$), clred, LogEdit)
			'call  AddClrString ("9521:iw="+str$(iw), clred, LogEdit)
			'RichBox.clear
			
			if fext$=".ino" or fext$=".h" or fext$=".cpp" or fext$=".c" then
				'call  AddClrString ("9424: ino SrcFileName="+(SrcFileName), cldb, LogEdit)
				'doevents
				
				StrUtf8List.Text=Window(iw)
				'call  AddClrString ("9521:iw="+str$(iw), clred, LogEdit)
				'call  AddClrString ("9450:StrUtf8List.ItemCount="+str$(StrUtf8List.ItemCount), clred, LogEdit)
				for ik=0 to StrUtf8List.ItemCount-1
					if len(StrUtf8List.Item(ik))>8 then
						'call  AddClrString ("9590:StrUtf8List.Item("+str$(ik)+")="+(StrUtf8List.Item(ik)), 0, LogEdit)
						StrUtf8List.Item(ik)=ConvertCodePage(StrUtf8List.Item(ik),cp_utf8, cp_win)
					end if
					'doevents
				next ik
				Window(iw)=StrUtf8List.text  
				'doevents
				
			end if
		end if
		'doevents
	next iw
	
	WaitForm.visible=0
	
	WindowsIndex=val(ProjectIni.get("Index","0"))
	'call  AddClrString ("9599:before WindowsIndex="+str$(WindowsIndex), clred, LogEdit)
	if WindowsIndex>VisibleWindows-1 then WindowsIndex=VisibleWindows-1
	
	
	
	'call  AddClrString ("9604:after WindowsIndex="+str$(WindowsIndex), 0, LogEdit)
	
	if WindowsIndex<0 then WindowsIndex=0 'exit sub
	'print"WindowsIndex=";WindowsIndex
	
	WindMnu(WindowsIndex).checked=1
	WindMnu(WindowsIndex).visible=1
	'call  AddClrString ("9192:WindMnu("+str$(WindowsIndex)+").checked="+str$(WindMnu(WindowsIndex).checked), clred, LogEdit)
	'call  AddClrString ("9193:WindMnu("+str$(WindowsIndex)+").caption="+(WindMnu(WindowsIndex).caption), cldg, LogEdit)
	
	'doevents
	
	'!!! показываем закладки для  окна
	
	BMarkIndex (WindowsIndex)=0
	BMarkMnu(BMarkIndex (WindowsIndex)).visible=1
	BMarkMnu(BMarkIndex (WindowsIndex)).checked=1
	
	
	
	for bmi=0 to BMarkItemCount-1 ' все непустые закладки делаем видимыми
		BMarkMnu(bmi).Caption=BMark(bmi,WindowsIndex)
		'call  AddClrString ("9294: BMarkMnu("+str$(bmi)+"Caption)="+(BMarkMnu(bmi).Caption), clred, LogEdit)
		if BMarkMnu(bmi).Caption <>"" then 
			BMarkMnu(bmi).visible=1
		else
			BMarkMnu(bmi).visible=0
		end if
	next bmi
	
	
	
	'SrcEdit.visible=0
	'NOPAINT=1
	SrcEdit.text=Window(WindowsIndex)
	SrcEdit.Modified=Modif(WindowsIndex)
	'call  AddClrString ("9536:SrcEdit.Modified="+str$(SrcEdit.Modified), clred, LogEdit)
	SrcFileName=field$(WindMnu(WindowsIndex).caption,chr$(160),2)
	'call  AddClrString ("9309:SrcFileName="+(SrcFileName), clred, LogEdit)
	SrcFilePath$=lcase$(StripPAth (SrcFileName))
	'call  AddClrString ("6560:SrcFilePath$="+(SrcFilePath$), clred, LogEdit)
	
	
	
	CmdLineEdit.text=CmdParam(WindowsIndex)
	'!!!---- выставляем курсор в запомненную позицию
	'NOPAINT=1
	LChar=SendMessageAPI (SrcEdit.handle,EM_LINEINDEX,SrcEdit.LineCount-2,0) 
	'SrcEdit.visible=0
	SrcEdit.SelStart=lChar ' выводим вниз
	'NOPAINT=0
	SrcEdit.SelStart=CursWin(WindowsIndex) 'восстанавливаем позицию курсора
	'SrcEdit.visible=1
	' номера строк в margin edit
	'doevents
	MarginEdit.visible=0
	for i=0 to SrcEdit.LineCount +50
		LineNumList.AddItems  str$(i+1)
	next i
	MarginEdit.text= LineNumList.text
	NOPAINT=0
	SrcEdit.visible=1
	MarginEdit.visible=1
	
	
	
	
	SubsListBox(WindowsIndex).Column(0).caption=WindMnu(WindowsIndex).caption'str$(WindowsIndex)+" "+SrcFileName
	'!!!---- показываем  статусбар
	'call  AddClrString ("5850:показываем  статусбар=", clred, LogEdit)
	
	
	call R2Change
	print" 9640 R2Change="
	
	call HtHLBoxClick
	print"HtHLBoxClick="
	
	if instr(SrcEdit.text ,crlf) > 0 then
		StatusBar.Panel(4).caption ="EOL=CRLF(13 10)"
	elseif instr(SrcEdit.text ,cr) >0 then
		StatusBar.Panel(4).caption ="EOL=CR(13)"
	elseif instr(SrcEdit.text ,lf) >0 then
		StatusBar.Panel(4).caption ="EOL=LF(10)"
	else
		StatusBar.Panel(4).caption ="-"
	end if
	'doevents
	StatusBar.Panel(5).caption =SrcFileName
	StatusPanel5.Caption=SrcFileName
	
	
	
	call  AddSubs
	print"9661 AddSubs="
	'call  AddClrString ("SubList created ", clDGreen, LogEdit)   
	
	'else
	'ShowMessage ("Ini file "+ProjectIni.FileName+" not found")
	'call  AddClrString ("6693:Ini file "+ProjectIni.FileName+" not found", clred, LogEdit)
	'end if
	'!!!----------- создаем список inc файлов  -------------
	call CreateSectionList
	print"CreateSectionList="
	'CreateINCfileslist
	
	
	'!!!----------- создаем список вкладок   -------------
	
	'!!! считываем Tab25 ----------------------------------------------------
	ProjectIni.Section="Tabs"
	Tab25TabCount=val(ProjectIni.get("TabCount","1"))
	
	'print"9679 Tab25TabCount=";Tab25TabCount
	'call  AddClrString ("9081:Tab25TabCount="+str$(Tab25TabCount), cldg, LogEdit)
	
	'!!! во вкладках должны присутствовать только файлы входящие в список окон в меню!!!
	' включить проверку этого!
	
	Tab25.clear
	TabListCmbox.clear
	
	SrcFN$=field$(WindMnu(WindowsIndex).caption,chr$(160),2) '
	'call  AddClrString ("9334:SrcFN$="+(SrcFN$), clred, LogEdit)
	flWIndex=0
	
	for i=0 to Tab25TabCount-1
		
		Tab25Tab1$=ProjectIni.get(str$(i),"")
		Tab25Tab$=field$(Tab25Tab1$,chr$(160),2)
		'call  AddClrString ("9095:Tab25Tab$="+(Tab25Tab$), clred, LogEdit)
		
		if Tab25Tab$=SrcFN$ then flWIndex=1 ' текущее окно присутствует в списке вкладок
		
		if fileexists(Tab25Tab$)>0 then
			TabListCmbox.AddItems Tab25Tab1$
			Tab25.AddTabs(StripFileName(Tab25Tab$))
		end if
	next i
	
	
	if flWIndex=0 then 'окна нет, добавляем ко вкладкам
		TabListCmbox.AddItems WindMnu(WindowsIndex).caption
		Tab25.AddTabs(StripFileName(SrcFN$)) ' GetFileNameNoPath
		
		'!!! синхронизируем с окном текущим Windows
		'!!! WindMnu(WindowsIndex).visible=1
		
		Tab25.TabIndex=Tab25.TabCount-1
		'call  AddClrString ("9361:Tab25.TabIndex="+str$(Tab25.TabIndex), clp, LogEdit)
		TabListCmbox.ItemIndex=Tab25.TabIndex 
		
		
	else
		Tab25.TabIndex=val(ProjectIni.get("TabIndex","0"))
		'call  AddClrString ("9346:Tab25.TabIndex="+str$(Tab25.TabIndex), clred, LogEdit)
		TabListCmbox.ItemIndex=Tab25.TabIndex 
		
	end if
	
	
	
	SetTab25length
else
	ShowMessage ("9731: Ini file "+ProjectIni.FileName+" not found")
	'call  AddClrString ("6693:Ini file "+ProjectIni.FileName+" not found", clred, LogEdit)
end if


nopaint=0


IDEPathEdit.text=StartPath$ ' загружаем текущую директорию IDE
'TimerOnce.Enabled = 1


end sub


'!***********************************************************************
sub PrjChoose (Sender as qmenuitem)
call  AddClrString ("6980:PrjChoose  -----------------------------", 0, LogEdit)
ErrStatusBar.visible=0

'call  AddClrString ("5911:DelPrjFlg="+str$(DelPrjFlg), clred, LogEdit)
if DelPrjFlg=1 then DelPrjFlg=0: PrjIndex=0:goto DelPrjLbl

call SavePrjFileOnClick ' сохраняем текущий проект

call clearWin_bm ' очищаем окна и закладки

PrjMnu(PrjIndex).checked=0
OldPrjIdx=PrjIndex
'call  AddClrString ("9794:OldPrjIdx="+str$(OldPrjIdx), clred, LogEdit)


PrjIndex=Sender.MenuIndex-PrjBeg '!!!!!!!!новый индекс проекта !!
PrjMnu(PrjIndex).checked=1

DelPrjLbl:
'call  AddClrString ("6995: PrjChoose.PrjIndex="+str$(PrjIndex), cldred, LogEdit)

'RQdbini.FileName="c:\BAS\RAPIDQ\RQ IDE\RQdb.ini"
RQdbini.FileName=StartDir+"RQdb.ini"

'call  AddClrString ("7002: RQdbini.FileName="+(RQdbini.FileName), clred, LogEdit)

RQdbini.Section="Projects"

NewProjectIniFileName$=RQdbini.get("PrjIni"+str$(PrjIndex),"")
call  AddClrString ("9859:NewProjectiniFileName$="+(NewProjectiniFileName$), cldb, LogEdit)

if instr(NewProjectiniFileName$,":")=0 then
	NewProjectiniFileName$=PrjPathEdit.text+RQdbini.get("PrjIni"+str$(PrjIndex),"")
end if

'call  AddClrString ("7012: NewProjectiniFileName$="+(NewProjectiniFileName$), clred, LogEdit)

if fileexists(NewProjectiniFileName$) then '(NewRQdbiniFileName$)<>""
	Projectini.FileName=NewProjectiniFileName$
else
	showmessage("9919:Project file not found "+NewProjectiniFileName$)
	checkIni=1
	'! обрабатываем отсутствующий проект
	'! проверяем существует ли папка проекта 
	prjfolder$=PrjPathEdit.text+FileNameNoExt(NewProjectiniFileName$)
	call  AddClrString ("9923:prjfolder$="+(prjfolder$), clred, LogEdit)
	
	'! 
	'! 
	PrjMnu(PrjIndex).checked=0 ' сбрасывем флаг с несуществующего проекта
	PrjIndex= OldPrjIdx
	'call  AddClrString ("5919:OldPrjIdx="+str$(OldPrjIdx), clred, LogEdit)
	'RQdbini.FileName=RQdbini.get("PrjIni"+str$(PrjIndex),"")
	'call  AddClrString ("5940:RQdbini.FileName="+str$(RQdbini.FileName), clred, LogEdit)
	'PrjMnu(PrjIndex).checked=1
	'exit sub
end if

checkIni=1
'call  AddClrString ("9836:checkIni="+str$(checkIni), clo, LogEdit)

call  AddClrString ("9939:Projectini.FileName="+(Projectini.FileName), clred, LogEdit)

PrjMnu(PrjIndex).checked=1
LabelPrj.caption="  Project "+PrjMnu(PrjIndex).caption
Timer1.Enabled = 0 'True !!!
sleep 0.2
'call RQFormShow:
print 10049
TimerOnceOver
'Timer1.Enabled = 1 'True !!!
print 10051
END SUB

'!***********************************************************************

'!************************************************************************'
sub  CreatePrjOnClick
call  AddClrString ("7032: CreatePrjOnClick-------------------------=", cldblue, LogEdit)

' сохраняем настройки текущего проекта
SavePrjFileOnClick

'---- сохраняем параметры текущего проекта ------------------
LastPrj=PrjIndex
call  AddClrString ("7039: CreatePrjOnClick LastPrj="+str$(LastPrj), clred, LogEdit)
PrjMnu(PrjIndex).checked=0
'LastWindMnu.Hint=WindMnu(WindowsIndex)
SubsListBox(WindowsIndex).visible=0
' сохраняем файлы текущего проекта
for i=0 to WindowsItemCount-1 
	if instr(WindMnu(i).caption,chr$(149) )>0 then
		SrcFileName1$=field$(WindMnu(i).caption,chr$(160),2):
		'call  AddClrString ("CreatePrjOnClick Save source into "+SrcFileName1$, clred, LogEdit)
		
		'RQDebug("i",i,SrcFileName1,SrcFileName1)
		showmessage ("7050: Save source into "+SrcFileName1$)
		if SaveString(Window(I) ,SrcFileName1$)< 1 then 
			showmessage ("7052:Can't save source into "+SrcFileName1$)
			'exit sub
		end if
	end if
next i
'exit sub

' создаем новый проект ------------------------------
NewProjectBox.showmodal
'PrjNAME(PrjIndex)

'- ищем первый от начала свободный номер
for i=0 to PrjItemCount-1
	'call AddClrString ( "PrjMnu("+str$(i)+").caption= "+PrjMnu(i).caption, clBlack, LogEdit)   
	if PrjMnu(i).caption = "" then  
		'if PrjNAME(i) = "" then  
		' PrjFreeIndex=i
		PrjIndex=i
		'call AddClrString ( "ищем первый от начала свободный номер PrjFreeIndex= "+str$(PrjFreeIndex), clred, LogEdit)   
		exit for
	end if
next i

if i=PrjItemCount then 
	showmessage ("Can't open more then "+str$(PrjItemCount)+ " projects. Close one."):exit sub
else
end if

' создаем меню
'PrjIndex=PrjFreeIndex
'PrjMnu(PrjIndex).caption=str$(PrjIndex)+chr$(160)+PrjNameEdit.text

PrjMnu(PrjIndex).caption=PrjNameEdit.text
PrjMnu(PrjIndex).visible=1
PrjMnu(PrjIndex).checked=1

'очищаем наборы окон и закладок
call clearWin_bm

' сохраняем настройки в RQdbini файле 
RQdbini.FileName=StartDir+"RQdb.ini"
RQdbini.Section="Projects"

PrjNameEdit.text=PrjMnu(PrjIndex).caption


'RQdbini.write("PrjPath",
RQdbini.write("PrjName"+str$(PrjIndex),PrjNameEdit.text)
RQdbini.write("PrjIni"+str$(PrjIndex),PrjNameEdit.text+".prj") 'MMPathEdit.text)
RQdbini.write("Index",str$(PrjIndex))


ProjectIni.FileName=PrjPathEdit.text+PrjNameEdit.text+".prj" '!!
'call  AddClrString ("7642:ProjectIni.FileName="+str$(ProjectIni.FileName), clred, LogEdit)

SrcFileName=MMPathEdit.text

'call  AddClrString ("7647: CreatePrjOnClick SrcFileName="+(SrcFileName), clred, LogEdit)

WindowsIndex=0
WindMnu(WindowsIndex).visible=1
WindMnu(WindowsIndex).checked=1
call  AddClrString ("9459:WindMnu("+str$(WindowsIndex)+"checked)="+str$(WindMnu(WindowsIndex).checked), clred, LogEdit)
WindMnu(WindowsIndex).Caption=str$(WindowsIndex)+ chr$(160)+SrcFileName

SavePrjFileOnClick
'RQDebug(BMarkMnu(0).Caption)
checkIni=1

end sub                                                              


'!***********************************************************************
SUB DeletePrjOnClick                                 
call  AddClrString ("DeletePrjOnClick  -----------------------------", clgrey, LogEdit)
call  AddClrString ("PrjIndex="+str$(PrjIndex), clred, LogEdit)

NewProjectIniFileName$=RQdbini.get("PrjIni"+str$(PrjIndex),"")
call  AddClrString ("10129:NewProjectiniFileName$="+(NewProjectiniFileName$), clp, LogEdit)

if instr(NewProjectiniFileName$,":")=0 then
	NewProjectiniFileName$=PrjPathEdit.text+RQdbini.get("PrjIni"+str$(PrjIndex),"")
end if

kill NewProjectiniFileName$

PrjMnu(PrjIndex).visible=0
PrjMnu(PrjIndex).caption=""

dec VisiblePrj

call clearWin_bm ' очищаем окна и закладки
PrjMnu(PrjIndex).checked=0

RQdbini.FileName=StartDir+"RQdb.ini"
'call  AddClrString ("RQdbini.FileName="+str$(RQdbini.FileName), cldblue, LogEdit)
RQdbini.Section="Projects"
RQdbini.write("PrjName"+str$(PrjIndex),"")
RQdbini.write("PrjIni"+str$(PrjIndex),"") 'MMPathEdit.text)
RQdbini.write("Index","0")

for i=0 to PrjItemCount-1 '!!! определяем новый индекс - любое непустое окно
	if PrjMnu(i).caption <> "" then  PrjIndex=i:exit for
next i

DelPrjFlg=1
'call  AddClrString ("PrjIndex="+str$(PrjIndex), clred, LogEdit)

call PrjChoose (PrjMnu(PrjIndex))

END SUB

'!******************************************************************************'
sub SavePrjFileOnClick

'SearchListForm.FormStyle=fsNormal 
SearchListForm.close

if EditFlg=0 then 
	' сохраняем содержимое SrcEdit
	'---- сохраняем параметры окна ------------------
	CmdParam(WindowsIndex)=CmdLineEdit.text
	CursWin(WindowsIndex)=SrcEdit.SelStart 'запоминаем положение курсора
	'Lastwind=WindowsIndex
	WindMnu(WindowsIndex).checked=0
	SubsListBox(WindowsIndex).visible=0
	Window(WindowsIndex)=SrcEdit.text
	'---- -----------------------------------------
else
	
end if


' заполняем поля как у текущего проекта
PrjNameEdit.text=PrjMnu(PrjIndex).caption
call  AddClrString ("9701:PrjNameEdit.text="+(PrjNameEdit.text), clb, LogEdit)

ProjectIni.FileName=PrjPathEdit.text+PrjNameEdit.text+".prj" '!!!! ?????
call  AddClrString ("10105:WindowsItemCount="+str$(WindowsItemCount), clo, LogEdit)

for i=0 to WindowsItemCount-1 ' проверяем открыт ли уже этот файл InsIdx
	'for i=InsIdx to WindowsItemCount-1 ' проверяем открыт ли уже этот файл InsIdx
	'call  AddClrString ("9812:WindMnu("+str$(i)+"caption)="+(WindMnu(i).caption), clred, LogEdit)
	filn$=field$(WindMnu(i).caption,chr$(160),2)
	
	if filn$="" then goto nexti5903: 
	
	if Modif(i)=1 then
		call  AddClrString ("9813:WindMnu("+str$(i)+")caption)="+(WindMnu(i).caption), cldg, LogEdit)
		filn$=field$(WindMnu(i).caption,chr$(160),2)
		call  AddClrString ("6144:filn$="+(filn$), clp, LogEdit)
		
		'IF FileExists(SrcFileName) > 0 THEN 
		IF FileExists(filn$) > 0 THEN 
			IF MessageBox("9878: Replace file "+chr$(10)+filn$ +" ?", "Warning! Saving changed file!", 1) = 1 THEN
			else
				goto nexti5903:   
			END IF
		END IF
		call AddClrString ( "9876: save filn$= "+filn$, cldgreen, LogEdit)   
		
		if SaveString( Window(i) ,filn$  )< 1 then 
			showmessage ("9888:(Save Project) Can't save source into "+filn$)
		else
			Modif(i)=0
			call  AddClrString ("9894:Modif("+str$(i)+")="+str$(Modif(i)), clo, LogEdit)
			if i=WindowsIndex then SrcEdit.Modified=Modif(i)
		end if
	end if
	nexti5903:
next i

IF MessageBox("9076: Save project properties in "+ProjectIni.FileName+"?", "Warning!", 1) = 1 THEN
	if ProjectIni.exist then kill ProjectIni.filename
	win$=""
	bmark$=""
	BMarkPo$=""
	numwin=0
	numbm=0
	CursWin(WindowsIndex)=SrcEdit.SelStart
	CmdParam(WindowsIndex)=CmdLineEdit.text
	
	'!!! сохраняем положения окон ----------------------------------------------------------
	ProjectIni.Section="WatchList"
	
	ProjectIni.write("Top",str$(WatchList.Top)  )
	ProjectIni.write("Left",str$(WatchList.Left))
	ProjectIni.write("Width",str$(WatchList.Width))
	ProjectIni.write("Height",str$(WatchList.Height))
	ProjectIni.write("TopM",str$(WLTopM))
	ProjectIni.write("LeftM",str$(WLLeftM))
	ProjectIni.write("Visible",str$(WatchList.Visible))
	ProjectIni.write("WindowState",str$(WatchList.WindowState))
	
	ProjectIni.Section="SubList"
	'ProjectIni.write("Top",str$(SubList.Top))
	'call  AddClrString ("5275 SavePrjFileOnClick SubList.Top="+str$(SubList.Top), clred, LogEdit)
	'ProjectIni.write("Left",str$(SubList.Left))
	'ProjectIni.write("Width",str$(SubList.Width))
	'ProjectIni.write("Height",str$(SubList.Height))
	'ProjectIni.write("TopM",str$(SLTopM))
	'ProjectIni.write("LeftM",str$(SLLeftM))
	'ProjectIni.write("Visible",str$(SubList.Visible))
	'ProjectIni.write("WindowState",str$(SubList.WindowState))
	
	ProjectIni.Section="ObjInspector"
	ProjectIni.write("Top",str$(ObjectInspector.Top)  )
	ProjectIni.write("Left",str$(ObjectInspector.Left))
	ProjectIni.write("Width",str$(ObjectInspector.Width))
	ProjectIni.write("Height",str$(ObjectInspector.Height))
	ProjectIni.write("TopM",str$(ObjInTopM))
	ProjectIni.write("LeftM",str$(ObjInLeftM))
	ProjectIni.write("Visible",str$(ObjectInspector.Visible))
	ProjectIni.write("WindowState",str$(ObjectInspector.WindowState))
	'ObjInTopM=80:ObjInLeftM=400:OnceFlgObj=0
	
	'SubList.WindowState=wsMinimized
	
	'!!! сохраняем настройки редактора ----------------------------------------------------
	
	RQdbini.Section="Editor"
	
	RQdbini.write("SyntaxHighLight",str$(SyntaxHLBox.checked))
	RQdbini.write("HotTabsHighLight",str$(HtHLBox.checked))
	'HLColor (0 TO 15)
	
	RQdbini.write("BackGroung",strl$(HLColor (0)))
	RQdbini.write("KeyWords",strl$(HLColor (1)))
	RQdbini.write("Operators",strl$(HLColor (2)))
	RQdbini.write("Directives",strl$(HLColor (3)))
	RQdbini.write("Properties",strl$(HLColor (4)))
	RQdbini.write("Types",strl$(HLColor (5)))
	RQdbini.write("Comments",strl$(HLColor (6)))
	RQdbini.write("Strings",strl$(HLColor (7)))
	RQdbini.write("Numbers",strl$(HLColor (8)))
	RQdbini.write("Text",strl$(HLColor (9)))
	RQdbini.write("GutterBG",strl$(HLColor (10)))
	RQdbini.write("GutterTxt",strl$(HLColor (11)))
	RQdbini.write("LogEditBG",strl$(HLColor (12)))
	RQdbini.write("Custom1",strl$(HLColor (13)))
	RQdbini.write("Custom2",str$(HLColor (14)))
	RQdbini.write("Custom3",str$(HLColor (14)))
	
	ProjectIni.Section="Editor"
	
	with ProjectIni
		.write("SyntaxHighLight",str$(SyntaxHLBox.checked))
		.write("HotTabsHighLight",str$(HtHLBox.checked))
		'HLColor (0 TO 15)
		
		.write("BackGroung",strl$(HLColor (0)))
		.write("KeyWords",strl$(HLColor (1)))
		.write("Operators",strl$(HLColor (2)))
		.write("Directives",strl$(HLColor (3)))
		.write("Properties",strl$(HLColor (4)))
		.write("Types",strl$(HLColor (5)))
		.write("Comments",strl$(HLColor (6)))
		.write("Strings",strl$(HLColor (7)))
		.write("Numbers",strl$(HLColor (8)))
		.write("Text",strl$(HLColor (9)))
		.write("GutterBG",strl$(HLColor (10)))
		.write("GutterTxt",strl$(HLColor (11)))
		.write("LogEditBG",strl$(HLColor (12)))
		.write("IncEditBG",strl$(HLColor (13)))
		.write("FileMngEditBG",strl$(HLColor (14)))
		
	end with
	'!!! сохраняем директории компилятора ----------------------------------------------------
	RQdbini.Section="Compiler"
	RQdbini.write("IDEPath",IDEPathEdit.text)
	call  AddClrString ("9443:IDEPathEdit.text="+(IDEPathEdit.text), clred, LogEdit)
	
	RQdbini.write("CompPath",CmpPathEdit.text)
	RQdbini.write("IncPath",IncPathEdit.text)
	RQdbini.write("LibPath",LibPathEdit.text)
	RQdbini.write("IcoFile",IcoFileEdit.text)
	RQdbini.write("TplFile",TplPathEdit.text)
	RQdbini.write("PrjPath",PrjPathEdit.text)
	call  AddClrString ("9451:PrjPathEdit.text="+(PrjPathEdit.text), clred, LogEdit)
	
	RQdbini.write("FBCompPath",FBCmpPathEdit.text)
	
	RQdbini.Section="Arduino"
	RQdbini.write("sketchbookPathEdit",sketchbookPathEdit.text)
	RQdbini.write("CliPathEdit",CliPathEdit.text)
	
	RQdbini.write("BuildPathEdit",BuildPathEdit.text)
	RQdbini.write("ArdLibraryPathEdit",ArdLibraryPathEdit.text)
	RQdbini.write("ArdLogFileEdit",ArdLogFileEdit.text)
	
	RQdbini.write("ArdWarningCmb",str$(ArdWarningCmb.text))
	RQdbini.write("ArdLogLevelCmb",str$(ArdLogLevelCmb.text))
	RQdbini.write("ArdCompilerMsg",str$(Combo2.ItemIndex))
	
	RQdbini.write("ArdCheckCode",str$(ChkBox4.checked))
	RQdbini.write("ArdSaveScetch",str$(ChkBox5.checked))
	RQdbini.write("ArdBoardMngLinks",Edit2.Text)
	
	
	'RQdbini.write("FBCompPath",qt+FBCmpPathEdit.text+QT)
	
	' сохраняем директории проекта ----------------------------------------------------
	'ProjectIni.Section="Project"
	'ProjectIni.write("PrjPath",PrjPathEdit.text)
	
	'!!! сохраняем Tab25 ----------------------------------------------------
	ProjectIni.Section="Tabs"
	ProjectIni.write("TabIndex",str$(Tab25.TabIndex)) 
	ProjectIni.write("TabCount",str$(Tab25.TabCount))
	
	'Tab25.TabIndex=Tab25.TabCount 
	for i=0 to TabListCmbox.ItemCount-1
		ProjectIni.write(str$(i),TabListCmbox.Item(i)) 
	next i
	
	
	'!!! сохраняем директории HotDirs в DirTree ----------------------------------------------------
	
	ProjectIni.Section="HotDirs"
	'HotDirsCount=7
	'Index=3
	ProjectIni.write("HotDirsCount",str$(CBoxHotDir.ItemCount))
	ProjectIni.write("Index",str$(CBoxHotDir.ItemIndex))
	
	for i=0 to CBoxHotDir.ItemCount-1
		ProjectIni.write(str$(i),CBoxHotDir.Item(i)) 
	next i
	
	'!!! сохраняем index текущего окна ----------------------------------------------------
	
	ProjectIni.Section="Windows" 
	ProjectIni.write("Index",str$(WindowsIndex))
	'call  AddClrString ("5333 SavePrjFileOnClick WindowsIndex="+str$(WindowsIndex), clred, LogEdit)
	
	'WindowsIndex=ProjectIni.get("Index","0")
	
	for wi=0 to WindowsItemCount-1 ' InsIdx
		if WindMnu(wi).caption <> "" then  
			
			if instr(WindMnu(wi).caption,":")=0 then ' если имя файла проекта не содержится полный путь, то
				
				win$=str$(Wi+1)+ chr$(160)+StartPath$+field$(WindMnu(Wi).caption,chr$(160),2)'WindMnu(wi).caption 
				
			else ' если полный путь
				win$=str$(Wi+1)+ chr$(160)+field$(WindMnu(Wi).caption,chr$(160),2)'WindMnu(wi).caption 
				win$=ReplaceSubStr$(win$,StartPath$,IdePath$) '!!! заменяем старый путь IDE на новый
				
				'call  AddClrString ("9523:win$="+(win$), cldr, LogEdit)
				
			end if
			
			IDEPathEdit.text=StartPath$
			
			'win$=StartPath$+win$
			'WindMnu(WindowsIndex).caption=str$(WindowsIndex)+ chr$(160)+SrcFileName
			'field$(WindMnu(WindowsIndex).caption,chr$(160),2)
			ProjectIni.Section="Windows" 
			ProjectIni.write("win"+str$(numwin),win$) ' окно 
			ProjectIni.write("curposic"+str$(numwin),str$(CursWin(Wi))) ' положение курсора
			ProjectIni.write("CmdLine"+str$(numwin),CmdParam(Wi)) ' окно 
			
			'win$=""
			numbm=0
			for wj=0 to BMarkItemCount-1 ' теперь сохраняем bookmarki
				if BMark(wj,wi) <> "" then 
					bmark$=BMark(wj,wi)-ht  '+str$(wi) +","str$(wj) ' текст закладки
					'call  AddClrString ("5349 bmark$="+(bmark$), clred, LogEdit)
					BMarkPo$=str$(BMarkPos (wj,wi))'+";" ' позиция закладки
					ProjectIni.Section="Bookmarks"
					'call  AddClrString ("5352 bmarkCaption"+str$(numwin)+","+str$(numbm)+"  "+qt+bmark$+qt, clred, LogEdit)
					
					ProjectIni.write("bmarkCaption"+str$(numwin)+","+str$(numbm),bmark$) 'qt+bmark$+qt
					'bmark$=""
					ProjectIni.write("bmarkPosic"+str$(numwin)+","+str$(numbm),BMarkPo$) 
					'BMarkPo$=""
					inc numbm
				end if
			next wj
			inc numwin           
		end if
	next wi
	
	call  AddClrString ("7907:SavePrjFileOnClick Ini file "+ProjectIni.FileName+"  saved", cldgreen, LogEdit)
	
else
	call  AddClrString ("7913: SavePrjFileOnClick Ini file "+ProjectIni.FileName+" not saved", clred, LogEdit)
end if    

'ClrSchComBoxList.Savetofile ("ClrSchComBox.lst")

RQdbini.Section="Color Schem"
RQdbini.write("Color Schem",ClrSchEdit.Text) 'текущая схема

RQdbini.Section=ClrSchEdit.Text
with RQdbini
	
	
	'=============
	.write("BackGroung",strl$(HLColor (0)))
	.write("KeyWords",strl$(HLColor (1)))
	.write("Operators",strl$(HLColor (2)))
	.write("Directives",strl$(HLColor (3)))
	.write("Properties",strl$(HLColor (4)))
	.write("Types",strl$(HLColor (5)))
	.write("Comments",strl$(HLColor (6)))
	.write("Strings",strl$(HLColor (7)))
	.write("Numbers",strl$(HLColor (8)))
	.write("Text",strl$(HLColor (9)))
	.write("GutterBG",strl$(HLColor (10)))
	.write("GutterTxt",strl$(HLColor (11)))
	.write("LogEditBG",strl$(HLColor (12)))
	.write("IncEditBG",strl$(HLColor (13)))
	.write("FileMngEditBG",strl$(HLColor (14)))
	'===========
end with


RQdbini.FileName=StartDir+"RQdb.ini"
RQdbini.Section="Projects"
RQdbini.write("Index",str$(PrjIndex))

RQdbini.write("PrjPath",PrjPathEdit.text)
RQdbini.write("PrjName"+str$(PrjIndex),PrjNameEdit.text)
RQdbini.write("PrjIni"+str$(PrjIndex),PrjNameEdit.text+".prj") 'MMPathEdit.text)



'sleep 1
'SearchListForm.FormStyle=fsStayOnTop 
end sub

'!************************************************************************'
sub Splitter2Moved
'Explorer.Width=RichEdit1.Width
'Explorer.Height=RichEdit1.Height
'Explorer.Top=RichEdit1.Top
'Explorer.Left=RichEdit1.Left
'form.repaint

end sub

'!************************************************************************'
sub SortBMark
'QUICKSORT(A(10), A(500), ASCEND) '-- sorts elements 10..500
'dim BMark(BMarkItemCount-1,WindowsItemCount-1) as string ' массив строк закладок
'dim BMarkPos(BMarkItemCount-1,WindowsItemCount-1) as integer ' массив положений курсора
'dim BMarkIndex(WindowsItemCount-1) as int ' номер текущей закладки
'BMarkMnu(BMarkItemCount-1)
dim CurWinBMark(BMarkItemCount-1) as string
dim CurWinBMarkPos(BMarkItemCount-1) as  integer
dim CurWinBMarkMnu(BMarkItemCount-1) as  string

for i=0 to BMarkItemCount-1
	CurWinBMark(i)=BMark(i,WindowsIndex) ' текст закладок
	CurWinBMarkPos(i)=BMarkPos(i,WindowsIndex) ' позиции 
	CurWinBMarkMnu(i)=BMarkMnu(i).caption ' текст меню
	'call  AddClrString ("CurWinBMark("+str$(i)+")="+CurWinBMark(i), clDPurple, LogEdit)   
	'call  AddClrString ("CurWinBMarkPos("+str$(i)+")="+str$(CurWinBMarkPos(i)), clDPurple, LogEdit)   
	
next i

QUICKSORT(CurWinBMark(0), CurWinBMark(BMarkItemCount-1), ASCEND)
QUICKSORT(CurWinBMarkPos(0), CurWinBMarkPos(BMarkItemCount-1), ASCEND)
QUICKSORT(CurWinBMarkMnu(0), CurWinBMarkMnu(BMarkItemCount-1), ASCEND)

for i=0 to BMarkItemCount-1
	BMark(i,WindowsIndex)=CurWinBMark(i)
	BMarkPos(i,WindowsIndex)=CurWinBMarkPos(i)
	BMarkMnu(i).caption=CurWinBMarkMnu(i)
	'call  AddClrString ("CurWinBMark("+str$(i)+")="+CurWinBMark(i), clDGreen, LogEdit)   
	'call  AddClrString ("CurWinBMarkPos("+str$(i)+")="+str$(CurWinBMarkPos(i)), clDPurple, LogEdit)   
	
next i

RefreshBMark
end sub

'!************************************************************************'
sub SetBMark
setbmf=1



call AddBMark

end sub            

'!************************************************************************'
SUB ChangeDirectory1 
'call  AddClrString ("ChangeDirectory1 DirTag="+str$(DirTag), clDPurple, LogEdit)   


if dirtag=0 then exit sub

'InitialDir=

'call  AddClrString ("8969:DirTree.Directory="+(DirTree.Directory), clb,LogEdit)
TruePath$=ConvertToTruePath(DirTree.Directory)
'call  AddClrString ("9831:TruePath$="+(TruePath$), clred, LogEdit)
'TruePath$=DirTree.Directory 'ConvertToTruePath(DirTree.Directory)

DirBox.text=TruePath$

select case DirTag
	
case IDEDirBtn.tag
	IDEPathEdit.text=TruePath$ 
	
	
case CompDirBtn.tag
	CmpPathEdit.text=TruePath$ 
case FBCompDirBtn.tag
	FBCmpPathEdit.text=TruePath$ 
case IncDirBtn.tag
	IncPathEdit.text=TruePath$ 
case LibDirBtn.tag
	LibPathEdit.text=TruePath$ 
case TPlDirBtn.tag
	TPlPathEdit.text=TruePath$ 
case PrjDirBtn.tag
	PrjPathEdit.text=TruePath$ 
	
case sketchbookPathBtn.tag
	sketchbookPathEdit.text=TruePath$ 
case BuildPathButn.tag
	BuildPathEdit.text=TruePath$ 
case ArdLibraryPathButn.tag
	ArdLibraryPathEdit.text=TruePath$ 
	
	
	
case 222 ' DirBtn
	call ChangeDirectory
case else
end select

print"8976: CURDIR$=";CURDIR$

end sub            

'!************************************************************************'
sub GoToLineError
NOPAINT=1
errChar=SendMessageAPI (SrcEdit.handle,EM_LINEINDEX,errlineNumb-1,0) 
'call AddFontString ( "errChar="+str$(errChar) , StrFontA, LogEdit)
SrcEdit.SelStart=len(SrcEdit.text)-1
SrcEdit.SelStart=errChar 
NOPAINT=0

SrcEdit.visible=1
LineErrChar=SendMessageAPI (MarginEdit.handle,EM_LINEINDEX,errlineNumb-1,0) 

'call AddclrString ( "GoToLineError 7047: LineErrChar="+str$(LineErrChar) , clblue, LogEdit)
MarginEdit.Line(errlineNumb-1)=MarginEdit.Line(errlineNumb-1)+">>"
MarginEdit.SelStart=LineErrChar
MarginEdit.SelLength=len(MarginEdit.Line(errlineNumb-1))
'call  AddClrString ("GoToLineError 7050:MarginEdit.SelLength="+str$(MarginEdit.SelLength), clred, LogEdit)
MarginEdit.SelAttributes.color=clred
MarginEdit.SelLength=0
MarginEdit.visible=0
MarginEdit.visible=1
MarginEdit.visible=0
MarginEdit.visible=1

'call  AddClrString ("GoToLineError 7054:MarginEdit.visible="+str$(MarginEdit.visible), clred, LogEdit)
end sub            

'!************************************************************************'
sub ChooseIcon
PrjPropert.FormStyle = fsNormal
OpenDialog.InitialDir=StartPath$
OpenDialog.FileName=IcoFileEdit.text
call  AddClrString ("7933:StartPath="+(StartPath$), clred, LogEdit)
OpenDialog.FilterIndex = 11
IF OpenDialog.Execute THEN 
	IcoFileEdit.text=OpenDialog.FileName
end if
PrjPropert.FormStyle = fsStayOnTop

end sub
'!***********************************************
sub LogValue
InpLine$=SrcEdit.Line(SrcEdit.WhereY)
InpPos=SrcEdit.SelStart-SrcEdit.LINEINDEX
ClrStr1$= ParseString (InpLine$, InpPos)
if SrcEdit.SelLength>0 then ClrStr$=SrcEdit.SelText else ClrStr$=ClrStr1$
lcsClrStr$=lcase$(ClrStr$)
if right$(lcsClrStr$ ,1)="$" or instr(lcsClrStr$,".text")>0 or instr(lcsClrStr$,".cell")_
or instr(lcsClrStr$,".caption")>0  or instr(lcsClrStr$,".line")>0 _
or instr(lcsClrStr$,"directory")>0 then vtp$="+(" else  vtp$="+str$("

' check indexes <<<<<<<<<<<<<
'MarginEdit.SelLength=len(MarginEdit.Line(errlineNumb-1))
Bbr=instr(ClrStr$,"(")
Ebr=instr(Bbr,ClrStr$,")")
Index$=mid$(ClrStr$,Bbr+1,Ebr-Bbr-1)
Lstr$=left$(ClrStr$,Bbr-1)
Rstr$=right$(ClrStr$,len(ClrStr$)-Ebr-1)
if Index$ <> "" then
	ClipBoard.Text=crlf+ "call  AddClrString (" +qt+ str$(SrcEdit.WhereY+1)+":"+Lstr$+"("+qt+ "+str$("+Index$+")+"+qt+Rstr$+")=" +qt +vtp$ +ClrStr$ +"), clred, LogEdit)"
else
	ClipBoard.Text=crlf+ "call  AddClrString (" +qt+ str$(SrcEdit.WhereY+1)+":" +ClrStr$ +"=" +qt +vtp$ +ClrStr$ +"), clred, LogEdit)"
end if

SrcEdit.GoToLineEnd
SrcEdit.PasteFromClipboard
end sub

'!************************************************************************'
sub PrintValue
ClrStr$=SrcEdit.Seltext
SrcEdit.SelLength=0
ClipBoard.Text=crlf+ "print" +qt +ClrStr$ +"=" +qt+";" +ClrStr$ 
SrcEdit.GoToLineEnd
SrcEdit.PasteFromClipboard

end sub
'!***********************************************************************
sub AddDeclaration (Sender as QMenuItem)
OldSearch$=SearchCBox.text
OldClipBrd$=clipboard.text

SearchCBox.text="'--- Declarations"

if Sender.tag=10 then
	ClrStr$=crlf+"Declare Sub "+SrcEdit.Seltext
	sfname$="Sub "+SrcEdit.Seltext
	sftype$="Sub "
else
	ClrStr$=crlf+"Declare Function "+SrcEdit.Seltext
	sfname$="Function "+SrcEdit.Seltext
	sftype$="Function "
end if
ClipBoard.Text=ClrStr$

SrcEdit.SelStart=0
SrcEdit.SelLength=0
call FindText (FindBtnDn)
'GoToLine (LineNumber)

SrcEdit.SelLength=0
SrcEdit.GoToLineEnd
SrcEdit.PasteFromClipboard
SrcEdit.SelStart=len(SrcEdit.Text)-1

star$="'!******************************************"
sr1$=" "
endd$="End "+sftype$

SrcEdit.AddStrings star$,sfname$,sr1$,endd$
SearchCBox.text=OldSearch$
clipboard.text=OldClipBrd$

END SUB

'!************************************************************************'
sub  FileLoad1  ' загрузка файла из selected text
if SrcEdit.SelLength>0 and trim$(SrcEdit.SelText)<>"" then 
	ClrStr$=SrcEdit.SelText 
else 
	exit sub
end if

if fileexists(ClrStr$)> 0 then
	
else
end if

if instr(ClrStr$, ".inc")>0 then filename$=""

end sub                                                              

'!************************************************************************'
sub SrcEditOnKeyDown (Key AS WORD, Shift AS INTEGER)
'call  AddClrString ("7104:Key="+str$(Key), clred, LogEdit)

if  key=13 then
	
	if KeyDowned=0 then 
		SrcEditWhereX=SrcEdit.WhereX
		OldClipBrd$=clipboard.text
		numtab0=tally(SrcEdit.Line(SrcEdit.WhereY-1),ht)
		numtab1=tally(SrcEdit.Line(SrcEdit.WhereY),ht)
		numtab2=tally(SrcEdit.Line(SrcEdit.WhereY+1),ht)
		txtleft$=left$(SrcEdit.Line(SrcEdit.WhereY),SrcEdit.WhereX)
		txtright$=right$(SrcEdit.Line(SrcEdit.WhereY),len(SrcEdit.Line(SrcEdit.WhereY))-SrcEdit.WhereX)
		numtabR=tally(txtright$,ht)
		'if numtabR=0 then ntab=numtab0 else ntab=numtabR
		clipboard.text=STRING$(numtabR, ht)
		SrcEdit.PasteFromClipboard
		lentxtr=len(txtright$-"ht")
		
		if lentxtr>0 then numtab=numtab1 else  numtab=numtab2
		clipboard.text=STRING$(numtab-numtabR, ht)
		KeyDowned=1
	else
		SrcEdit.PasteFromClipboard
	end if
elseif key =VK_DELETE then
	'if SrcEdit.WhereX=0 and (SrcEdit.Line(SrcEdit.WhereY)-ht)="" then
	if (SrcEdit.Line(SrcEdit.WhereY)-ht)="" then
		SrcEdit.SelStart=SrcEdit.LINEINDEX
		SrcEdit.SelLength=len(SrcEdit.Line(SrcEdit.WhereY))
		'SrcEdit.SelText=""
		
	end if
else
	
end if

end sub                                                              

'!************************************************************************'
sub SrcEditOnKeyUp (Key AS WORD, Shift AS INTEGER)

if  key=13  then
	'call  AddClrString ("7169:KeyDowned="+str$(KeyDowned), clb, LogEdit)
	if KeyDowned=1 then
		SrcEdit.PasteFromClipboard
		clipboard.text=OldClipBrd$
	elseif KeyDowned=2 then 
		clipboard.text=OldClipBrd$
	end if 
	'call  AddClrString ("6474: UP ------- clipboard.text="+(clipboard.text), clDG, LogEdit)
end if
KeyDowned=0

R2Change


'SrcEdit.visible=0
'SrcEdit.visible=1

end sub                                                              

'!************************************************************************'
sub   CreateFileOnClick
'---- -----------------------------------------
'- ищем первый от начала свободный номер
for i=0 to WindowsItemCount-1
	'call  AddClrString ("6614:WindMnu("+str$(i)+"caption)="+(WindMnu(i).caption), clred, LogEdit)
	if WindMnu(i).caption = "" then  
		WindowsFreeIndex=i
		exit for
	end if
next i
if i=WindowsItemCount then 
	showmessage ("Can't open more then "+str$(WindowsItemCount)+ " windows. Close one."):exit sub
else
end if

'---- сохраняем параметры окна ------------------
CmdParam(WindowsIndex)=CmdLineEdit.text
'call  AddClrString ("6582:WindowsIndex="+str$(WindowsIndex), clred, LogEdit)
CursWin(WindowsIndex)=SrcEdit.SelStart 'запоминаем положение курсора
Lastwind=WindowsIndex
WindMnu(WindowsIndex).checked=0
'LastWindMnu.Hint=WindMnu(WindowsIndex)
SubsListBox(WindowsIndex).visible=0
Window(WindowsIndex)=SrcEdit.text ' сохраняем содержание редактора в строковую переменную
WindMnu(WindowsIndex).Caption=str$(WindowsIndex)+ chr$(160)+SrcFileName

WindowsIndex=WindowsFreeIndex
'call  AddClrString ("6601:WindowsIndex="+str$(WindowsIndex), clred, LogEdit)

'"New Source"'Window(WindowsIndex)
SrcFileName=StartPath$+""+"NewWindow"+str$(WindowsIndex)+".bas"

StatusBar.Panel(5).caption =SrcFileName '"NewWindow.bas"
StatusPanel5.Caption=SrcFileName '"NewWindow.bas"

'WindMnu(WindowsIndex).caption="New Window"
WindMnu(WindowsIndex).Caption=chr$(149)+str$(WindowsIndex)+ chr$(160)+SrcFileName
WindMnu(WindowsIndex).visible=1

SrcEdit.text="" '"$INCLUDE "+qt+"RAPIDQ.INC"
OldClp$=Clipboard.text
Clipboard.text="$INCLUDE "+qt+"RAPIDQ.INC"+qt+crlf+"'--- Declarations ---"
'SrcEdit.AddStrings "'--- Declarations ---"
SrcEdit.PasteFromClipboard
SrcEdit.Modified=1
Modif(WindowsIndex)=1
Clipboard.text=OldClp$

Tab25AddTab


end sub                                                              

'!************************************************************************'
sub OpenStFiles (Sender as QMenuItem)
call  AddClrString ("8145:OpenStFiles="+str$(OpenStFiles), clb, LogEdit)

select case Sender.tag
case  105 ' Inc
	if direxists(IncPathEdit.text)>0 then OpenDialog.InitialDir=IncPathEdit.text
	'OpenDialog.Filter="*.inc|*.inc"
	OpenDialog.FilterIndex = 3 
	'call FileLoad
	
case  106 ' Template
	
	ViewTplFileMng 
	
	'if direxists(TplPathEdit.text)>0 then OpenDialog.InitialDir=TplPathEdit.text
	'call  AddClrString ("8201:TplPathEdit.text="+(TplPathEdit.text), clred, LogEdit)
	'OpenDialog.FilterIndex = 3 
	'FileLoad
	
case  107  'Project
	
	ViewPrjFileMng
	
case else
	'OpenDialog.InitialDir=StartDir
	OpenDialog.InitialDir=StripPATH(SrcFileName)
	OpenDialog.FilterIndex = 1
	'FileLoad
	
end select





end sub                                                              



'!************************************************************************'
sub ObjClick (Sender as QMenuItem)  '(Sender as QCOOLBTN)

Select case Sender.Tag
case 400 ' Qform
	'call  AddClrString ("TplPathEdit.text+Qform.tpl="+(TplPathEdit.text+"Qform.tpl"), clred, LogEdit)
	
	ClipBoard.Text=LoadString (TplPathEdit.text+"Qform.tpl")
	SrcEdit.PasteFromClipboard
	
case 401 '"QLABEL"
	ClipBoard.Text=LoadString (TplPathEdit.text+"QLABEL.tpl")
	SrcEdit.PasteFromClipboard
	
case 402 'QEdit
	ClipBoard.Text=LoadString (TplPathEdit.text+"QEdit.tpl")
	SrcEdit.PasteFromClipboard
	
case 403 '"QButton"
	ClipBoard.Text=LoadString (TplPathEdit.text+"QBUTTON.tpl")
	SrcEdit.PasteFromClipboard
	
case 404'QCOOLBTN
	ClipBoard.Text=LoadString (TplPathEdit.text+"QMenuItem.tpl")
	SrcEdit.PasteFromClipboard
	
case 405 'QOvalBtn
	ClipBoard.Text=LoadString (TplPathEdit.text+"QOvalBtn.tpl")
	SrcEdit.PasteFromClipboard
	
case 406 'QCheckBox
	ClipBoard.Text=LoadString (TplPathEdit.text+"QCheckBox.tpl")
	SrcEdit.PasteFromClipboard
	
case 407 'QRadioBtn
	ClipBoard.Text=LoadString (TplPathEdit.text+"QRadioBtn.tpl")
	SrcEdit.PasteFromClipboard
	
case 408 'QComboBox
	ClipBoard.Text=LoadString (TplPathEdit.text+"QComboBox.tpl")
	SrcEdit.PasteFromClipboard
	
case 409 'QListBox
	ClipBoard.Text=LoadString (TplPathEdit.text+"QListBox.tpl")
	SrcEdit.PasteFromClipboard
	
case 410 'QPanel
	ClipBoard.Text=LoadString (TplPathEdit.text+"QPanel.tpl")
	SrcEdit.PasteFromClipboard
	
case 411 'QImage
	ClipBoard.Text=LoadString (TplPathEdit.text+"QImage.tpl")
	SrcEdit.PasteFromClipboard
	
case 412 'QGroupBox
	ClipBoard.Text=LoadString (TplPathEdit.text+"QGroupBox.tpl")
	SrcEdit.PasteFromClipboard
	
case 413 'QScrollBox
	ClipBoard.Text=LoadString (TplPathEdit.text+"QScrollBox.tpl")
	SrcEdit.PasteFromClipboard
case 414 'QRichEdit
	ClipBoard.Text=LoadString (TplPathEdit.text+"QRichEdit.tpl")
	SrcEdit.PasteFromClipboard
	
case 415 'QStringGrid
	ClipBoard.Text=LoadString (TplPathEdit.text+"QStringGrid.tpl")
	SrcEdit.PasteFromClipboard
	
case 416 'QTabControl
	ClipBoard.Text=LoadString (TplPathEdit.text+"QTabControl.tpl")
	SrcEdit.PasteFromClipboard
	
case 417 'QCanvas
	ClipBoard.Text=LoadString (TplPathEdit.text+"QCanvas.tpl")
	SrcEdit.PasteFromClipboard
	
case 418 'QStatusBar
	ClipBoard.Text=LoadString (TplPathEdit.text+"QStatusBar.tpl")
	SrcEdit.PasteFromClipboard
	
case 419 'QScrollBar
	ClipBoard.Text=LoadString (TplPathEdit.text+"QScrollBar.tpl")
	SrcEdit.PasteFromClipboard
	
case 420 'QTrackBar
	ClipBoard.Text=LoadString (TplPathEdit.text+"QTrackBar.tpl")
	SrcEdit.PasteFromClipboard
	
case 421 'QFileListBox
	ClipBoard.Text=LoadString (TplPathEdit.text+"QFileListBox.tpl")
	SrcEdit.PasteFromClipboard
	
case 422 'QGauge
	ClipBoard.Text=LoadString (TplPathEdit.text+"QGauge.tpl")
	SrcEdit.PasteFromClipboard
	
case 423 'QDirTree
	ClipBoard.Text=LoadString (TplPathEdit.text+"QDirTree.tpl")
	SrcEdit.PasteFromClipboard
	
case 424 'QListView
	ClipBoard.Text=LoadString (TplPathEdit.text+"QListView.tpl")
	SrcEdit.PasteFromClipboard
	
case 425 'QBitmap
	ClipBoard.Text=LoadString (TplPathEdit.text+"QBitmap.tpl")
	SrcEdit.PasteFromClipboard
	
case 426 'QOutline
	ClipBoard.Text=LoadString (TplPathEdit.text+"QOutline.tpl")
	SrcEdit.PasteFromClipboard
	
case 427 '
case 428 '
case 429 '
case else
end select

end sub                                                              

'!***********************************************************************
SUB MainModulePathClick (Sender AS QCOOLBTN)
'-- Enter your code here
OpenDialog.FilterIndex = 1

IF OpenDialog.Execute THEN
	MMpathEdit.text=OpenDialog.FileName
end if

END SUB

'!***********************************************************************
sub clearWin_bm

'очищаем наборы окон и закладок
'SrcFileName=MMPathEdit.text
'call  AddClrString ("5494 SrcFileName="+SrcFileName, clDpurple, LogEdit)

for i=0 to WindowsItemCount-1
	WindMnu(i).visible=0
	WindMnu(i).checked=0
	WindMnu(i).Caption=""
	CursWin(i)=1
	Modif(i)=0
	Window(i)=""
	BMarkIndex(I)=-1
	BMarkFreeIndex (i)=0
	LastBMarkIdx(i)=0
	VisibleBMarkCnt(i)=0
	for j=0 to BMarkItemCount-1
		BMark(j,i)=""
		BMarkPos(j,i)=1
		
	next j
next i
WindowsIndex =0
WindowsFreeIndex=1

for j=0 to BMarkItemCount-1
	BMarkMnu(j).Caption=""
	BMarkMnu(j).checked=0
	BMarkMnu(j).visible=0
next j


END SUB

'!***********************************************************************
sub ReplCheckClick              
if ReplCheckBox.checked=1 then 
	ReplaceComBox.enabled=1
	ReplaceComBox.Color=clwhite
	
	ReplaceBtn.enabled=1
	ReplaceSkipBtn.enabled=1
	ReplaceAllBtn.enabled=1
	
else
	ReplaceComBox.enabled=0
	ReplaceComBox.Color=clgrey
	ReplaceBtn.enabled=0
	ReplaceSkipBtn.enabled=0
	ReplaceAllBtn.enabled=0
	
end if
END SUB

'!***********************************************************************
sub GoToLine (LineNumber as int)
'call  AddClrString ("6490:LineNumber="+str$(LineNumber), clred, LogEdit)

'if LineNumber=0 then LineNumber=val(SearchCBox.text)
'call  AddClrString ("6492:LineNumber="+str$(LineNumber), clred, LogEdit)

LChar=SendMessageAPI (SrcEdit.handle,EM_LINEINDEX,LineNumber-1,0) 
SrcEdit.SelStart=len(SrcEdit.Text )-2
SrcEdit.SelStart=lChar
SrcEdit.SelLength=0
END SUB


'!******************************************
Sub SavePrjAsOnClick
'---- сохраняем параметры текущего проекта ------------------
call  AddClrString ("8503: Save cur prj ="+(PrjMnu(PrjIndex).caption), clred, LogEdit)
LastPrj=PrjIndex
call  AddClrString ("7628: SavePrjAsOnClick LastPrj="+str$(LastPrj), clred, LogEdit)

PrjMnu(PrjIndex).checked=0

'LastWindMnu.Hint=WindMnu(WindowsIndex)
SubsListBox(WindowsIndex).visible=0

' сохраняем файлы текущего проекта
for i=0 to WindowsItemCount-1 
	if instr(WindMnu(i).caption,chr$(149) )>0 then
		SrcFileName1$=field$(WindMnu(i).caption,chr$(160),2):
		'call  AddClrString ("CreatePrjOnClick Save source into "+SrcFileName1$, clred, LogEdit)
		
		if SaveString(Window(I) ,SrcFileName1$)< 1 then 
			showmessage ("Can't save source into "+SrcFileName1$)
		end if
	end if
next i

' создаем новый проект ------------------------------
' заполняем поля как у текущего проекта
PrjNameEdit.text=PrjMnu(PrjIndex).caption 'SrcFileName
call  AddClrString ("8522:PrjMnu("+str$(PrjIndex)+"caption)="+(PrjMnu(PrjIndex).caption), clb, LogEdit)
MMpathEdit.text=SrcFileName'SrcFilePath$
call  AddClrString ("8528:MMpathEdit.text="+(MMpathEdit.text), cldb, LogEdit)


NewProjectBox.showmodal '!!! SavePrjAsOnClick
'PrjNAME(PrjIndex)

'- ищем первый от начала свободный номер
for i=0 to PrjItemCount-1
	'call AddClrString ( "WindMnu("+str$(i)+").caption= "+WindMnu(i).caption, clBlack, LogEdit)   
	if PrjMnu(i).caption = "" then  
		PrjFreeIndex=i
		'call AddClrString ( "ищем первый от начала свободный номер WindowsFreeIndex= "+str$(WindowsFreeIndex), clred, LogEdit)   
		exit for
	end if
next i

if i=PrjItemCount then 
	showmessage ("Can't open more then "+str$(WindowsItemCount)+ " windows. Close one."):exit sub
else
end if

' создаем меню
PrjIndex=PrjFreeIndex
'PrjMnu(PrjIndex).caption=str$(PrjIndex)+chr$(160)+PrjNameEdit.text
PrjMnu(PrjIndex).caption=PrjNameEdit.text
PrjMnu(PrjIndex).visible=1
PrjMnu(PrjIndex).checked=1

' сохраняем настройки в prj файле
RQdbini.FileName=StartDir+"RQdb.ini"
RQdbini.Section="Projects"
RQdbini.write("PrjPath",PrjPathEdit.text)
RQdbini.write("PrjName"+str$(PrjIndex),PrjNameEdit.text)
RQdbini.write("PrjIni"+str$(PrjIndex),PrjNameEdit.text+".prj") 'MMPathEdit.text)
RQdbini.write("Index",str$(PrjIndex))


ProjectIni.FileName=PrjPathEdit.text+PrjNameEdit.text+".prj" '+"\"
call  AddClrString ("5730 CreatePrjOnClick RQdbini.FileName="+(ProjectIni.FileName), clred, LogEdit)

SavePrjFileOnClick

checkIni=1

End Sub                                                                                                                                                         
'!******************************************
Sub FBCompileOnClick
'call  AddClrString ("FBCompileOnClick===================="+, clo, LogEdit)
lcext$=lcase$(StripFileExt (SrcFileName))
if lcext$ <> ".bas" and lcext$ <> ".rqb" and lcext$ <> ".rq" then
	IF MessageDlg("Sourcefile is not basic file: "+SrcFileName+" Save it as "+SrcFileName+".bas ?", mtWarning, mbYes OR mbNo, 0) = mbYes THEN
		
		SaveAsOnClick
	else
		ShowMessage ("10454: Sourcefile not compiled: "+SrcFileName)
		EXIT Sub
		
	end if
	
else
end if

ErrStatusBar.visible=0
call AddClrString ( "save SrcFileName= "+SrcFileName, clred, LogEdit)   
rz =SaveString(SrcEdit.text ,SrcFileName  )
if rz=0 then showmessage ("Can't save source into "+SrcFileName):exit sub
Modiflg=0
SrcEdit.Modified=0


'dbglbl:

'CHDIR  FBCmpPathEdit.text'startdir
SrcDir$=StripPath(SrcFileName)
call  AddClrString ("6714:SrcDir$="+(SrcDir$), clred, LogEdit)
CHDIR SrcDir$

сall AddClrString ( "compile SrcFileName= "+SrcFileName, cldblue, LogEdit)   
SrcFileNameQT$=qt+SrcFileName+qt 

if ddlflg=1 then 
	dllflg=0
	ExeFileName$=FullPathNoExt(SrcFileNameQT$)+".exe"+qt  :
	'ExeFileName$=""
else
	ExeFileName$=FullPathNoExt(SrcFileNameQT$)+".exe"+qt  :
end if


ExeFileName1$=""
call AddClrString ( "6933: ExeFileName$= "+ExeFileName$, cldred, LogEdit)   


FBCmpPathEdit.text=replacesubstr$(FBCmpPathEdit.text+"\","\\","\")
call  AddClrString ("7836:FBCmpPathEdit.text="+(FBCmpPathEdit.text), clred, LogEdit)

FBcmpFileName$=qt+FBCmpPathEdit.text+"fbc.exe"+qt '' : print "cmpFileName$=" ,cmpFileName$
call AddClrString ( "6936: FBcmpFileName$= "+FBcmpFileName$, cldred, LogEdit)   

msgFile$=qt+FBCmpPathEdit.text+"fb.msg"+qt: ''  print "msgFile$=" ,msgFile$
call AddClrString ( "6938: msgFile$= "+msgFile$, cldred, LogEdit)   

FBCompileStr$=FBcmpFileName$'+_

if FBCmpVerEdit.text="0.18" then 
	cmd_o$="-o"
else
	cmd_o$=""
end if

FBCompileParStr$=" "+FBCompParam$+_ 
" "+ExeFileName1$+_
" "+SrcFileNameQT$+_ 
" >"+msgFile$

call  AddClrString ("6951: FBCompileStr$="+(FBCompileStr$), clb, LogEdit)
call  AddClrString ("6952: FBCompileParStr$="+(FBCompileParStr$), clM, LogEdit)

batFileName$=FBCmpPathEdit.text+"FB.bat"
call  AddClrString ("7888:batFileName$="+(batFileName$), cldb, LogEdit)

zz=SaveString(FBCompileStr$+FBCompileParStr$,batFileName$)
if zz=0 then
	call  AddClrString ("6959: Can't create file "+batFileName$, clr, LogEdit)
elseif zz=-1 then
	call  AddClrString ("6961: Can't write file "+batFileName$, clr, LogEdit)
elseif zz=1 then 
else
end if

if fileexists(batFileName$)=0 then
	ShowMessage "6967:Can't create compiler bat file "+batFileName$
	call  AddClrString ("6967:Can't create compiler bat file "+batFileName$, clred, LogEdit)
	exit sub
end if

PID=SHELL ( qt+FBCmpPathEdit.text+"FB.bat"+qt,1)
'PID=SHELL (FBCompileStr$+FBCompileParStr$,1)

'PID=ShellExecute (LogEdit.handle,"",FBCompileStr$,"","",SW_SHOWNORMAL)
'PID=ShellExecute (RQForm.handle,"open",FBCompileStr$,FBCompileParStr$,"",0)

call AddClrString ( "9903: compile PID= "+str$(PID), clblack, LogEdit)   

if PID=0 then 
	ShowMessage "6964: Can't run compiler from bat-file "+batFileName$
	exit sub
end if

msgFile$=msgFile$-qt

call  AddClrString ("10792:WaitForSingleObject=", clm, LogEdit)
WaitForSingleObject (PID, 5000)
call  AddClrString ("9918:msgFile$="+(msgFile$), clred, LogEdit)

'sleep 1

'C:\_F\bas\rapidq\freebasic\FreeBasic16\fb.msg
'C:\_F\BAS\RAPIDQ\FREEBASIC\FREEBASIC16\fb.msg 
'msgFile$="C:\_F\bas\rapidq\freebasic\FreeBasic16\fb.msg"
'call  AddClrString ("9918:msgFile$="+(msgFile$), clred, LogEdit)

'brem 0
'==========


if fileexists(msgFile$)>0 then
	'rezcmp$=LoadString(msgFile$)
	MsgList.LoadFromFile(msgFile$) 
	
else
	
	ShowMessage ("10570: File not found "+msgFile$)
	
end if 
'erem 

call AddClrString ( "9913: fb.msg MsgList.Text="+MsgList.Text, clDgreen, LogEdit)   

'StrFontA.Name="FixedSys"
StrFontA.Color=clBlue
'call AddFontString ( "------------------compiler message -------------- "+, StrFontA, LogEdit)   
LogEdit.HideSelection=1
for i=0 to MsgList.ItemCount-1
	if instr(MsgList.Item(i),"error")>0 or instr(MsgList.Item(i),"Expected")>0 or instr(MsgList.Item(i),"WARNING")>0 then '
		LogEdit.HideSelection=1
		if instr(MsgList.Item(i),"error")>0 or instr(MsgList.Item(i),"WARNING")>0 then
			
			lsk=instr(MsgList.Item(i),"(")
			rsk=instr(MsgList.Item(i),")")
			lenl=rsk-lsk
			errlineNumb=val( mid$(MsgList.Item(i), lsk+1 , lenl  )  )
		elseif instr(MsgList.Item(i),"Expected")>0 then
			'Line 11, 4: Expected = but got "/"
			errlineNumb=val(field$(MsgList.Item(i),",",1)-"Line ")
		else
		end if
		ErrStatusBar.visible=1
		PanelErr.caption=MsgList.Item(i)
		'call AddFontString ( "errlineNumb="+str$(errlineNumb) , StrFontA, LogEdit)
		StrFontA.Color=clRed
		'call AddFontString ( MsgList.Item(i), StrFontA, LogEdit)   
		StrFontA.Color=clBlue
		' переходим на строку с ошибкой
		GoToLineError
		exit sub
	else
		call AddFontString ( MsgList.Item(i)+, StrFontA, LogEdit)   
	end if 
next i

StrFontA.Color=clDBlue
StrFontA.Name="Arial"

if FBCompParam$=" -dll " then exit sub

call AddFontString ( "run ExeFileName$= "+ExeFileName$, StrFontA, LogEdit)   

'if fileexists(ExeFileName$)=0 then
'ShowMessage "File not found "+ ExeFileName$
'call AddClrString ( "File not found "+ ExeFileName$, clr, LogEdit)   
'end if
call  AddClrString ("10548:CmdLineEdit.text="+(CmdLineEdit.text), clred, LogEdit)

PID = SHELL(ExeFileName$ +" "+CmdLineEdit.text, 1) '+"> CmpOutput.txt"
call AddClrString ( "run PID= "+str$(PID), clred, LogEdit)   
LogEdit.HideSelection=0


End Sub 
'!******************************************
Sub FBExeOnClick

FBCompParam$="-i "+qt+FBCmpPathEdit.text+"inc"+qt
FBCompileOnClick

End Sub  

'!******************************************
Sub FBDllOnClick
ddlflg=1
FBCompParam$=" -dll "
FBCompileOnClick 
End Sub                                                       

'!******************************************
Sub CMDLParamOnClick
CmdLineForm.showmodal
End Sub       
'!******************************************
Sub PasteCurFileNameOnClick
OldClipBrd$=ClipBoard.text 
ClipBoard.text=SrcFileName
SrcEdit.PasteFromClipboard
ClipBoard.text=OldClipBrd$

End Sub 
'!******************************************
Sub PasteDateOnClick
OldClipBrd$=ClipBoard.text 
ClipBoard.text=field$(Date$,"-",2)+"-"+field$(Date$,"-",1)+"-"+field$(Date$,"-",3)

SrcEdit.PasteFromClipboard
ClipBoard.text=OldClipBrd$
End Sub        
'!******************************************
Sub VerifyEventOnClick
dim eventlist as qstringlist
dim srclist as qstringlist


call  AddClrString ("7761: refresh sublist...", clred, LogEdit)
call SubsListOnClick

srclist.text=srcedit.text
srclist.text=lcase$(srclist.text)

eventlist.loadfromfile(StartDir+"events.dat")
eventlist.text=lcase$(eventlist.text)
'call  AddClrString ("7742:eventlist.text="+(eventlist.text), clred, LogEdit)
'call  AddClrString ("7744:eventlist.itemcount="+str$(eventlist.itemcount), clred, LogEdit)
'call  AddClrString ("7747:srclist.itemcount="+str$(srclist.itemcount), clred, LogEdit)

Gauge1.visible=1
Gauge1.position=5
Gauge1.forecolor=clo
handlerpresentsGlob=1

for i=0 to eventlist.itemcount-1
	handlerpresents=1
	event$=eventlist.item(i)
	'call  AddClrString ("7749:event$="+(event$), clm, LogEdit)
	for j=0 to srclist.itemcount-1
		srclistitem$=srclist.item(j)
		'call DelComments (@srclistitem$)
		if instr(srclistitem$,event$)>0 then 
			if GetVarPos(srclistitem$,event$)>0 and instr(srclistitem$,"=")>0 then
				'call  AddClrString ("7753:srclist.item("+str$(j)+")="+(srclist.item(j)), clred, LogEdit)
				eventhadler$=(field$(srclistitem$,"=",2))
				if instr(eventhadler$,"'" )>0 then call DelComments (@eventhadler$)
				eventhadler$=trim$(eventhadler$)
				call  AddClrString ("7753:eventhadler$="+(eventhadler$), clb, LogEdit)
				' ищем его в списке процедур
				handlerpresents=0 ' взводим флаг 
				SlIC=SubsListBox(WindowsIndex).ItemCount
				
				for isub=0 to SlIC-1
					
					'call  AddClrString ("7937:SubsListBox("+str$(WindowsIndex)+"Item(isub).Caption)="+(SubsListBox(WindowsIndex).Item(isub).Caption), clred, LogEdit)
					
					'if instr(SubsListBox(WindowsIndex).Item(isub).Caption,eventhadler$)>0 then
					subh$=lcase$(SubsListBox(WindowsIndex).Item(isub).Caption)
					subh$=(field$(subh$,"sub ",2))
					'call  AddClrString ("7941:subh$="+(subh$), clb, LogEdit)
					subh$=trim$((subh$))-ht
					subh$=(field$(subh$," ",1))
					'call  AddClrString ("7941:subh$="+(subh$), cldr, LogEdit)
					subh$=(field$(subh$,"'",1))
					'if instr(subh$,"(")>0 then subh$=(field$(subh$,"(",1))
					subh$=(field$(subh$,"(",1))
					subh$=trim$((subh$))
					'call  AddClrString ("7941:subh$="+(subh$), cldg, LogEdit)
					'subh$=trim$(lcase(SubsListBox(WindowsIndex).Item(isub).Caption))-ht
					evh$=trim$(lcase$(eventhadler$))-ht
					'call  AddClrString ("7952:evh$="+(evh$), cldp, LogEdit)
					if subh$=evh$ then
						handlerpresents=1
						exit for
					end if
					doevents
				next isub 
				if handlerpresents=0 then
					call  AddClrString ("!!! Not found eventhadler$ !!! 7805:srclist.item("+str$(j)+")="+(srclist.item(j)), clp, LogEdit)
					handlerpresentsGlob=0
				end if
			end if
		end if
	next j
	
	Gauge1.position=i/(eventlist.ItemCount+1)*100
	
next i
Gauge1.visible=0
if handlerpresentsGlob=1 then
	call  AddClrString ("7819:Verify Event handlers - OK", cldg , LogEdit)
end if
End Sub


'!******************************************
Sub IncFilesChoose (Sender as QMenuItem)
'if SrcEdit.color=&HE2ECB8 then  goto nosrc
BMarkMain.enabled=0

if EditFlg<>0 then goto nosrc 'если был inc файл то его параметры не запоминаем

'---- сохраняем параметры окна ------------------
CmdParam(WindowsIndex)=CmdLineEdit.text
CursWin(WindowsIndex)=SrcEdit.SelStart 'запоминаем положение курсора
Lastwind=WindowsIndex
SubsListBox(WindowsIndex).visible=0
Window(WindowsIndex)=SrcEdit.text
Modif(WindowsIndex)=SrcEdit.Modified
LastWindMnu.Enabled=1
nosrc:

With sender
	' пытаемся загрузить файл из текущей директории
	
	If fileexists(IncFilesMnu(.MenuIndex).caption)>0 then
		SrcEdit.loadfromFile(IncFilesMnu(.MenuIndex).caption)
		StatusPanel5.Caption=IncFilesMnu(.MenuIndex).caption
	elseif fileexists(IncPathEdit.text+IncFilesMnu(.MenuIndex).caption)>0 then
		SrcEdit.loadfromFile(IncPathEdit.text+IncFilesMnu(.MenuIndex).caption)
		StatusPanel5.Caption=(IncPathEdit.text+IncFilesMnu(.MenuIndex).caption)
		'call  AddClrString ("6384:IncPathEdit.text+IncFilesMnu("+str$(.MenuIndex)+"caption)="+(IncPathEdit.text+IncFilesMnu(.MenuIndex).caption), clred, LogEdit)
	else
	end if
	SrcEdit.color=HLColor(13) ' &HE2ECf8
	BorderRich.color=srcedit.color
	EditFlg=2
	SrcFileName=StatusPanel5.Caption
	
end with
End Sub           
'!******************************************
Sub RqTplChange
Clipboard.Text=RapidQTplItem$(TplListBox.ItemIndex)
SrcEdit.PasteFromClipboard
TplListBox.Clear
call LoadRqTpl
TplListBox.text="Code templates"
End Sub         

'!******************************************
Sub LoadRqTpl
'-----------------------------
if TplPathEdit.text[len(TplPathEdit.text)]<>"\" then TplPathEdit.text=TplPathEdit.text+"\"
RapidQTpl$=LoadString(TplPathEdit.text+"RapidQ.tpl")
TplCount=tally(RapidQTpl$,"<!---")

redim RapidQTplItem$(TplCount\2) as string

TplListBoxItems$=""

for i=2 to TplCount step 2
	TplListBoxItems$=field$(RapidQTpl$,"<!---",i)-cr-lf
	'call  AddClrString ("2449:i*2+1="+str$(i*2+1), clB, LogEdit)
	'call  AddClrString ("2443:TplListBoxItems$="+(TplListBoxItems$), clred, LogEdit)
	TplListBox.AddItems TplListBoxItems$
next i

for i=1 to TplCount/2 'step 2
	RapidQTplItem$(i-1)=field$(RapidQTpl$,"<!---",i*2+1)
	'call  AddClrString ("2448:RapidQTplItem$("+str$(i-1)+")="+(RapidQTplItem$(i-1)), clred, LogEdit)
next i
'--------------------------
TplListBox.text="Code templates"
End Sub 

'!******************************************
Sub ClearBMark

End Sub      
'!******************************************
Sub ListChBoxClick


if ListOfSearchChBox.checked=1 then
	'ObjTreePanel.visible=0
	SearchListForm.Show
else
	SearchListForm.Close
	'ObjTreePanel.visible=0
	
end if

End Sub       
'!******************************************
Sub ListSelectCell(Col% , Row%, CanSelect%, Sender AS QStringGrid)  
LineN=val(SEarchListGrid.cell(0,Row%))
'call  AddClrString ("11235:LineN="+str$(LineN), clred, LogEdit)
SrcEdit.selstart=SrcEdit.GetLineIdx(SrcEdit.LineCount-1)
SrcEdit.selstart=SrcEdit.GetLineIdx(LineN-1)

End Sub  

'!******************************************
Sub ViewTplFileMng 'Sender as QMenuItem)
DirBox.text=TplPathEdit.text
if direxists(DirBox.text)=0 then DirBox.text=StartPath$ '"c:\"
FileListBox1.Directory=DirBox.text
MaskBox.Text="*.tpl"
FileListBox1.mask=MaskBox.Text

FilePanel.visible=1
FileListPanel.visible=1

end sub

'!******************************************
Sub ViewPrjFileMng 'Sender as QMenuItem)
DirBox.text=PrjPathEdit.text
if direxists(DirBox.text)=0 then DirBox.text=StartPath$ '"c:\"
FileListBox1.Directory=DirBox.text
MaskBox.Text="*.prj"
FileListBox1.mask=MaskBox.Text

FilePanel.visible=1
FileListPanel.visible=1

end sub


'!******************************************
Sub ViewFileMng 'Sender as QMenuItem)
DirBox.text=StripPATH(SrcFileName)
if direxists(DirBox.text)=0 then DirBox.text=StartPath$'"c:\"
FileListBox1.Directory=DirBox.text


MaskBox.Text="*"+StripFileExt(SrcFileName) '"*.tpl"
FileListBox1.mask=MaskBox.Text


FilePanel.visible=1
FileListPanel.visible=1

exit sub '??? !!! ----------------------------


' сохраняем текущее окно
if FileMng.checked=0 then 
	'---- сохраняем параметры окна ------------------
	CmdParam(WindowsIndex)=CmdLineEdit.text
	CursWin(WindowsIndex)=SrcEdit.SelStart 'запоминаем положение курсора
	'Lastwind=WindowsIndex
	WindMnu(WindowsIndex).checked=0
	'LastWindMnu.Hint=WindMnu(WindowsIndex)
	SubsListBox(WindowsIndex).visible=0
	Window(WindowsIndex)=SrcEdit.text
	Modif(WindowsIndex)=SrcEdit.Modified
	ErrStatusBar.visible=0
	
	FileMng.checked=1
	FileMngBtn.down=1'true
	FilePanel.visible=1
	FileListPanel.visible=1
else
	'FilePanel.visible=0
	'FileListPanel.visible=0
	FileMngBtn.down=false
	if FMngOpenFlg=1 then 
		LastWindClick:FMngOpenFlg=0 ' !!!!! его надо открывать только если было открыто окно через файл манагер
	end if
	FileMng.checked=0
end if
doevents
DirTree.InitialDir =StartDir ' curdir1$

End Sub 
'!************************************************************************'
sub OnDirPopUp

if instr(FileListBox1.FileName , ".") >0 then
	ext=FileListBox1.FileName-left$(FileListBox1.FileName, instr(FileListBox1.FileName , "."))
	ext="*."+lcase$(ext)
else
end if
AddExt.Caption="choose extention "+ext
end sub
'!************************************************************************'
sub AddExtOnClick
if ext="" then exit sub
MaskBox.Text=ext
FileListBox1.mask=ext

for i=0 to MaskBox.ItemCount-1
	if MaskBox.Item(i)= ext then exit sub
next

MaskBox.AddItems ext

end sub

'!************************************************************************'
sub FileOnlyOnClick
if FileListBox1.FileName="" then exit sub
rins=rinstr(FileListBox1.FileName,"\")+1
lens=len(FileListBox1.FileName)-rins+1

MaskBox.Text=mid$(FileListBox1.FileName,rins,lens)
FileListBox1.mask=MaskBox.Text'FileListBox1.FileName

end sub
'!************************************************************************'
sub RefreshDirTree
DirTree.Reload
'ChangeDirectory1 
end sub
'!************************************************************************'
SUB Check1
call  AddClrString ("11442:Check1="+str$(Check1), clred, LogEdit)
FileListBox1.mask=MaskBox.Text

if direxists(DirBox.text)>0 then
	FileListBox1.Directory=DirBox.text
else
	DirTree.Reload
end if

if direxists(DirBox.text)>0 then
	FileListBox1.Directory=DirBox.text
else
	Showmessage "10964: Directory does not exists "+DirBox.text
end if


'MaskBoxHdl=getfocus()
END SUB
'!************************************************************************'
sub FileListBox1OnChange (Sender as QFileListBox)
if SubDirCheckbox.Checked=0 then  NumFilesT=FileListBox1.ItemCount 
end sub

'!************************************************************************'
sub FileListDblClick
filName$=FileListBox1.fileName
call  AddClrString ("7039:FileListDblClick filName$="+(filName$), clred, LogEdit)
FMngOpenFlg=1 ' файл загружен из файл-менеджера в список окон
if instr(filName$, ".exe")>0 or instr(filName$, ".com")>0 or instr(filName$, ".bat")>0 then
	run filName$
else
	FileLoad
end if
'qwq = SendMessageAPI (RichEdit1.handle,EM_EXLIMITTEXT,0,65535*32)
FMngOpenFlg=0 ' файл загружен из файл-менеджера в список окон

end sub 
'!************************************************************************'
sub FileClick '--- загрузка файла без включения его в список окон.
SrcEdit.color=HLColor(14) '&HE2ECB8
BorderRich.color=srcedit.color
FMngOpenFlg=1 '' открываем файл из файл-менеджера без включения

FNtmp$ = DIR$(FileListBox1.fileName, 0) 
'FileRec.Date        - Returns the file date as a string
'FileRec.Time        - Returns the file time as a string
'FileRec.Size        

Rusdate$=field$(FileRec.Date,"-",2)+"-"+field$(FileRec.Date,"-",1)+"-"+right$(field$(FileRec.Date,"-",3),2)

if FileRec.Size <=1000 then
	razm$="b"
	okrD=1
elseif FileRec.Size <1000000 then
	razm$="kb"
	okrD=1024
else
	razm$="Mb"
	okrD=1024*1024
end if

len$=vidnum$(FileRec.Size /okrD, 1)+" "+razm$ '+Rusdate$ +"   "+FileRec.Time

StatusPanel5.Caption=DirBox.text+FNtmp$+" "+len$+" "+Rusdate$+" "+FileRec.Time
'call  AddClrString ("7099:FNtmp$="+(FNtmp$), clred, LogEdit)

fext$=lcase$(StripFileExt (FNtmp$))
'call  AddClrString ("7101:FNtmp$="+(FNtmp$), clred, LogEdit)
select case fext$
case   ".png",".bmp",".ico",".jpg",".gif",".pcx",".exe",".dll" ,".bin",".obj",".zip",".rar",".arj",".tif",".pak",".cab",".chm"
	goto FileClickExitSub:
case else
end select

call  AddClrString ("10961:FileClick EditFlg="+str$(EditFlg), clred, LogEdit)


if EditFlg=0 then ' если в редакторе был загружен файл из списка - сохраняем его параметры.
	' сохраняем содержимое SrcEdit
	'---- сохраняем параметры окна ------------------
	CmdParam(WindowsIndex)=CmdLineEdit.text
	'call  AddClrString ("7256:WindowsIndex="+str$(WindowsIndex), clb, LogEdit)
	CursWin(WindowsIndex)=SrcEdit.SelStart 'запоминаем положение курсора
	'Lastwind=WindowsIndex
	WindMnu(WindowsIndex).checked=0
	SubsListBox(WindowsIndex).visible=0
	Window(WindowsIndex)=SrcEdit.text
	'---- -----------------------------------------
else
	
end if

EditFlg=1
call  AddClrString ("10979:FileClick EditFlg="+str$(EditFlg), cldb, LogEdit)

' загружаем без включения в проект
SrcEdit.loadFromFile(FNtmp$) ' подсветка нормально
'call  AddClrString ("7264:EditFlg="+str$(EditFlg), clred, LogEdit)
FileClickExitSub:
end sub

'!************************************************************************'
Sub btnClick (Sender as QButtonXP)
Select Case Sender.Tag
	'Case 0:  Call Main    ' Get
	'Case 1: Call     ChangeDirectory                        ' New
	'Case 21: Call FindText (Sender )
Case 222:    
	FilesOprFlg=0
	DirTreeForm.Height=450
	DirTreeForm.top=60
	DirTreeForm.show:
	
	'Case 3: Call  MaskChange                     ' Delete
End Select
End Sub

'!************************************************************************'
SUB ChangeDirectory
'call AddClrString ( "ChangeDirectory FilesOprFlg= "+str$(FilesOprFlg), clred, LogEdit)   
TruePath$=ConvertToTruePath(DirTree.Directory) 
'TruePath$=(DirTree.Directory) 

FileListBox1.Directory=TruePath$ ' DirTree.Directory 
DirBox.Text=TruePath$ 'DirTree.Directory 
CBoxHotDir.Text=TruePath$ 'DirTree.Directory 

'currdir$=DirTree.Directory 
FileListBox1.Mask=""
FileListBox1.Mask=MaskBox.Text

FileOriginalSaved=false
END SUB 

'!******************************************
Sub RichEditUndo 
SrcEdit.undo
End Sub 
'!******************************************
Sub RichEditCut
SrcEdit.cut 
End Sub 
'!******************************************
Sub RichEditCopy
SrcEdit.Copy 

End Sub 
'!******************************************
Sub RichEditPaste
nopaint=1
SrcEdit.visible=0
SrcEdit.Paste 
SrcEdit.visible=1
nopaint=0

End Sub 
'!******************************************
Sub RichEditDelete
SrcEdit.Delete 

End Sub 
'!******************************************
Sub RichEditSelectAll
SrcEdit.SelectAll 

End Sub                   
'!******************************************
Sub ListGridDrawCell (Col%, Row%, State%, Rect AS QRect)
exit sub ' почему-то выскакивает ошибка !!!
if ListGridReady=0 then exit sub

with SEarchListGrid
	if col%=2 and row% >0 then 
		sscel$=SEarchListGrid.cell(Col%, Row%)
		'call  AddClrString ("-----------", clb, LogEdit)
		'call  AddClrString ("6979:sscel$="+(sscel$), clred, LogEdit)
		if mid$(sscel$,1,1)="'" then 
			'if sscel$[1]="'" then
			'.FillRect(Rect.Left, Rect.Top, Rect.Left+.ColWidths(0), Rect.Top+.RowHeights(Row%),clgrey )
			.FillRect(Rect.Left, Rect.Top, Rect.Left+.ColWidths(Col%), Rect.Top+.RowHeights(Row%), clgrey)
			.TextOut(Rect.Left+2, Rect.Top+2, .Cell(Col%, Row%), Clr, -1)
		end if
	end if
end with

End Sub           
'!******************************************
Sub DeleteLedSpaces '(Sender as QMenuItem)
dim FakeList as QStringList
FakeList.Text=SrcEdit.Text

bzzzz$="---"
SrcEdit.HideSelection=1
SrcEdit.Enabled=0
oldi=0
Gauge1.Forecolor=clDP
Gauge1.visible=1
Gauge1.position=20

for i=0 to SrcEdit.LineCount-1
	if left$(FakeList.Item(i),1)=" " then
		FakeList.Item(i)=ltrim$(FakeList.Item(i))' bzzzz$
	end if
	if i>oldi+150 then 
		oldi=i
		Gauge1.position=i/(SrcEdit.LineCount+1)*100
	end if
next i
Gauge1.visible=0
Gauge1.Forecolor=clR
SrcEdit.Text=FakeList.Text

SrcEdit.HideSelection=1
SrcEdit.Enabled=1

End Sub 
'!******************************************
Sub DelHotTabs
zzz1=tally(SrcEdit.Text,ht)
'call  AddClrString ("7054:zzz1="+str$(zzz1), clred, LogEdit)
bzzzz$="---"

'SendMessageAPI SrcEdit.handle,EM_EXLIMITTEXT,0,65535*32
SrcEdit.HideSelection=1
SrcEdit.Enabled=0

bzzzz$=SrcEdit.Text
bzzzz$=ReplaceSubStr$(bzzzz$,ht,"")
SrcEdit.Text= bzzzz$
zzz1=tally(SrcEdit.Text,ht)
'call  AddClrString ("7060:zzz1="+str$(zzz1), clred, LogEdit)
SrcEdit.HideSelection=1
SrcEdit.Enabled=1
End Sub 


'!******************************************
Sub ReFormatHTml

'SrcEdit.Text=ReplaceSubStr$(SrcEdit.Text,ht," ")
'call  AddClrString ("7445: Deleting Led Spaces. Wait please...", clM, LogEdit)
CurPos=SrcEdit.SelStart
call DeleteLedSpaces

SrcEdit.Text=replacesubstr$(SrcEdit.Text,chr$(10), " ")
SrcEdit.Text=replacesubstr$(SrcEdit.Text,chr$(13), " ")
SrcEdit.Text=replacesubstr$(SrcEdit.Text,ht, " ")
SrcEdit.Text=replacesubstr$(SrcEdit.Text,"  ", " ")


'dim htmlfileLst as QStringList
'htmlfileLst.loadfromfile (HtmlfileName$)
'for i=0 to htmlfileLst.ItemCount-1

'htmlfileLst.Item(i)=Trim$(htmlfileLst.Item(i))
'next i


'dim htmlfile$ as string
Newhtmlfile$=""
kod$ = ""
probcnt=0

htmlfile$=SrcEdit.Text

' формируем отступы 
for i=1 to len (SrcEdit.Text)-1
	
	ss$ =htmlfile$[i]
	
	IF ss$ = "<" THEN  Tagflag=1 ' начало тега
	
	if Tagflag=1 then 
		' сохраняем текст
		if trim$(texttg$)<>"" then 
			inc probcnt
			probel$=STRING$(probcnt-1, ht) ' SPACE$(probcnt-1)
			Newhtmlfile$=Newhtmlfile$+probel$+LTRIM$(RTRIM$(texttg$))+crlf
			dec probcnt
			
		end if
		texttg$=""
		tag$ = tag$ + ss$ ' формируем тег
	else
		texttg$=texttg$+ss$ ' формируем техт
	end if
	
	IF ss$ = ">" THEN  
		'tag$ = tag$ + ss$ ' формируем тег - последний символ 
		' сохраняем тег 
		Testtag$=tag$-" "
		if   instr(lcase$(Testtag$), "br>" )  =0   and _
		instr(lcase$(Testtag$), "</" )=0 and _
		instr(lcase$(Testtag$), "<link" )=0 and _
		instr(lcase$(Testtag$), "<!" )=0 and _
		instr(lcase$(Testtag$), "<img" )=0 and _
		instr(lcase$(Testtag$), "<img" )=0 and _
		instr(lcase$(Testtag$), "<meta" )=0 and  _
		instr(lcase$(Testtag$), "<input" )=0 and  _ 
		instr(lcase$(Testtag$), "<param" )=0 and  _ 
		instr(lcase$(Testtag$), "<embed" )=0  _ 
		then inc probcnt '  instr(lcase$(Testtag$), "br>" )  =0   and
		'if instr(lcase$(Testtag$), "</" ) >0   then dec probcnt
		call  AddClrString ("8387:Testtag$="+(Testtag$), clred, LogEdit)
		
		if  instr(lcase$(Testtag$), "br>" )  >0 or _
		instr(lcase$(Testtag$), "<link" )>0 or _
		instr(lcase$(Testtag$), "<!" )>0 or _
		instr(lcase$(Testtag$), "<img" )>0 or _
		instr(lcase$(Testtag$), "<img" )>0 or _
		instr(lcase$(Testtag$), "<meta" )>0   _ 
		then
			probel$=STRING$(probcnt, ht) '   SPACE$(probcnt) INPUT  instr(lcase$(Testtag$), "<object" )>0   _ embed
			Newhtmlfile$=Newhtmlfile$+probel$+LTRIM$(RTRIM$(tag$))+crlf
			call  AddClrString ("8398:Testtag$="+(Testtag$), clred, LogEdit)
			
		else
			probel$=STRING$(probcnt-1, ht)
			Newhtmlfile$=Newhtmlfile$+probel$+LTRIM$(RTRIM$(tag$))+crlf
			
		end if
		
		
		cls
		'print Newhtmlfile$
		tag$ = "":
		if instr(lcase$(Testtag$), "</" ) >0   then dec probcnt
		
		Tagflag=0'
	end if
next i

SrcEdit.Text=Newhtmlfile$
SrcEdit.SelStart=CurPos

End Sub 

'!******************************************
Sub ReFormatRTF
'WaitForm.show
nopaint=1
CurPos=SrcEdit.SelStart
SrcEdit.Font=StrFont
SrcEdit.SelectAll
SrcEdit.SelAttributes=StrFont
SrcEdit.SelLength=0
nopaint=0

call  AddClrString ("7443: Deleting hot tabs. Wait please...", clM, LogEdit)
SrcEdit.Text=ReplaceSubStr$(SrcEdit.Text,ht," ")
call  AddClrString ("7445: Deleting Led Spaces. Wait please...", clM, LogEdit)
call DeleteLedSpaces

dim FakeList as QStringList
string1$=SrcEdit.Text

oldi=0

string1$=ReplaceSubStr$(string1$,"/{",chr$(134))
string1$=ReplaceSubStr$(string1$,"/}",chr$(135))
string1$=ReplaceSubStr$(string1$,"//",chr$(136))

string1$=ReplaceSubStr$(string1$,"{",crlf+"{"+crlf)
string1$=ReplaceSubStr$(string1$,"}",crlf+"}"+crlf)

'Clipboard.Text=string1$
FakeList.Text=string1$
indent$=""

for i=0 to FakeList.ItemCount-1  '  !!!------------------------------- --
	
	if left$(FakeList.Item(i)-ht,1)="{" then ' увеличиваем отступ
		FakeList.Item(i)=indent$+FakeList.Item(i)
		indent$=indent$+ht
	elseif left$(FakeList.Item(i)-ht,1)="}" then ' уменьшаем отступ
		indent$=left$(indent$,len(indent$)-1)
		FakeList.Item(i)=indent$+FakeList.Item(i)
	else
		FakeList.Item(i)=indent$+FakeList.Item(i)
	end if
	
	
next i

SrcEdit.Text=FakeList.Text

badd$=""
eadd$=""

z1=instr(string1$, "{")
z2=instr(string1$, "}")

cz1=tally(string1$, "{")
'call  AddClrString ("8107:cz1="+str$(cz1), clred, LogEdit)
cz2=tally(string1$, "}")
'call  AddClrString ("8109:cz2="+str$(cz2), clred, LogEdit)

exit sub

for i=0 to cz1
	string1$ = Insert$("", string1$,  z1+1) '-- returns Jello
	z1=instr(z1+1,string1$, "{")
	
	if z1<z2 then 
	else
	end if
	
	
	z2=instr(z1+1,string1$, "}")
	
	
	indent$=indent$+crlf+ht
next i

while z1>0 and z2>0 
	if z2>z1 then 
		string1$=delete$(string1$, z1, z2-z1+1) '+chr$(10) 
	else
		'<a class="small" target="_top" href="/default.asp">MSDN Home</a>&nbsp;>&nbsp;
		string1$=delete$(string1$, z2, 1)
	end if
	z1=instr( string1$, "<")
	z2=instr( string1$, ">")
	'dec z0
	inc z0
	
	if z0 > z0Old+50 then
		z0Old=z0 
	end if
	doevents
	if StopS1=1 then  exit while 'StopS1=0: "Завершаем операцию"
wend




End Sub 

'!******************************************
Sub ReFormatHotTabs
'IndentWords$="For-Next;With-End With;Select case-case-case else-end select;If-elseif-else-end if;Create-end create
'While-wend;Do-Loop"
' устанавливаем шрифт
WaitForm.show
nopaint=1
CurPos=SrcEdit.SelStart
SrcEdit.Font=StrFont
'SrcEdit.SelectAll
'SrcEdit.SelAttributes=StrFont
'SrcEdit.SelLength=0

'sssr$=SrcEdit.Text
'sssr$=ReplaceSubStr$(sssr$,ht,"")
WaitLabel.caption="Deleting hot tabs. Wait please..."
'call  AddClrString ("8877: Deleting hot tabs. Wait please...", clM, LogEdit)
t1=timer
SrcEdit.Text=ReplaceSubStr$(SrcEdit.Text,ht," ")
t2=timer
call  AddClrString ("8881: Deleting hot tabs. Time="+str$(t2-t1), clM, LogEdit)

'call  AddClrString ("7445: Deleting Led Spaces. Wait please...", clM, LogEdit)
WaitLabel.caption="Deleting Led Spaces. Wait please..."
t1=timer
call DeleteLedSpaces
t2=timer
call  AddClrString ("8888: DeletingLedSpaces. Time="+str$(t2-t1), clM, LogEdit)

'WaitLabel.caption="One moment. Wait please..."


dim FakeList as QStringList
FakeList.Text=SrcEdit.Text
SrcEdit.HideSelection=1: SrcEdit.Enabled=0
oldi=0
Gauge1.Forecolor=clM:Gauge1.visible=1: Gauge1.position=30
'FakeList.Text=ReplaceSubStr$(FakeList.Text,ht,"")

indent$=""
blockif=0
waitnext=0
waitendwith=0
selectcase=0
waitendcreate=0
blockwhile=0
lineN$=""

for i=0 to SrcEdit.LineCount-1  '  !!!------------------------------- --
	LSrc$=trim$(LCase$(FakeList.Item(i)))
	
	' обработка конкатенации
	if right$(LSrc$,1) ="_" then 
		LSrcSum$=LSrcSum$+LSrc$
		FakeList.Item(i)=indent$+FakeList.Item(i)
		
		goto nxtlin: 'next line
	else
		if LSrcSum$<>"" then ' значит что-то насуммировали, 
			LSrcSum$=LSrcSum$+LSrc$
			
			If instr(LSrcSum$,"_")>0 then 'разбираемся с конкатенациями
				~tmp$=""
				~tmp2$=""
				ssCnt=Tally(LSrcSum$,"_")
				'call  AddClrString ("8914:ssCnt="+str$(ssCnt), clred, LogEdit)
				for i22=1 to ssCnt+2
					~tmp$=(field$(LSrcSum$,"_",i22))
					~tmp1$=(field$(LSrcSum$,"_",i22+1))
					if trim$(~tmp1$)<>"" and right$(~tmp$,1)<>" " then ~tmp$=~tmp$+"_"
					~tmp2$=~tmp2$+~tmp$
				next i22
				LSrc$=trim$(~tmp2$)
				LSrcSum$=""
			end if
		end if
		
	end if
	
	
	if instr(LSrc$,"'")>0 then 
		call DelComments(@LSrc$)
		LSrc$=trim$(LSrc$)
	end if
	if GetVarPos(LSrc$,"for") =1  and GetVarPos(LSrc$,"next") =0  then
		lineN$="<next> missed in line "+str$(i+1)
		FakeList.Item(i)=indent$+FakeList.Item(i)
		inc waitnext:indent$=ht+indent$
	elseif GetVarPos(LSrc$, "next")=1 then
		lineN$=""
		dec waitnext:
		if waitnext<0 then 
			call  AddClrString ("Error! FOR st missed, but NEXT presents in line "+str$(i+1), &H8200FF, LogEdit)
			waitnext=0
		end if
		indent$=delete$(indent$,1,1)
		FakeList.Item(i)=indent$+FakeList.Item(i)
	elseif GetVarPos(LSrc$, "with")=1 then
		lineN$="<end with> missed in line "+str$(i+1)
		FakeList.Item(i)=indent$+FakeList.Item(i)
		inc waitendwith:indent$=ht+indent$
	elseif GetVarPos(LSrc$-" ", "endwith")=1 then
		lineN$=""
		dec waitendwith:
		if waitendwith<0 then 
			call  AddClrString ("Error! WITH st missed, but END WITH presents  in line "+str$(i+1), &H8200FF, LogEdit)
			waitendwith=0
		end if
		indent$=delete$(indent$,1,1)
		FakeList.Item(i)=indent$+FakeList.Item(i)
	elseif GetVarPos(LSrc$, "select case")=1 then
		lineN$="<end select> missed in line "+str$(i+1)
		
		FakeList.Item(i)=indent$+FakeList.Item(i)
		inc selectcase:indent$=ht+indent$
	elseif GetVarPos(LSrc$, "case")=1 then
		dec selectcase:
		if selectcase<0 then 
			call  AddClrString ("Error! SELECT CASE st missed, but CASE presents  in line "+str$(i+1), &H8200FF, LogEdit)
			selectcase=0
		end if
		indent$=delete$(indent$,1,1)
		FakeList.Item(i)=indent$+FakeList.Item(i)
		inc selectcase:indent$=ht+indent$
	elseif GetVarPos(LSrc$, "case else")=1 then
		dec selectcase:
		if selectcase<0 then 
			call  AddClrString ("Error! CASE st missed, but CASE ELSE presents  in line "+str$(i+1), &H8200FF, LogEdit)
			selectcase=0
		end if
		indent$=delete$(indent$,1,1)
		FakeList.Item(i)=indent$+FakeList.Item(i)
		inc selectcase:indent$=ht+indent$
	elseif GetVarPos(LSrc$, "end select")=1 then
		lineN$=""
		dec selectcase:
		if selectcase<0 then 
			call  AddClrString ("Error! CASE ELSE st missed, but END SELECT presents in line "+str$(i+1), &H8200FF, LogEdit)
			selectcase=0
		end if
		indent$=delete$(indent$,1,1)
		FakeList.Item(i)=indent$+FakeList.Item(i)
		
	elseif GetVarPos(LSrc$,"if") =1 and ( right$( DelCommentsF(LSrc$), 5  )=" then" or right$(DelCommentsF(LSrc$), 5)=qt+"then" or right$(DelCommentsF(LSrc$), 5)=")then" or right$(  DelCommentsF(LSrc$) , 5 )=ht+"then") then   
		'elseif GetVarPos(LSrc$,"if") =1 and ( right$(DelCommentsF(LSrc$),5)=" then" or right$(DelCommentsF(LSrc$),5)="then ") then
		FakeList.Item(i)=indent$+FakeList.Item(i)
		inc blockif: indent$=ht+indent$
	elseif (Left$(LSrc$, 7)="elseif " or Left$(LSrc$, 7)="elseif(") and (right$(LSrc$, 5)=" then" or right$(LSrc$, 5)=qt+"then" or right$(LSrc$, 5)=")then" ) then
		'elseif GetVarPos(LSrc$,"elseif") =1  and GetVarPos(LSrc$,"then") >0  then
		dec blockif:
		if blockif<0 then 
			call  AddClrString ("Error! IF st missed, but ELSE IF presents  in line "+str$(i+1), &H8200FF, LogEdit)
			blockif=0
		end if
		indent$=delete$(indent$,1,1)
		FakeList.Item(i)=indent$+FakeList.Item(i)
		inc blockif:indent$=ht+indent$
		'elseif Left$(LSrc$, 4)="else" then
	elseif GetVarPos(LSrc$, "else")=1 then
		dec blockif:
		if blockif<0 then 
			call  AddClrString ("Error! IF st missed, but ELSE presents  in line "+str$(i+1), &H8200FF, LogEdit)
			blockif=0
		end if
		
		indent$=delete$(indent$,1,1)
		FakeList.Item(i)=indent$+FakeList.Item(i)
		inc blockif:indent$=ht+indent$
		'elseif Left$(LSrc$-" ", 5)="endif" then
		'elseif GetVarPos(LSrc$-" ", "endif")=1 then
	elseif GetVarPos(LSrc$-" ", "endif")=1 then
		dec blockif:
		if blockif<0 then 
			call  AddClrString ("Error! <For> or <ELSE> st missed, but <END IF> presents  in line "+str$(i+1), &H8200FF, LogEdit)
			blockif=0
		end if
		indent$=delete$(indent$,1,1)
		FakeList.Item(i)=indent$+FakeList.Item(i)
		'elseif Left$(LSrc$, 7)="create " then
	elseif GetVarPos(LSrc$, "create")=1 then '  !!! 
		FakeList.Item(i)=indent$+FakeList.Item(i)
		inc waitendcreate:indent$=ht+indent$
		'elseif Left$(LSrc$-" ", 9)="endcreate" then
	elseif GetVarPos(LSrc$-" ", "endcreate")=1 then
		dec waitendcreate:
		if waitendcreate<0 then 
			call  AddClrString ("Error! CREATE st missed, but END CREATE presents in line "+str$(i+1), &H8200FF, LogEdit)
			waitendcreate=0
		end if
		indent$=delete$(indent$,1,1)
		FakeList.Item(i)=indent$+FakeList.Item(i)
		'elseif Left$(LSrc$, 6)="while " then
	elseif GetVarPos(LSrc$, "while")=1 then
		lineN$="<wend> missed in line "+str$(i+1)
		FakeList.Item(i)=indent$+FakeList.Item(i)
		inc blockwhile:indent$=ht+indent$
		'elseif Left$(LSrc$, 4)="wend" then
	elseif GetVarPos(LSrc$, "wend")=1 then
		lineN$=""
		
		dec blockwhile:
		if blockwhile<0 then 
			call  AddClrString ("Error! WHILE st missed, but WEND  presents  in line "+str$(i+1), &H8200FF, LogEdit)
			blockwhile=0
		end if
		indent$=delete$(indent$,1,1)
		FakeList.Item(i)=indent$+FakeList.Item(i)
	else
		FakeList.Item(i)=indent$+FakeList.Item(i)
	end if
	if i>oldi+150 then 
		oldi=i
		Gauge1.position=i/(SrcEdit.LineCount+1)*100
	end if
	nxtlin:
next i

'call  AddClrString ("9077:lineN$="+(lineN$), clred, LogEdit)

if lineN$<>"" then
	call  AddClrString ("Error! "+lineN$, &H8200FF, LogEdit)
	
end if


if blockif>0 then call  AddClrString ("Error! <end if> missed", &H8200FF, LogEdit)
waitnext=0
waitendwith=0
selectcase=0
waitendcreate=0
blockwhile=0



Gauge1.visible=0:Gauge1.Forecolor=clR:
SrcEdit.HideSelection=0:SrcEdit.Enabled=1
SrcEdit.Text=FakeList.Text
SrcEdit.SelStart=CurPos
'WindowState=1

nopaint=0
WaitForm.Visible=0

End Sub



'!******************************************
Sub DelCommentsOnClick
dim FakeList as QStringList
FakeList.Text=SrcEdit.Text
SrcEdit.HideSelection=1: SrcEdit.Enabled=0
oldi=0
Gauge1.Forecolor=clo:Gauge1.visible=1: Gauge1.position=0
for i=0 to SrcEdit.LineCount-1
	LSrc$=FakeList.Item(i)
	if instr(LSrc$,"'")>0 then 
		call DelComments(@LSrc$)
	end if
	FakeList.Item(i)=LSrc$
	if i>oldi+150 then 
		oldi=i
		Gauge1.position=i/(SrcEdit.LineCount+1)*100
	end if
next
Gauge1.visible=0:Gauge1.Forecolor=clR:
SrcEdit.HideSelection=0:SrcEdit.Enabled=1
SrcEdit.Text=FakeList.Text
DelEmptyLinesOnClick
End Sub 
'!******************************************
Sub DelEmptyLinesOnClick
dim FakeList as QStringList
dim FakeListOut as QStringList
'call  AddClrString ("7257:FakeList.ItemCount-1="+str$(FakeList.ItemCount-1), clred, LogEdit)

FakeListOut.clear

FakeList.Text=SrcEdit.Text
SrcEdit.HideSelection=1: SrcEdit.Enabled=0
oldi=0
defint jout=0
Gauge1.Forecolor=clDg:Gauge1.visible=1: Gauge1.position=0
for i=0 to FakeList.ItemCount-1
	'call  AddClrString ("7257:i="+str$(i), clred, LogEdit)
	if trim$(FakeList.Item(i)-ht)<>"" then
		FakeListOut.AddItems FakeList.Item(i)
		'FakeListOut.Item(jout)=FakeList.Item(i)
		'inc jout
	end if
	if i>oldi+150 then 
		oldi=i
		Gauge1.position=i/(SrcEdit.LineCount+1)*100
	end if
next 
Gauge1.visible=0:Gauge1.Forecolor=clR:
SrcEdit.HideSelection=0:SrcEdit.Enabled=1
SrcEdit.Text=FakeListOut.Text

End Sub 

'!******************************************
Sub ReFormatHotTabsCurSub
dim FakeList as QStringList
LSrcEditText$=lcase$(SrcEdit.Text)

call  AddClrString ("11921:BegSub="+str$(BegSub), clred, LogEdit)


Endsub1=instr(BegSub+1,LSrcEditText$,"end sub")
Endsub2=instr(BegSub+1,LSrcEditText$,"end function")

if Endsub1<Endsub2 then Endsub=Endsub1-1 else Endsub=Endsub2-1
oldselstart=SrcEdit.SelStart
SrcEdit.SelStart=BegSub
'call  AddClrString ("8398:BegSub="+str$(BegSub), clred, LogEdit)
SrcEdit.SelLength=Endsub-BegSub
call  AddClrString ("8400:Endsub="+str$(Endsub), clred, LogEdit)
'SrcEdit.SelText=""
SrcEdit.SelAttributes=StrFont
tmptxt$=SrcEdit.SelText
tmptxt$=ReplaceSubStr$(tmptxt$,ht," ")

SrcEdit.SelText=tmptxt$
'SrcEdit.SelText=ReplaceSubStr$(SrcEdit.SelText,ht," ")

LbSub=SrcEdit.LINEFROMPos (BegSub)
'call  AddClrString ("8403:LbSub="+str$(LbSub), clred, LogEdit)
LeSub=SrcEdit.LINEFROMPos (Endsub)
'call  AddClrString ("8405:LeSub="+str$(LeSub), clred, LogEdit)

'exit sub
SrcEdit.SelStart=BegSub
'call  AddClrString ("8398:BegSub="+str$(BegSub), clred, LogEdit)
SrcEdit.SelLength=Endsub-BegSub
'call  AddClrString ("8429:Endsub-BegSub="+str$(Endsub-BegSub), clred, LogEdit)

FakeList.text=SrcEdit.SelText
'call  AddClrString ("8432:FakeList.text="+(FakeList.text), clred, LogEdit)


for i=0 to LeSub-LbSub-1
	'call  AddClrString ("8439:FakeList.Item("+str$(i)+")="+str$(FakeList.Item(i)), clred, LogEdit)
	'call  AddClrString ("8436:i="+str$(i), clred, LogEdit)
	if left$(FakeList.Item(i),1)=" " then
		FakeList.Item(i)=ltrim$(FakeList.Item(i))' bzzzz$
	end if
next i

'exit sub


indent$=""
blockif=0
waitnext=0
waitendwith=0
selectcase=0
waitendcreate=0
blockwhile=0
lineN$=""


'call  AddClrString ("8443:indent$="+(indent$), clred, LogEdit)
'call  AddClrString ("8446:LeSub -LbSub="+str$(LeSub -LbSub), clred, LogEdit)
for i=0 to LeSub -LbSub-1 '  !!!------------------------------- --
	
	'call  AddClrString ("8446:i="+str$(i), clred, LogEdit)
	LSrc$=trim$(LCase$(FakeList.Item(i)))
	
	' обработка конкатенации
	if right$(LSrc$,1) ="_" then 
		LSrcSum$=LSrcSum$+LSrc$
		FakeList.Item(i)=indent$+FakeList.Item(i)
		
		goto nxtlin11: 'next line
	else
		if LSrcSum$<>"" then ' значит что-то насуммировали, 
			LSrcSum$=LSrcSum$+LSrc$
			
			If instr(LSrcSum$,"_")>0 then 'разбираемся с конкатенациями
				~tmp$=""
				~tmp2$=""
				ssCnt=Tally(LSrcSum$,"_")
				'call  AddClrString ("8914:ssCnt="+str$(ssCnt), clred, LogEdit)
				for i22=1 to ssCnt+2
					~tmp$=(field$(LSrcSum$,"_",i22))
					~tmp1$=(field$(LSrcSum$,"_",i22+1))
					if trim$(~tmp1$)<>"" and right$(~tmp$,1)<>" " then ~tmp$=~tmp$+"_"
					~tmp2$=~tmp2$+~tmp$
				next i22
				LSrc$=trim$(~tmp2$)
				LSrcSum$=""
			end if
		end if
		
	end if
	
	
	if instr(LSrc$,"'")>0 then 
		call DelComments(@LSrc$)
		LSrc$=trim$(LSrc$)
	end if
	'if Left$(LSrc$, 3)="for" and (mid$(LSrc$, 4,1)=" " or mid$(LSrc$, 4,1)="(")  and GetVarPos(LSrc$,"next") =0  then
	if GetVarPos(LSrc$,"for") =1  and GetVarPos(LSrc$,"next") =0  then
		lineN$="<next> missed in line "+str$(i+1)
		FakeList.Item(i)=indent$+FakeList.Item(i)
		inc waitnext:indent$=ht+indent$
		'elseif Left$(LSrc$, 5)="next" or Left$(LSrc$, 5)="next " or Left$(LSrc$, 5)="next(" then
	elseif GetVarPos(LSrc$, "next")=1 then
		lineN$=""
		dec waitnext:
		if waitnext<0 then call  AddClrString ("Error! FOR st missed, but NEXT presents in line "+str$(i+1), &H8200FF, LogEdit)
		indent$=delete$(indent$,1,1)
		FakeList.Item(i)=indent$+FakeList.Item(i)
		'elseif Left$(LSrc$, 5)="with " then
	elseif GetVarPos(LSrc$, "with")=1 then
		lineN$="<end with> missed in line "+str$(i+1)
		
		FakeList.Item(i)=indent$+FakeList.Item(i)
		inc waitendwith:indent$=ht+indent$
		'elseif Left$(LSrc$-" ", 7)="endwith" then
	elseif GetVarPos(LSrc$-" ", "endwith")=1 then
		lineN$=""
		dec waitendwith:
		if waitendwith<0 then call  AddClrString ("Error! WITH st missed, but END WITH presents  in line "+str$(i+1), &H8200FF, LogEdit)
		indent$=delete$(indent$,1,1)
		FakeList.Item(i)=indent$+FakeList.Item(i)
		'elseif Left$(LSrc$-" ", 10)="selectcase" then
		'elseif GetVarPos(LSrc$-" ", "selectcase")=1 then
	elseif GetVarPos(LSrc$, "select case")=1 then
		lineN$="<end select> missed in line "+str$(i+1)
		
		FakeList.Item(i)=indent$+FakeList.Item(i)
		inc selectcase:indent$=ht+indent$
		'elseif Left$(LSrc$, 5)="case " or Left$(LSrc$, 5)="case(" or Left$(LSrc$, 5)="case"+qt then
	elseif GetVarPos(LSrc$, "case")=1 then
		dec selectcase:
		if selectcase<0 then call  AddClrString ("Error! SELECT CASE st missed, but CASE presents  in line "+str$(i+1), &H8200FF, LogEdit)
		indent$=delete$(indent$,1,1)
		FakeList.Item(i)=indent$+FakeList.Item(i)
		inc selectcase:indent$=ht+indent$
		'elseif Left$(LSrc$-" ", 8)="caseelse" then
	elseif GetVarPos(LSrc$, "case else")=1 then
		dec selectcase:
		if selectcase<0 then call  AddClrString ("Error! CASE st missed, but CASE ELSE presents  in line "+str$(i+1), &H8200FF, LogEdit)
		indent$=delete$(indent$,1,1)
		FakeList.Item(i)=indent$+FakeList.Item(i)
		inc selectcase:indent$=ht+indent$
		'elseif Left$(LSrc$-" ", 9)="endselect" then
		'elseif GetVarPos(LSrc$-" ", "endselect")=1 then
	elseif GetVarPos(LSrc$, "end select")=1 then
		lineN$=""
		dec selectcase:
		if selectcase<0 then call  AddClrString ("Error! CASE ELSE st missed, but END SELECT presents in line "+str$(i+1), &H8200FF, LogEdit)
		indent$=delete$(indent$,1,1)
		FakeList.Item(i)=indent$+FakeList.Item(i)
		
		elseif GetVarPos(LSrc$,"if") =1 and ( right$( DelCommentsF(LSrc$), 5  )=" then" or right$(DelCommentsF(LSrc$), 5)=qt+"then" or right$(DelCommentsF(LSrc$), 5)=")then" or _
	right$(  DelCommentsF(LSrc$) , 5 )=ht+"then") then   
		
		'elseif (Left$(LSrc$, 3)="if " or Left$(LSrc$, 3)="if(") and (right$(LSrc$, 5)=" then" or right$(LSrc$, 5)=qt+"then" or right$(LSrc$, 5)=")then") then   
		'elseif GetVarPos(LSrc$,"if") =1  and GetVarPos(LSrc$,"then") >0  then
		
		FakeList.Item(i)=indent$+FakeList.Item(i)
		inc blockif:indent$=ht+indent$
	elseif (Left$(LSrc$, 7)="elseif " or Left$(LSrc$, 7)="elseif(") and (right$(LSrc$, 5)=" then" or right$(LSrc$, 5)=qt+"then" or right$(LSrc$, 5)=")then" ) then
		'elseif GetVarPos(LSrc$,"elseif") =1  and GetVarPos(LSrc$,"then") >0  then
		dec blockif:
		if blockif<0 then call  AddClrString ("Error! IF st missed, but ELSE IF presents  in line "+str$(i+1), &H8200FF, LogEdit)
		indent$=delete$(indent$,1,1)
		FakeList.Item(i)=indent$+FakeList.Item(i)
		inc blockif:indent$=ht+indent$
		'elseif Left$(LSrc$, 4)="else" then
	elseif GetVarPos(LSrc$, "else")=1 then
		dec blockif:
		if blockif<0 then call  AddClrString ("Error! IF st missed, but ELSE presents  in line "+str$(i+1), &H8200FF, LogEdit)
		indent$=delete$(indent$,1,1)
		FakeList.Item(i)=indent$+FakeList.Item(i)
		inc blockif:indent$=ht+indent$
		'elseif Left$(LSrc$-" ", 5)="endif" then
		'elseif GetVarPos(LSrc$-" ", "endif")=1 then
	elseif GetVarPos(LSrc$-" ", "endif")=1 then
		dec blockif:
		if blockif<0 then call  AddClrString ("Error! ELSE st missed, but END IF presents  in line "+str$(i+1), &H8200FF, LogEdit)
		indent$=delete$(indent$,1,1)
		FakeList.Item(i)=indent$+FakeList.Item(i)
		'elseif Left$(LSrc$, 7)="create " then
	elseif GetVarPos(LSrc$, "create")=1 then '  !!! 
		FakeList.Item(i)=indent$+FakeList.Item(i)
		inc waitendcreate:indent$=ht+indent$
		'elseif Left$(LSrc$-" ", 9)="endcreate" then
	elseif GetVarPos(LSrc$-" ", "endcreate")=1 then
		dec waitendcreate:
		if waitendcreate<0 then call  AddClrString ("Error! CREATE st missed, but END CREATE presents in line "+str$(i+1), &H8200FF, LogEdit)
		indent$=delete$(indent$,1,1)
		FakeList.Item(i)=indent$+FakeList.Item(i)
		'elseif Left$(LSrc$, 6)="while " then
	elseif GetVarPos(LSrc$, "while")=1 then
		lineN$="<wend> missed in line "+str$(i+1)
		FakeList.Item(i)=indent$+FakeList.Item(i)
		inc blockwhile:indent$=ht+indent$
		'elseif Left$(LSrc$, 4)="wend" then
	elseif GetVarPos(LSrc$, "wend")=1 then
		lineN$=""
		
		dec blockwhile:
		if blockwhile<0 then call  AddClrString ("Error! WHILE st missed, but WEND  presents  in line "+str$(i+1), &H8200FF, LogEdit)
		indent$=delete$(indent$,1,1)
		FakeList.Item(i)=indent$+FakeList.Item(i)
	else
		FakeList.Item(i)=indent$+FakeList.Item(i)
	end if
	
	nxtlin11:
next i

if lineN$<>"" then
	call  AddClrString ("Error! "+lineN$, &H8200FF, LogEdit)
end if

'SrcEdit.SelText=FakeList.text
oldcliptxt$=clipboard.text
'call  AddClrString ("8555:oldcliptxt$="+(oldcliptxt$), clred, LogEdit)
clipboard.text=FakeList.text
SrcEdit.PasteFromClipboard
clipboard.text=oldcliptxt$
SrcEdit.SelStart=oldselstart

'call  AddClrString ("8541:FakeList.text="+(FakeList.text), clred, LogEdit)


End Sub 

'!******************************************
Sub PasteSubDelimiter
OldClipBrd$=ClipBoard.text 
ClipBoard.text=SubDelim$
SrcEdit.PasteFromClipboard
ClipBoard.text=OldClipBrd$

End Sub 

'!******************************************
Sub NextSub
NextSubNamePos1=Instr(SrcEdit.SelStart, lcase$(SrcEdit.Text)," sub")
NextSubNamePos=Instr(NextSubNamePos1+2, lcase$(SrcEdit.Text),"sub ")
SrcEdit.SelStart=Len(SrcEdit.Text)-5
SrcEdit.SelStart=NextSubNamePos+10'SubLineIdxNext

End Sub 
'!******************************************
Sub PrevSub
PrevSubNamePos=rInstr(SubNamePos-1, lcase$(SrcEdit.Text),"sub ")
SrcEdit.SelStart=PrevSubNamePos+10'SubLineIdxPrev

End Sub 
'!******************************************
'Sub NextBookMark

'End Sub 
'!******************************************
'Sub PrevBookMark

'End Sub   

'!******************************************
sub NewProjectBoxClose (Sender as QButtonXP)                              
'NewProjectBox.visible=0
NewProjectBox.close
'NewProjectBox.WindowState=1

END SUB

'!******************************************
sub fake

End Sub

'!******************************************
Sub HelpRus
PID=ShellExecute (0,"open",StartPath$+"help\index.htm","","",1)
End Sub 
'!******************************************
Sub HelpEn
PID=ShellExecute (0,"open",StartPath$+"help\indexEn.htm","","",1)
End Sub 

'!******************************************
Sub HtHLBoxClick
if HtHLBox.checked=1 then 
	
	'call  AddClrString ("12361:HtHLBox.checked="+str$(HtHLBox.checked), clred, LogEdit)
	
	
	
	'StatusBar.Panel(4).caption ="EOL=CRLF(13 10)"
	'StatusBar.Panel(4).caption ="EOL=CR(13)"
	'StatusBar.Panel(4).caption ="EOL=LF(10)"
	
	
	if instr(SrcEdit.text ,crlf) > 0 then
		HotTAbHL=3
		StatusBar.Panel(4).caption ="EOL=CRLF(13 10)"
	elseif instr(SrcEdit.text ,cr) >0 then
		StatusBar.Panel(4).caption ="EOL=CR(13)"
		HotTAbHL=1
	elseif instr(SrcEdit.text ,lf) >0 then
		StatusBar.Panel(4).caption ="EOL=CR(10)"
		HotTAbHL=1
	else
	end if
	
else
	
	if instr(SrcEdit.text ,crlf) > 0 then
		HotTAbHL=2
		StatusBar.Panel(4).caption ="EOL=CRLF(13 10)"
	elseif instr(SrcEdit.text ,cr) >0 then
		StatusBar.Panel(4).caption ="EOL=CR(13)"
		HotTAbHL=0
	elseif instr(SrcEdit.text ,lf) >0 then
		StatusBar.Panel(4).caption ="EOL=CR(10)"
		HotTAbHL=0
	else
	end if
	
	
	
end if
'call  AddClrString ("12380:HotTAbHL="+str$(HotTAbHL), 88, LogEdit)


SrcEdit.visible=0
SrcEdit.visible=1

End Sub                    


'!******************************************
Sub DisRemBtnOnClick
'call  AddClrString ("7512:DisRemBtn.checked="+str$(DisRemBtn.checked), clred, LogEdit)

if DisRemBtn.checked=1 then
	
	for i=1 to SEarchListGrid.RowCount-1 
		ahgrr$=ltrim$(SEarchListGrid.cell(2,I))
		if ahgrr$= "" then goto nextiDis
		ss1$=ahgrr$[1]
		if ss1$="'" then 'and DisRemBtn.checked=1 then
			SEarchListGrid.RowHeights(I)=0
		else
		end if
		nextiDis:
	next i
	SEarchListGrid.repaint
else
	for i=1 to SEarchListGrid.RowCount-1 
		SEarchListGrid.RowHeights(I)=SEarchListGrid.DefaultRowHeight
	next i
	
end if

End Sub   

'!******************************************
Sub WindowsOnClick
'call  AddClrString ("7541:FileMng.checked="+str$(FileMng.checked), clb, LogEdit)
if FileMng.checked=1 then 
	LastWindClick
	'FileMng.checked=0
end if
ViewFileMng
'call  AddClrString ("7543:FileMng.checked="+str$(FileMng.checked), clred, LogEdit)

End Sub  

'!******************************************
Sub ShowFreePhysicalMemory
Dim sCaption As String * 15
sCaption = String$(15, "0")
GlobalMemoryStatus (tMemInfo)
MemStatusLabel2.Caption ="Virtual Mem="+ StrF$(tMemInfo.dwAvailVirtual, ffNumber, 15, 0)
MemStatusLabel1.Caption ="Free Mem="+ StrF$(tMemInfo.dwAvailPhys, ffNumber, 15, 0)

exit sub

'frmMemStatus.Caption = StrF$(tMemInfo.dwAvailPhys, ffNumber, 15, 0) & " Bytes Of Free Phys Mem"
'
'rchWin.Clear
'rchWin.Addstring = " Used Phys(%) = " & sTr$(tMemInfo.dwMemoryLoad)
'rchWin.Addstring = " Total Phys = " & sTr$(tMemInfo.dwTotalPhys)
'rchWin.Addstring = " Avail Phys = " & sTr$(tMemInfo.dwAvailPhys)
'rchWin.Addstring = "Total Page File = " & sTr$(tMemInfo.dwTotalPageFile)
'rchWin.Addstring = "Avail Page File = " & sTr$(tMemInfo.dwAvailPageFile)
'rchWin.Addstring = " Totat Virtial = " & sTr$(tMemInfo.dwTotalVirtual)
'rchWin.Addstring = " Avail Virtual = " & sTr$(tMemInfo.dwAvailVirtual)
End Sub


'!******************************************
Sub UtilOnClick

End Sub  

'!******************************************
Sub SplitterV2Moved

DirBox.Width =FilePanel.Width-40
DirBtn.Left =DirBox.Width+6
MaskBox.Width =FilePanel.Width-12
FileListBox1.Width =FilePanel.Width-12
LogTopLeftPanel.width=LogTopPanel.width/2 +25


End Sub 

'!******************************************
Sub CheckEmptyWinOnClick
for i=0 to WindowsItemCount-1 ' определяем новый индекс - любое непустое окно
	FileName$ =field$(WindMnu(i).caption,chr$(160),2)'WindMnu(WindowsIndex).caption
	'call  AddClrString ("7741:WindMnu("+str$(i)+").caption)="+(WindMnu(i).caption), cldg, LogEdit)
	'call  AddClrString ("7741:FileName$ ="+(FileName$ ), clblue, LogEdit)
	if fileexists(FileName$)=0  then  
		'call  AddClrString ("7740:File not exists="+(FileName$), clr, LogEdit)
		'Windows.DelIndex(i)  ' DelItems WindMnu(i)Enabled
		WindMnu(i).enabled=0
	else
	end if
	
next i

End Sub 

'!******************************************
Sub DelEmptyWinOnClick
for i=0 to WindowsItemCount-1 ' определяем новый индекс - любое непустое окно
	WindMnu(i).enabled=1
	
	'call  AddClrString ("7972:WindMnu("+str$(i)+"caption)="+(WindMnu(i).caption), cldg, LogEdit)
	'call  AddClrString ("7974:WindMnu("+str$(i)+"MenuIndex)="+str$(WindMnu(i).MenuIndex), cldg, LogEdit)
	
	FileName$ =field$(WindMnu(i).caption,chr$(160),2)'WindMnu(WindowsIndex).caption
	if fileexists(FileName$)=0  then  
		
		Window(i)=""
		WindMnu(i).visible=0
		WindMnu(i).caption=""
		SubsListBox(i).visible=0
		CursWin(i)=1
		dec VisibleWindows
		
		'Windows.DelItems (WindMnu(i).MenuIndex)
		'call  AddClrString ("7972:WindMnu("+str$(i)+"caption)="+(WindMnu(i).caption), clred, LogEdit)
		'call  AddClrString ("7974:WindMnu("+str$(i)+"MenuIndex)="+str$(WindMnu(i).MenuIndex), clred, LogEdit)
		'Windows.DelIndex WindMnu(i).MenuIndex
		'Enabled
		Modif(i)=0
		'WindowsIndex=0
		
		call Tab25DelTab
		
	else
	end if
	
next i

End Sub 


'!******************************************
Sub ObjTreeOnClick
call  AddClrString ("12275:ObjTreeOnClick="+str$(ObjTreeOnClick), cllb, LogEdit)

'if ObjTreeMnu.checked=0 then 
'ObjTreeMnu.checked=1

Call BuildObjTree

FilePanel.visible=1
ObjTreePanel.visible=1
'FileListPanel.visible=0
ObjTreeView.visible=1
IncTreeView.visible=0
SubsListBox(WindowsIndex).visible=0
'else
'ObjTreeMnu.checked=0
'FilePanel.visible=0
'ObjTreePanel.visible=0
'ObjTreeView.visible=0
'FileListPanel.visible=0
'end if
End Sub  

'!******************************************
Sub BuildObjTree
dim FakeList as QStringList
FakeList.Text=SrcEdit.Text
LevelCount=256
dim NodCountOnLevel (LevelCount) as long
LevelIdx=0
ItemCount=0

ObjTreeView.Clear

Gauge1.Forecolor=clM:Gauge1.visible=1: Gauge1.position=30
for i=0 to FakeList.ItemCount-1
	LFAke$=LCase$(FakeList.Item(i))-ht
	if instr(LFAke$, "create")=0 then goto nxtlin1:
	LSrc$=trim$(LFAke$)
	if instr(LSrc$,"'")>0 then 
		call DelComments(@LSrc$)
		LSrc$=trim$(LSrc$)
	end if
	zzcreate=GetVarPos(LSrc$, "create")
	zzEndcreate=GetVarPos(LSrc$-" ", "endcreate")
	if zzcreate=1 then
		if LevelIdx=0 then 
			ObjTreeView.AddItems LSrc$+chr$(160)+str$(i+1)
			NodCountOnLevel (LevelIdx)=ItemCount ' число узлов на нулевом уровне.
			inc ItemCount
			NodCountOnLevel (LevelIdx)=ItemCount ' число узлов на нулевом уровне.
		else
			ObjTreeView.AddChildItems NodCountOnLevel (LevelIdx-1)-1, LSrc$+chr$(160)+str$(i+1) ' добавляем узел на уровень LevelIdx
			inc ItemCount
			NodCountOnLevel (LevelIdx)=ItemCount
		end if
		inc LevelIdx ' на следующий уровень
		
	elseif zzEndcreate=1 then
		dec LevelIdx ' на предыдущий уровень
		if LevelIdx<0 then call  AddClrString ("Error! CREATE statement missed, but END CREATE presents in line "+str$(i+1), &H8200FF, LogEdit)
	else
	end if
	
	if i>oldi+150 then 
		oldi=i
		Gauge1.position=i/(SrcEdit.LineCount+1)*100
	end if
	nxtlin1:
next i 
Gauge1.visible=0:Gauge1.Forecolor=clR:
'ObjTreeView.FullExpand

End Sub 

'!******************************************
Sub ObjTreeViewClick
Lnum=val(field$ (ObjTreeView.Item(ObjTreeView.ItemIndex).Text,chr$(160),2 ))
'call  AddClrString ("7960:Lnum="+str$(Lnum), clred, LogEdit)
nopaint=1
SrcEdit.SelStart=Len(SrcEdit.Text)-2
SrcEdit.SelStart=SrcEdit.GetLINEIdx(Lnum-1)
SrcEdit.SelLength=1
nopaint=0

End Sub 
'!******************************************
Sub IncTreeViewClick
call  AddClrString ("8949:IncTreeViewClick=", clred, LogEdit)
'BMarkMain.enabled=0

call  AddClrString ("12139:IncTreeViewClick EditFlg="+str$(EditFlg), cldr, LogEdit)


if EditFlg=0 then   'если был  файл  из списка то его параметры запоминаем
	'---- сохраняем параметры окна ------------------
	CmdParam(WindowsIndex)=CmdLineEdit.text
	CursWin(WindowsIndex)=SrcEdit.SelStart 'запоминаем положение курсора
	'Lastwind=WindowsIndex
	SubsListBox(WindowsIndex).visible=0
	Window(WindowsIndex)=SrcEdit.text
	Modif(WindowsIndex)=SrcEdit.Modified
	LastWindMnu.Enabled=1
end if
EditFlg=2
call  AddClrString ("12153:IncTreeViewClick EditFlg="+str$(EditFlg), cldr, LogEdit)

IncFileName$= (bspath(IncTreeView.Item(IncTreeView.ItemIndex).text))
call  AddClrString ("8988:IncFileName$="+(IncFileName$), clred, LogEdit)
FNtmp$ =IncFileName$

MarginEdit.color=&HABECFF 'HLColor(10)

SrcEdit.loadfromFile (IncFileName$)  ' (IncFilesMnu(.MenuIndex).caption)
StatusPanel5.Caption=IncFileName$




Tab25.AddTabs(StripFileName(IncFileName$))

TabListCmbox.AddItems "inc"+chr$(160)+(IncFileName$)

SetTab25length

Tab25.TabIndex=Tab25.TabCount-1

End Sub 

'!******************************************
Sub CreateINCfileslist
'showmessage ("Create INC files list")
call  AddClrString ("3561:Create INC files list", clred, LogEdit)
Gauge1.visible=1
Gauge1.position=5
Gauge1.ForeColor=clg

for i%=0 to IncFilesCount-1
	IncFilesMnu(i%).visible=0
	IncFilesMnu(i%).caption=""
next i%

IncFilesFreeIndex=0
SEarchList.Text=SrcEdit.Text
SEarchList.Text=lcase$(SEarchList.Text)
for i=0 to SEarchList.ItemCount-1
	if instr ((SEarchList.Item(i)), "$include")>0 then
		if GetWord ( (SEarchList.Item(i)), "$include")>0 then
			
			if instr(SEarchList.Item(i),qt)>0  then
				IncFilesMnu(IncFilesFreeIndex).caption=field$(SEarchList.Item(i),qt,2)
			elseif instr(SEarchList.Item(i),"<")>0 then
				IncFilesMnu(IncFilesFreeIndex).caption=field$(SEarchList.Item(i),"<",2)-">"
			end if
			
			
			IncFilesMnu(IncFilesFreeIndex).visible=1
			
			'call  AddClrString ("4668:IncFilesMnu("+str$(IncFilesFreeIndex)+")="+(IncFilesMnu(IncFilesFreeIndex).caption), clred, LogEdit)
			IncFilesFreeIndex ++
		end if
	end if
	if i>oldi+150 then 
		oldi=i
		Gauge1.position=i/(SEarchList.ItemCount+1)*100
	end if
next i
'-----------
Gauge1.visible=0
Gauge1.ForeColor=clr

End Sub 

'!******************************************
Sub CreateINCfilesTree
'showmessage ("Create INC files list")
'call  AddClrString ("7953:Create INC files tree", cldg, LogEdit)
IncTreeView.clear
Gauge1.ForeColor=clg
needRefreshFlg=1
includeCount=0'1

LevelCount=256 
dim NodCountOnLevel (LevelCount) as long ' Index уровня для вставки child 
LevelIdx=0 ' номер уровня
ItemCount=0 ' общее число itemov

SearchIncTxt$=SrcEdit.Text
Gauge1.visible=1

while needRefreshFlg>0 
	
	Gauge1.position=2
	
	
	SEarchList.Text=SearchIncTxt$
	SEarchList.Text=lcase$(SEarchList.Text)
	'call  AddClrString ("7963:SEarchList.ItemCount="+str$(SEarchList.ItemCount), 0, LogEdit)
	
	for i=0 to SEarchList.ItemCount-1 '!!! ---
		if instr ((SEarchList.Item(i)), "$include")>0 then
			if GetWord ( (SEarchList.Item(i)), "$include")>0 then
				
				IncFilesName$=bspath(field$(SEarchList.Item(i),qt,2))
				
				if instr(SEarchList.Item(i),qt)>0  then
					IncFilesName$=bspath(field$(SEarchList.Item(i),qt,2))
				elseif instr(SEarchList.Item(i),"<")>0 then
					IncFilesName$=bspath(field$(SEarchList.Item(i),"<",2))'-">"
					IncFilesName$=left$(IncFilesName$,instr(IncFilesName$,">")-1)'-">"
				end if
				
				
				
				if instr(IncFilesName$,":")>0 then ' полный путь, сразу берем
				elseif fileexists(SrcFilePath$+IncFilesName$)>0 then ' надо проверять текущий 
					IncFilesName$=SrcFilePath$+IncFilesName$
				elseif fileexists(IncPathEdit.text+IncFilesName$)>0 then ' надо проверять inc path в опциях
					IncFilesName$=IncPathEdit.text+IncFilesName$
				end if
				
				if includeCount=0 then
					IncTreeView.AddItems IncFilesName$ '+chr$(160)+str$(IncTreeView.ItemCount)  ' str$(IncTreeView.ItemCount)
				else
					IncTreeView.AddChildItems includeCount-1,IncFilesName$ '+chr$(160)+str$(IncTreeView.ItemCount)
				end if
				NodCountOnLevel (LevelIdx)=IncTreeView.ItemCount
				
			end if
		end if
		if i>oldi+150 then 
			oldi=i
			Gauge1.position=i/(SEarchList.ItemCount+1)*100
		end if
	next i
	
	if includeCount=IncTreeView.ItemCount then 
		needRefreshFlg=0
	else
		'call  AddClrString ("8000:includeCount="+str$(includeCount), clp, LogEdit)
		'IncFilesName$=field$(IncTreeView.Item(includeCount).text,chr$(160),1)
		IncFilesName$=IncTreeView.Item(includeCount).text
		'call  AddClrString ("8000:IncFilesName$="+(IncFilesName$), clp, LogEdit)
		SearchIncTxt$=LoadString(IncFilesName$)
		slen=len(SearchIncTxt$)
		'call  AddClrString ("8004:slen="+str$(slen), cldg, LogEdit)
	end if
	'-----------
	oldi=0
	inc includeCount
	'call  AddClrString ("7997:includeCount="+str$(includeCount), clm, LogEdit)
	'call  AddClrString ("7998:IncTreeView.ItemCount="+str$(IncTreeView.ItemCount), clm, LogEdit)
	
wend

Gauge1.visible=0
Gauge1.ForeColor=clr
IncTreeView.FullExpand
End Sub 
'!*******************************
Sub IncFilesMnuOnClick

call CreateINCfilesTree
'call  AddClrString ("8019:Created  INCfilesTree=", clW, LogEdit)

'FileListPanel.visible=0
FilePanel.visible=1
ObjTreePanel.visible=1
IncTreeView.visible=1
SubsListBox(WindowsIndex).visible=0

End Sub 

'!******************************************
Sub CLoseRightPanelOnClick

FilePanel.visible=0 
if EditFlg>0 then ' если в редакторе был файл не из списка, то восстанавливаем файл из списка окон 
	LastWindClick:
	EditFlg=0
end if

End Sub 
'!******************************************
Sub ExpandRightPanelOnClick
FilePanel.width=250 
End Sub 
'!******************************************
Sub MinRightPanelOnClick
FilePanel.width=25 

End Sub 




'!******************************************
Sub ObjTreeViewDblClick

End Sub 
'!******************************************
Sub IncTreeViewDblClick
call  AddClrString ("9126:IncTreeViewDblClick=", clred, LogEdit)

filName$=IncTreeView.Item(IncTreeView.ItemIndex).text
call  AddClrString ("9227:filName$="+(filName$), clb, LogEdit)
filName$=bsPath(filName$)
if instr(filName$,":")=0 then
	filName$=SrcFilePath$+filName$
	
end if

FMngOpenFlg=1

call FileLoad

'SrcFileName=IncTreeView.Item(IncTreeView.ItemIndex).text

'exit sub


End Sub     

'!******************************************
SUB HLDrawCell (Col%, Row%, State%, Rect AS QRect, Sender AS QStringGrid)

with Sender
	fontcolor=HlColor(Row%-1)
	if col%=1 and row% >0  then
		.FillRect(Rect.Left, Rect.Top, Rect.Left+.ColWidths(Col%), Rect.Top+.RowHeights(Row%),fontcolor )
		.TextOut(Rect.Left+2, Rect.Top+2, .Cell(0, Row%), fontcolor, -1)
		
	end if
	if col%=0 and row%>0  then
		.FillRect(Rect.Left, Rect.Top, Rect.Left+.ColWidths(Col%), Rect.Top+.RowHeights(Row%),HlColor(0) )
		.TextOut(Rect.Left+2, Rect.Top+2, .Cell(0, Row%), fontcolor, -1)
		if row%=1 then .TextOut(Rect.Left+2, Rect.Top+2, .Cell(0, Row%), HlColor(9), -1)
		if row%=11 then 
			.FillRect(Rect.Left, Rect.Top, Rect.Left+.ColWidths(Col%), Rect.Top+.RowHeights(Row%),HlColor(10) )
			.TextOut(Rect.Left+2, Rect.Top+2, .Cell(0, Row%), HlColor(11), -1)
			
		end if
	end if
end with

end sub 

'!******************************************
SUB HLSelectCell ( Col%, Row%, CanSelect%) 
RowClr=row%
fontcolor=HLColor(Row%-1) 'HtmlColor2BGR (htclr$)
with TypeListGrid
	
	if col%=1  then
		.DelOptions(goEditing)
		if  ClrDlgOpen=0 then
			ClrDlgOpen=1
			ColorDialog.DefColor=HLColor(Row%-1)
			if ColorDialog.Execute(ColorDialogFrm) then
				'row%=RowClr
				'.FillRect(Rect.Left, Rect.Top, Rect.Left+.ColWidths(1), Rect.Top+.RowHeights(Row%),clred )
				HLColor(Row%-1)=ColorDialog.RgbColor
				ClrDlgOpen=0
			else
				ClrDlgOpen=0
			end if
		else
		end if
	else
		'.AddOptions(goEditing)
	end if
	srcedit.color=HLColor(0)
	BorderRich.color=srcedit.color
	MarginEdit.color=HLColor(10)
	MarginEdit.font.color=HLColor(11)
	LogEdit.color=HLColor(12)
	StrFont.color=HLColor(9)
	srcedit.Font.color=HLColor(9)
	'srcedit.Font.Name=.get("srcedit.Font.Name","FixedSys")
	'srcedit.Font.Size=val(.get("srcedit.Font.Size","10"))
	
	.repaint
	srcedit.visible=0
	srcedit.visible=1
	RichPanel.repaint
	
end with
end sub 




'!******************************************
Sub Str2ChrOnClick
if SrcEdit.SelLength>0 and trim$(SrcEdit.SelText)<>"" then 
	ClrStr1$=SrcEdit.SelText 
else 
	exit sub
end if
c$=""
for i=1 to len( ClrStr1$)-1
	c$=c$+"chr$("+str$( asc( ClrStr1$[i] )  )+")+"
	'print chr$(asc( ClrStr$[i] ))
next i
'print chr$(asc( ClrStr$[i] ))

c$=c$+"chr$("+str$( asc( ClrStr1$[i] )  )+")"'+"chr$(34)"
ClipboardtextOld$=Clipboard.text
Clipboard.text =c$
' print Clipboard.text
'SrcEdit.PasteFromClipboard
'Clipboard.text=ClipboardtextOld$

End Sub 

'!******************************************
Sub HLNewTypeNAme  (Col%, Row%, Value$)

End Sub 


'!******************************************
Sub LangMngFormResize

End Sub 
'!******************************************
Sub LangManagerOnClick
if LangMngForm.visible=false then LangMngForm.show

End Sub   

'!******************************************
Sub AddLangOnClick1
'LangGrid1.InsertCol(2)
LangGrid1.colcount=LangGrid1.colcount+1
LangGrid1.cell(LangGrid1.colcount,0) =SrcLangEdit.text ' LangNameCBox.Text '"English":


End Sub 
'!******************************************
Sub DelLangOnClick

if LangGrid1.Col>1 then
	LangGrid1.DeleteCol(LangGrid1.Col)
end if

End Sub 
'!******************************************
Sub SectionOnClick
swidth=SectionListBox.width
call  AddClrString ("12679:swidth="+str$(swidth), clred, LogEdit)
SectionListBox.width=600
LineN=val(field$(SectionListBox.Text,chr$(160),1))

SrcEdit.selstart=SrcEdit.GetLineIdx(SrcEdit.LineCount-1)
SrcEdit.selstart=SrcEdit.GetLineIdx(LineN-1)

SectionListBox.width=swidth


End Sub 

'!******************************************
Sub CreateSectionList
dim TMPList as QStringList
TMPList.text=SrcEdit.Text
Gauge1.visible=1
Gauge1.position=5
SectionListBox.Clear
iidx=0
oldi=0

for i=0 to TMPList.ItemCount-1 'TMPList
	LSrcEditLine$=LCase$(TMPList.Item(i))
	if instr(LSrcEditLine$,"!!!")>0 then 
		Item$=Str$(i+1)+chr$(160)+trim$(TMPList.Item(i))-ht
		
		SectionListBox.AddItems Item$
		inc iidx
	else
	end if
	if i>oldi+150 then 
		oldi=i
		Gauge1.position=i/(TMPList.ItemCount+1)*100
	end if
next i
Gauge1.position=0
Gauge1.visible=0
bzzz$=SectionListBox.Item(1)
SectionListBox.text="1"+chr$(160)+" Choose section"

End Sub 


'!******************************************
Sub ScanSrcLang
dim TMPList as QStringList
dim Oper1$(1) as string,OperPos(1) as integer
' а не забацать ли массив строковых перемнных для замены литералов???

idxs=0
numline=0 '

TMPList.text=SrcEdit.Text

' проверяем результаты предыдущего создания langpack и вписываем найденные переменные в соответствующие ячейки массива
for i=0 to TMPList.ItemCount-1 ' перебираем по строкам
	tmp1$=TMPList.Item(i)
	if tmp1$="" or instr (tmp1$,qt)=0 then goto nextiLl ' если строка пустая или нет кавычек - литералов, то пропускаем
	if instr (tmp1$,"LangVar$(")>0 and  instr (tmp1$,"Langini.get(")>0   then
		' переменная найдена
		' находим индекс в массиве
		aq=instr(tmp1$,"(")
		bq=instr(tmp1$,")")
		VarIdx$=mid$(tmp1$,aq+1, bq-aq-1)
		'call  AddClrString ("8678:VarIdx$="+(VarIdx$), clred, LogEdit)
		VarIdx=val(VarIdx$)
		'call  AddClrString ("8678:VarIdx="+str$(VarIdx), clred, LogEdit)
		aq=instr(tmp1$,qt)
		bq=instr(aq+1,tmp1$,qt)
		LVar$=mid$(tmp1$,aq+1, bq-aq-1)
		'call  AddClrString ("8682:LVar$="+(LVar$), clred, LogEdit)
		LangVar$(VarIdx)=LVar$
	end if
	nextiLl:
next i


'exit sub

' ищем первый свободный индекс
idxs=0
'call  AddClrString ("8695:idxs="+str$(idxs), clred, LogEdit)
while LangVar$(idxs)<>""
	idxs++
wend
'call  AddClrString ("8703:свободный idxs="+str$(idxs), cldg, LogEdit)

' search strings
'call  AddClrString ("8634:TMPList.ItemCount-1 ="+str$(TMPList.ItemCount-1 ), clb, LogEdit)

for i=0 to TMPList.ItemCount-1 ' перебираем по строкам
	qt2=1
	tmp1$=TMPList.Item(i)
	if tmp1$="" or instr (tmp1$,qt)=0 then goto nextiL
	
	numoper=tally(tmp1$,":")
	'call  AddClrString ("8713:numoper="+str$(numoper), clred, LogEdit)
	redim Oper1$(numoper) as string,OperPos(numoper) as integer
	GetOperators (Oper1$() ,OperPos() , tmp1$ ,@NumOper )
	
	for j=0 to NumOper-1
		' исключения 
		if instr(ucase$(Oper1$(j)),"$OPTION")<>0 then goto nextj9703
		if instr(ucase$(Oper1$(j)),"$RESOURCE")<>0 then goto nextj9703
		if instr(ucase$(Oper1$(j)),"$INCLUDE")<>0 then goto nextj9703
		if instr(ucase$(Oper1$(j)),"DECLARE")<>0 then goto nextj9703
		if instr(ucase$(Oper1$(j)),".FILTER")<>0 then goto nextj9703
		if instr(ucase$(Oper1$(j)),".SECTION")<>0 then goto nextj9703
		if instr(ucase$(Oper1$(j)),".FILENAME")<>0 then goto nextj9703
		if instr(ucase$(Oper1$(j)),".NAME")<>0 then goto nextj9703
		if instr(ucase$(Oper1$(j)),".FILEEXISTS")<>0 then goto nextj9703
		if instr(ucase$(Oper1$(j)),".LOADFROMFILE")<>0 then goto nextj9703
		if instr(ucase$(Oper1$(j)),".SAVETOFILE")<>0 then goto nextj9703
		if instr(ucase$(Oper1$(j)),".GET")<>0 then goto nextj9703
		if instr(ucase$(Oper1$(j)),".WRITE")<>0 then goto nextj9703
		'if instr(ucase$(Oper1$(j)),"OPTION")<>0 then goto nextj9703
		
		qt1=instr(tmp1$, qt) 
		'qt1=instr(tmp1$, Oper1$(j))
		while qt1 >0 and qt2>0 ' внутри строки ищем все строковые переменные
			'qt1=qtpos
			qt2=instr(qt1+1,Oper1$(j), qt)
			tmpstr$=mid$(Oper1$(j),qt1+1,qt2-qt1-1) ' 
			if trim$(tmpstr$)<>"" then
				for jj=0 to len(tmpstr$)
					chl$=tmpstr$[jj]
					if (asc(chl$)>63 and asc(chl$)<91) or (asc(chl$)>96 and asc(chl$)<123) or (asc(chl$)>191)  then
						goodword=1
						exit for
					else
						goodword=0
					end if
				next jj
				
				if goodword=0 then goto weeend:
				
				LangGrid2.cell(0,idxs+1)=str$(idxs)
				LangGrid2.cell(1,idxs+1)=str$(i+1)
				LangGrid2.cell(2,idxs+1)=tmpstr$'tmp1$-ht'tmpstr$
				LangGrid2.cell(3,idxs+1)=tmp1$-ht'tmpstr$
				
				LangGrid1.cell(2,idxs+1)="LangVar$("+str$(idxs)+")"
				
				' формируем набор переменных
				' надо вставить проверку дубликатов
				'if LangGrid1.col<3 then ShowMessage "Choose lanfuage before..": exit sub
				scanLngCol=SrcLang ' LangGrid1.col ' запоминаем номер колонки языка документа
				LangGrid1.cell(SrcLang,idxs+1)=tmpstr$ 
				LangGrid1.cell(1,idxs+1)=str$(i+1)
				'LangVar$(idxs)=
				idxs++
				while LangVar$(idxs)<>""
					idxs++
					' со строчками таблицы соотвествующими уже заполненным в предыдущий раз литералами надо что-то делать
				wend
				'idxs++
				'call  AddClrString ("8755:idxs="+str$(idxs), clb, LogEdit)
				if LangGrid2.rowcount<idxs then LangGrid2.rowcount=idxs
				if LangGrid1.rowcount<idxs then LangGrid1.rowcount=idxs
				weeend:
			end if
			qt1=instr(qt2+1,Oper1$(j), qt)
			doevents
			
		wend
		doevents
		nextj9703:
	next j
	nextiL:
next i

Gauge1.visible=0
iidx=0
oldi=0


if SortBtn.checked=1 then 
	LangGrid1.Enabled=0
	
	call SortBtnOnClick
	LangGrid1.Enabled=1
end if

if DelDupBtn.checked=1 then 
	LangGrid1.Enabled=0
	
	call DelDupBtnOnClick
	LangGrid1.Enabled=1
end if


SrcFilePath$=lcase$(StripPAth (bsPath(SrcFileName)))
LangFilePath$=SrcFilePath$+"language\"

' если существует файл сохраненной таблицы, то проверяем по нему перевод !!!
call  AddClrString ("9788:LangFilePath$+LangGrid1.tab="+(LangFilePath$+"LangGrid1.tab"), cldp, LogEdit)

' загружаем файл в теневую таблицу
if fileexists(LangFilePath$+"LangGrid1.tab")>0 then
	LangGrid1h.loadfromfile (LangFilePath$+"LangGrid1.tab",0,0,30000)
else
	showmessage "12750: File not found:"+LangFilePath$+"LangGrid1.tab"
end if

' проверяем src lang ячейки и при совпадении переписываем перевод
' из теневой таблицы в текущую
if SrcLang=3 then DestLang=4 else DestLang=3

for j=0 to LangGrid1.RowCount-1
	
	for i=0 to LangGrid1h.RowCount-1
		if LangGrid1h.cell(SrcLang,i)=LangGrid1.cell(SrcLang,j) then
			LangGrid1.cell(DestLang,j)=LangGrid1h.cell(DestLang,i)
		end if
	next i
next j

' ставим галки там где заполнены английская и русская ячейки
for j=0 to LangGrid1.RowCount-1
	if LangGrid1.cell(3,j)<> "" and LangGrid1.cell(4,j)<>"" then
		LangGrid1.cell(0,j)="1"
	end if
next j


End Sub 


'!******************************************
Sub LangGrid1DrawCell (Col%, Row%, State%, Rect AS QRect, Sender AS QStringGrid)
ColGalka=0
GalClr= clRed
with sender
	
	if  col%=SrcLang and row%=0 then ' подсветка языка источника
		.FillRect(Rect.Left, Rect.Top, Rect.Left+.ColWidths(Col%), Rect.Top+.RowHeights(Row%), cla)
		.TextOut(Rect.Left+2, Rect.Top+2, .Cell(Col%, Row%), Clb, -1)
	end if
	
	
	if  col%=0   then ' рисование галки
		
		
		.FillRect(Rect.Left, Rect.Top, Rect.Left+.ColWidths(Col%), Rect.Top+.RowHeights(Row%), clSilver)
		.Rectangle(Rect.Left+2, Rect.Top+2, Rect.Left+18, Rect.Top+18, 0)
		
		
		if .Cell(ColGalka, Row%)="1" or Row%=0 then 
			.Line(Rect.Left+2, Rect.Top+9, Rect.Left+9, Rect.Top+16, GalClr)
			.Line(Rect.Left+2, Rect.Top+10, Rect.Left+9, Rect.Top+17, GalClr)
			.Line(Rect.Left+9, Rect.Top+16, Rect.Left+16, Rect.Top+4, GalClr)
			.Line(Rect.Left+8, Rect.Top+16, Rect.Left+16, Rect.Top+3, GalClr)
			
		else
			.FillRect(Rect.Left, Rect.Top, Rect.Left+.ColWidths(Col%), Rect.Top+.RowHeights(Row%), clSilver)
			.Rectangle(Rect.Left+2, Rect.Top+2, Rect.Left+18, Rect.Top+18, 0)
			'.Rectangle(Rect.Left+.ColWidths(Col%)/6, Rect.Top+.RowHeights(Row%)/6, Rect.Left+.ColWidths(Col%)/6*5, Rect.Top+.RowHeights(Row%)/6*5, 0)
		end if
	end if
	
	
end with 
'call  AddClrString ("8767:col%="+str$(col%), clred, LogEdit)

End Sub 

'!******************************************
Sub LangGrid1SelectCell  (Col% , Row%, CanSelect%, Sender AS QStringGrid) 
with sender
	if  col%=0 then  
		'call  AddClrString ("8737:col%="+str$(col%), clred, LogEdit)
		LangGrid1.delOptions (goEditing)
		'call  AddClrString ("8740:LangGrid1.Cell(0,"+str$(Row%)+")="+(LangGrid1.Cell(0, Row%)), clred, LogEdit)
		if LangGrid1.Cell(0, Row%)="1" then 
			LangGrid1.Cell(0, Row%)="0" 
		else 
			LangGrid1.Cell(0, Row%)="1"
		end if
		
	else
		LangGrid1.AddOptions (goEditing)
		SrcLangEdit.text=LangGrid1.Cell(col%, 0)
		
	end if
end with
LineN=val(LangGrid1.cell(1,Row%))+numLine '+1
'call  AddClrString ("8852:LineN="+str$(LineN), clred, LogEdit)
'call  AddClrString ("8852:numLine="+str$(numLine), cl0, LogEdit)
SrcEdit.selstart=SrcEdit.GetLineIdx(SrcEdit.LineCount-1)
SrcEdit.selstart=SrcEdit.GetLineIdx(LineN-1)

for i=0 to LangGrid2.RowCount-1
	if LangGrid1.cell(1,Row%)=LangGrid2.cell(1,i) then
		LangGrid2.TopRow=i
	end if
next i
' проверка была ли обработана данная переменная 
if col%>2 then
	if LangGrid1.cell(col%,Row%)="" then
		currVArr$=LangGrid1.cell(3,Row%)
		currNac$=""
		
		for i=1 to LangGrid1.RowCount-1
			if ucase$(LangGrid1.cell(3,i))=ucase$(currVArr$) then
				if LangGrid1.cell(col%,i) <>"" and currNac$="" then
					currNac$=LangGrid1.cell(col%,i)
				elseif LangGrid1.cell(col%,i) <>"" and currNac$<> "" and ucase$(currNac$)<>ucase$(LangGrid1.cell(col%,i)) then
					showmessage ("Variable "+LangGrid1.cell(3,i)+"already define as "+LangGrid1.cell(col%,i))
				elseif LangGrid1.cell(col%,i) ="" and currNac$<>"" then
					LangGrid1.cell(col%,i) =currNac$
					
				end if
				
			end if
		next i
		
	end if
end if

call  AddClrString ("9893:SrcLang="+str$(SrcLang), clred, LogEdit)
call  AddClrString ("9893:Col%="+str$(Col%), clred, LogEdit)

'if Col%=SrcLang then
'LangNameCBox.checked=1
'else
'LangNameCBox.checked=0
'end if

End Sub 

'!******************************************
Sub LangGrid2SelectCell  (Col% , Row%, CanSelect%, Sender AS QStringGrid) 
LineN=val(LangGrid2.cell(1,Row%))+numLine
SrcEdit.selstart=SrcEdit.GetLineIdx(SrcEdit.LineCount-1)
SrcEdit.selstart=SrcEdit.GetLineIdx(LineN-1)

for i=0 to LangGrid1.RowCount-1
	if LangGrid1.cell(1,i)=LangGrid2.cell(1,Row%) then
		LangGrid1.TopRow=i
	end if
next i
End Sub 

'!******************************************
Sub LangGrid2DrawCell (Col%, Row%, State%, Rect AS QRect, Sender AS QStringGrid)

End Sub 

'!******************************************
Sub LangGrid2SetEditText (Col%, Row%, Value$)
LineN=val(LangGrid2.cell(1,Row%))
SrcEdit.Line(LineN-1)=LangGrid2.cell(3,Row%)

End Sub 


'!******************************************
Sub CreateLangPack
Dim TmpList as QStringList
with LangGrid1
	
	'* заменяем литералы на строковые переменные
	call  AddClrString ("8938: заменяем литералы на строковые переменные=", clred, LogEdit)
	
	' -- заменяем строковые на сформированные переменные ---
	TmpList.text=SrcEdit.text
	scanLngCol=SrcLang 'LangGrid1.col
	call  AddClrString ("9857:scanLngCol="+str$(scanLngCol), clb, LogEdit)
	
	if  scanLngCol<>3 and scanLngCol<>4 then
		showmessage "Choose language first in table"
		exit sub
	end if
	notick=1
	'call  AddClrString ("8941:numLine="+str$(numLine), clp, LogEdit)
	for rowi=1 to .RowCount-1
		if .cell(0,rowi)="1" then
			notick=0
			exit for
		end if
	next rowi
	if notick=1 then
		showmessage "No rows marked!"
		exit sub
	end if
	
	for rowi=1 to .RowCount-1
		if .cell(0,rowi)="1" then
			'call  AddClrString ("8931:.cell(1,"+str$(rowi)+")="+(.cell(1,rowi)), clb, LogEdit)
			ItIdx=numLine+val(.cell(1,rowi))-1 ' номер строки
			'call  AddClrString ("8940:ItIdx="+str$(ItIdx), clp, LogEdit)
			' ищем литерал для замены из нижней таблицы
			for row2=1 to LangGrid2.RowCount-1
				if LangGrid2.cell(1,row2)=LangGrid1.cell(1,rowi) then
					sLiter$=LangGrid2.cell(2,row2) ' +" ' "+sLiter$
					exit for
				end if
			next row2
			'sLiter$=.cell(scanLngCol,rowi)
			TmpList.Item (ItIdx)=replaceSubStr$(TmpList.Item (ItIdx),qt+sLiter$+qt,.cell(2,rowi) )+ " ' " +.cell(2,rowi)+"="+ sLiter$
			
		else
		end if
		
	next rowi
	
	SrcEdit.text=TmpList.text
	
	'exit sub
	
	LangIniHeaderPos=instr(TmpList.text,"$INCLUDE <LangIniHeader.inc>")
	'call  AddClrString ("8963:LangIniHeaderPos="+str$(LangIniHeaderPos), cldb, LogEdit)
	
	if LangIniHeaderPos=0 then
		'!!! -------- шапка ---------------
		ClrStr$=crlf+"$INCLUDE <LangIniHeader.inc>"+crlf
		
		' * ищем ''!!!--- LangIniHeader ---
		OldClipBrd$=clipboard.text
		OldSearch$=SearchCBox.text
		SearchCBox.text="'!!!--- LangIniHeader ---"
		
		ClipBoard.Text=ClrStr$
		
		SrcEdit.SelStart=0
		SrcEdit.SelLength=0
		call FindText (FindBtnDn)
		'GoToLine (LineNumber)
		'call  AddClrString ("8911:SrcEdit.SelStart="+str$(SrcEdit.SelStart), clred, LogEdit)
		
		SrcEdit.SelLength=0
		SrcEdit.GoToLineEnd
		'SrcEditSelStart=SrcEdit.SelStart
		'* вставляем шапку
		SrcEdit.PasteFromClipboard
		'SrcEdit.SelStart=SelSt+len(ClrStr$)-1
		
		'call  AddClrString ("8919:SelSt="+str$(SelSt), cldg, LogEdit)
		
		SearchCBox.text=OldSearch$
	else
		' идем в конец существующей строки заголовка 
		SrcEdit.SelStart=LangIniHeaderPos
		SrcEdit.SelLength=0
		SrcEdit.GoToLineEnd
		
	end if
	
	'!!!--- вставляем имена языковых файлов 
	'English$=Langini.get("English","English")
	'Russian$=Langini.get("Russian","Russian")"
	ClrStr$=crlf
	' формируем список языков 
	for coli=3 to  .ColCount-1
		adlngstr$="$=Langini.get("+qt+.cell(coli,0) +qt+","+qt+.cell(coli,0)+qt+")"
		if instr(TmpList.text,adlngstr$)=0 then ' если языка нет, то добавляем
			ClrStr$=ClrStr$+.cell(coli,0)+adlngstr$+crlf
		end if
	next coli
	ClrStr$=ClrStr$+crlf
	
	OldClipBrd$=clipboard.text
	ClipBoard.Text=ClrStr$
	'* вставляем
	SrcEdit.PasteFromClipboard
	
	numLine=numLine+5
	'!!!--- вставляем список переменных на английском
	ClrStr$="" 'crlf
	
	' numLine=12
	OldVAr$=""
	for rowi=1 to .RowCount-1
		if .cell(0,rowi)="1"  and OldVAr$<>.cell(3,rowi) then
			adValstr$="=Langini.get("+qt+.cell(3,rowi)+qt+","+qt+.cell(3,rowi)+qt+")"
			if instr(TmpList.text,adValstr$)=0 then ' если переменной нет, то добавляем
				ClrStr$=ClrStr$+ .cell(2,rowi)+adValstr$ +crlf
			end if
			OldVAr$=.cell(3,rowi)
			numLine++
			'numLine++
			'Call  AddClrString ("8918:numLine ="+str$(numLine ), clred, LogEdit)
		else
		end if
		
	next rowi
	
	
	clipboard.text=ClrStr$
	SrcEdit.PasteFromClipboard
	
	clipboard.text=OldClipBrd$
	'numLine=numLine
	
	'-- формируем языковые файлы ---------
	
	
	
end with

' сохраняем таблицу с переводом

LangGrid1.savetofile (LangFilePath$+"LangGrid1.tab",0,0,LangGrid1.Rowcount)
LangGrid2.savetofile (LangFilePath$+"LangGrid2.tab",0,0,LangGrid2.Rowcount)

End Sub 


'!******************************************
Sub ClearLangGrids

for i=0 to LangGrid1.ColCount-1
	for j=1 to LangGrid1.RowCount-1
		LangGrid1.cell(i,j)=""
	next j
next i

for j=0 to 1023
	LangVar$ (j)=""
next j

LangGrid1.RowCount=5

for i=0 to LangGrid2.ColCount-1
	for j=1 to LangGrid2.RowCount-1
		LangGrid2.cell(i,j)=""
	next j
next i

LangGrid2.RowCount=5
numline=0

End Sub 

'!******************************************
Sub SortBtnOnClick

'if SortBtn.checked=0 then exit sub

Dim TmpList as QStringList
DIM Mem AS QMemoryStream
Slst$=""
TmpList.clear
'TmpList.AddItems "                Заголовок пропускаем"

LangGrid1.SwapCols (0,LangGrid1.col)

for ri=1 to LangGrid1.RowCount-1
	for ci=0 to LangGrid1.ColCount-1
		Slst$=Slst$+LangGrid1.cell(ci,ri)+LangGrid1.Separator
	next ci
	TmpList.AddItems Slst$
	Slst$=""
	
next ri
TmpList.Sort

for ri1=0 to TmpList.ItemCount-1
	for ci1=0 to LangGrid1.ColCount-1
		Slst$=TmpList.Item(ri1)
		SCell$=field$(Slst$,LangGrid1.Separator,ci1+1)
		LangGrid1.cell(ci1,ri1+1)=SCell$
	next ci1
	
next ri1

LangGrid1.SwapCols (0,LangGrid1.col)

End Sub 
'!******************************************
Sub SortLabelOnClick(Sender as QLabel)

End Sub 

'!******************************************
Sub DelDupBtnOnClick
'call  AddClrString ("9012:DelDupBtnOnClick=", clred, LogEdit)
if DelDupBtn.checked=0 then exit sub

for ri=1 to LangGrid1.RowCount-1
	if lcase$(LangGrid1.cell(LangGrid1.col,ri))=lcase$(LangGrid1.cell(LangGrid1.col,ri-1)) then
		LangGrid1.cell(2,ri)=LangGrid1.cell(2,ri-1)
	end if
next ri

End Sub 


'!******************************************
Sub SwapcellBtnOnClick

swap LangGrid1.cell(LangGrid1.col,LangGrid1.row), LangGrid1.cell(3,LangGrid1.row)

if LangGrid1.row<LangGrid1.rowCount-1 then
	LangGrid1.row=LangGrid1.row+1
end if

End Sub 
'------- 
'!******************************************
Sub SaveLangOnClick
LangFileContent$ =""

LangFilePath$=SrcFilePath$+"language\"
call  AddClrString ("10085:LangFilePath$="+(LangFilePath$), clred, LogEdit)
MKSubDir (LangFilePath$)

'--- сохраняем талицы ---
' сохраняем таблицу с переводом

LangGrid1.savetofile (LangFilePath$+"LangGrid1.tab",0,0,LangGrid1.Rowcount)
LangGrid2.savetofile (LangFilePath$+"LangGrid2.tab",0,0,LangGrid2.Rowcount)


AlreadyVAr$=""
call  AddClrString ("10205:AlreadyVAr="+str$(AlreadyVAr), clred, LogEdit)

with LangGrid1
	
	for LangIdx=3 to 4 ' LangGrid1.RowCount-1 английский и русский
		LangFileName$=LangFilePath$+LangGrid1.cell(LangIdx,0)+"04.lng"
		call  AddClrString ("9146:LangFileName$="+(LangFileName$), clred, LogEdit)
		LangFileContent$ =""
		AlreadyVAr$=""
		if fileexists(LangFileName$)<>0 then
			IF MessageDlg("File "+LangFileName$ +"alredy exists. Overwrite?", mtWarning, mbYes OR mbNo, 0) = mrYes THEN
				'elseif !!!
				' добавить к существующему
				
			else
				Showmessage ("Rename existing file")
				exit sub
			END IF
		END IF
		
		for ri=1 to LangGrid1.RowCount-1
			if .cell(0,ri)="1" then
				if AlreadyVAr$=LangGrid1.cell(2,ri) then goto nextri:
				call  AddClrString ("9068:ri="+str$(ri), clred, LogEdit)
				lngs$=LangGrid1.cell(3,ri)+"="+LangGrid1.cell(LangIdx,ri)
				call  AddClrString ("9081:lngs$="+(lngs$), clred, LogEdit)
				LangFileContent$ =LangFileContent$+lngs$ +crlf
				AlreadyVAr$=LangGrid1.cell(2,ri)
			end if
			nextri:
		next ri
		existingFile$=LoadString (LangFileName$)
		if existingFile$="0" then existingFile$=""
		LangFileContent$=existingFile$+ crlf+LangFileContent$
		zz=SaveString (LangFileContent$,LangFileName$)
	next LangIdx
end with

'call  AddClrString ("9069:LangFileContent$="+(LangFileContent$), clred, LogEdit)
'print "LangFileContent$=";LangFileContent$

'zz=SaveString (LangFileContent$,LangFileName$)
'call  AddClrString ("9077:zz="+str$(zz), clred, LogEdit)

'if zz<> 1 then showmessage ("Can't save file "+ LangFileName$)

' сохраняем текущую таблицу и словарь


End Sub 
'!******************************************
Sub LoadGridBtnOnClick
' загружаем таблицы с переводом
LangFilePath$=SrcFilePath$+"language\"
call  AddClrString ("9141:SrcFilePath$="+(SrcFilePath$), clred, LogEdit)
'MKSubDir (LangFilePath$)

if fileexists(LangFilePath$+"LangGrid1.tab")>0 then
	LangGrid1.loadfromfile (LangFilePath$+"LangGrid1.tab",0,0,30000)
else
	showmessage "13224:File not found:"+LangFilePath$+"LangGrid1.tab"
end if

if fileexists(LangFilePath$+"LangGrid2.tab")>0 then
	LangGrid2.loadfromfile (LangFilePath$+"LangGrid2.tab",0,0,30000)
else
	showmessage "13230:File not found:"+LangFilePath$+"LangGrid2.tab"
end if

End Sub 

'!******************************************
Sub CodPChange (sender as QButtonXP)
' шрифт должен выбираться из настроек !!!!!
Select case sender.tag
case 10
	'StrFont.name ="FixedSys"
	'StrFont.size=10
	
	HiLiteFont.Name=StrFont.name'"FixedSys"
	HiLiteFont.size=StrFont.size
	codp$="win"
	
case 11
	'StrFont.name ="Terminal"
	'StrFont.size=14
	'!HiLiteFont.Name="Terminal"
	'!HiLiteFont.size=StrFont.size
	codp$="dos"
	dostxt$=SrcEdit.Text
	wintxt$=dostxt$+" "
	oemtoChar wintxt$,dostxt$
	SrcEdit.Text=wintxt$
case 12
	'StrFont.name ="Arial KOI-8"
	'StrFont.size=10
	HiLiteFont.Name="Arial KOI-8"
	HiLiteFont.size=StrFont.size
	codp$="koi"
case 13
	call QPDecode
case 14
	call UTF8Decode
	
case else
	
end select

nopaint=1
MarginEdit.HideSelection=1
SrcEdit.HideSelection=1

CurPos=SrcEdit.SelStart
SrcEdit.Font=HiLiteFont
SrcEdit.SelectAll
SrcEdit.SelAttributes=StrFont
SrcEdit.SelLength=0

MarginEdit.Font=StrFont
MarginEdit.SelectAll
MarginEdit.SelAttributes=StrFont
MarginEdit.SelLength=0

nopaint=0
SrcEdit.HideSelection=0
MarginEdit.HideSelection=0


End Sub 

'!************************************************************************'
function  LCaseR$ (Cases$) as string
Cases$=lcase$(Cases$)
for i= 192 to 223
	Cases$=ReplaceSubStr$(Cases$, chr$(i), chr$(i+32))
next 
LCaseR$=Cases$
end function

'!************************************************************************'
function UCaseR$ (Cases$)  as string
Cases$=ucase$(Cases$)
for i= 224 to 255
	Cases$=ReplaceSubStr$(Cases$, chr$(i), chr$(i-32))
next
UCaseR$=Cases$
end function

'!************************************************************************'
function  LCaseROEM$ (Cases$) as string
Cases$=lcase$(Cases$)
for i= 128 to 143
	Cases$=ReplaceSubStr$(Cases$, chr$(i), chr$(i+32))
next 

for i= 144 to 159
	Cases$=ReplaceSubStr$(Cases$, chr$(i), chr$(i+80))
next 
Cases$=ReplaceSubStr$(Cases$, chr$(240), chr$(241))


LCaseR$=Cases$
end function

'!************************************************************************'
function UCaseROEM$ (Cases$)  as string
Cases$=ucase$(Cases$)

for i= 160 to 175
	Cases$=ReplaceSubStr$(Cases$, chr$(i), chr$(i-32))
next

for i= 224 to 239
	Cases$=ReplaceSubStr$(Cases$, chr$(i), chr$(i-80))
next
Cases$=ReplaceSubStr$(Cases$, chr$(241), chr$(240))

UCaseR$=Cases$
end function
'!******************************************
Sub Win2DOS

Chartooem AddInText,AddInText

SrcEdit.Seltext= AddInText
End Sub 
'!******************************************
Sub DOS2Win

oemtoChar AddInText,AddInText
'call  AddClrString ("9507:AddInText="+(AddInText), clred, LogEdit)

SrcEdit.Seltext=AddInText

End Sub 
'!******************************************
Sub FindAll

End Sub 
'!******************************************
Sub ReFormatC
'WaitForm.show
nopaint=1
CurPos=SrcEdit.SelStart
SrcEdit.Font=StrFont
SrcEdit.SelectAll
SrcEdit.SelAttributes=StrFont
SrcEdit.SelLength=0
nopaint=0

call  AddClrString ("7443: Deleting hot tabs. Wait please...", clM, LogEdit)
SrcEdit.Text=ReplaceSubStr$(SrcEdit.Text,ht," ")
call  AddClrString ("7445: Deleting Led Spaces. Wait please...", clM, LogEdit)
call DeleteLedSpaces
call  AddClrString ("13729: Deleted Led Spaces. Wait please...", clM, LogEdit)

dim FakeList as QStringList
dim NewFakeList as QStringList
NewFakeList.clear

dim TmpList as QStringList ' для разворачивания строки с множеством скобок
TmpList.clear

'string1$=SrcEdit.Text

SrcEdit.HideSelection=1: SrcEdit.Enabled=0


FakeList.text=SrcEdit.Text 'string1$
oldi=0

'string1$=ReplaceSubStr$(string1$,"/{",chr$(134))
'string1$=ReplaceSubStr$(string1$,"/}",chr$(135))
'string1$=ReplaceSubStr$(string1$,"//",chr$(136))

'string1$=ReplaceSubStr$(string1$,"{",crlf+"{"+crlf)
'string1$=ReplaceSubStr$(string1$,"}",crlf+"}"+crlf)


for i=0 to FakeList.ItemCount-1  '  !!!------------------------------- --
	
	string1$=FakeList.Item(i)
	
	if tally(string1$,"{")<> tally(string1$,"}") then
		if len (string1$-ht-" ")>1 then
			string1$=ReplaceSubStr$(string1$,"{",crlf+"{"+crlf)
			string1$=ReplaceSubStr$(string1$,"}",crlf+"}"+crlf)
		end if
	end if
	'call  AddClrString ("13753:string1$="+(string1$), clred, LogEdit)
	
	NewFakeList.AddItems (string1$)
next i

'Clipboard.Text=string1$
FakeList.Text=NewFakeList.Text 'string1$
NewFakeList.clear
indent$=""

'SrcEdit.Text=FakeList.Text

for i=0 to FakeList.ItemCount-1  '  !!!------------------------------- --
	string1$=FakeList.Item(i)
	
	if left$(FakeList.Item(i)-ht,1)="{" then ' увеличиваем отступ
		
		if tally(string1$,"{")<> tally(string1$,"}") then
			
			FakeList.Item(i)=indent$+FakeList.Item(i)
			'SrcEdit.Text=FakeList.Text
			indent$=indent$+ht
		else
			FakeList.Item(i)=indent$+FakeList.Item(i)
			'SrcEdit.Text=FakeList.Text
			
		end if
		
	elseif left$(FakeList.Item(i)-ht,1)="}" then ' уменьшаем отступ
		
		if tally(string1$,"{")<> tally(string1$,"}") then
			indent$=left$(indent$,len(indent$)-1)
			FakeList.Item(i)=indent$+FakeList.Item(i)
			'SrcEdit.Text=FakeList.Text
		else
			FakeList.Item(i)=indent$+FakeList.Item(i)
			'SrcEdit.Text=FakeList.Text
		end if
		
	else
		FakeList.Item(i)=indent$+FakeList.Item(i)
		'SrcEdit.Text=FakeList.Text
	end if
	
	
next i
SrcEdit.HideSelection=0:SrcEdit.Enabled=1

SrcEdit.Text=FakeList.Text

badd$=""
eadd$=""

z1=instr(string1$, "{")
z2=instr(string1$, "}")

cz1=tally(string1$, "{")
'call  AddClrString ("8107:cz1="+str$(cz1), clred, LogEdit)
cz2=tally(string1$, "}")
'call  AddClrString ("8109:cz2="+str$(cz2), clred, LogEdit)

'exit sub
nopaint=0
if SrcEdit.Modified=1 then 
	'IF Modiflg=1 THEN
	call AddSubs
	Modiflg=0
	'exit sub 
end if


End Sub 
'!******************************************
Sub H2IncOnClick
dim FakeList as QStringList
string1$=lcase$(SrcEdit.Text)
string1$=ReplaceSubStr$(string1$,"//","'") ' заменяем комментарии
string1$=ReplaceSubStr$(string1$,ht," ") ' удаляем табы

string1$=ReplaceSubStr$(string1$,"struct","Type") ' 
typeflg=0 ' счетчик вложенности типов
Figskob=0 ' счетчик вложенности скобок
FigskobFlg=0 ' флаг - внутри скобок

FakeList.Text=string1$

for i=0 to FakeList.ItemCount-1  '  !!!------------------------------- --
	line$=FakeList.Item(i)
	if instr(line$, "Type")>0 then typeflg=typeflg+1 ' начался тип, ожидаем его окончания
	if instr(line$, "{")>0 and instr(line$, "}")=0 then Figskob=Figskob+1: FigskobFlg=1' началась фигурнаф скобка, ожидаем окончания
	if instr(line$, "{")=0 and instr(line$, "}")>0 then Figskob=Figskob-1 ' началась фигурнаф скобка, ожидаем окончания
	
	if Figskob=0 and FigskobFlg=1 and typeflg>0 then ' вышли из вложенных фигурных скобок
		FakeList.Item(i)=replacesubstr$(line$,"}","End Type")
		typeflg=typeflg-1
		FigskobFlg=0 ' вышли из скобок
	elseif typeflg>0 and FigskobFlg=1 then
		type1$=field$(trim$(line$)," ",1)
		var1$=field$(trim$(line$)," ",2)
		var1$=field$(trim$(var1$),";",1)
		comment1$=field$(trim$(var1$),";",2)
		line1$=var1$+" as "+type1$+" ' "+comment1$
		FakeList.Item(i)=line1$
		call  AddClrString ("10365:line1$="+(line1$), clred, LogEdit)
	else
		
	end if
	
next i

for i=0 to FakeList.ItemCount-1  '  !!!------------------------------- --
	
next i

SrcEdit.Text=FakeList.Text


End Sub 
'!******************************************
Sub SetDefLng
if LangGrid1.col>2 then
	SrcLang=LangGrid1.col
	call  AddClrString ("10525:SrcLang="+str$(SrcLang), clb, LogEdit)
end if
LangGrid1.repaint
End Sub 
'!******************************************
Sub SrcLangEditOnKeyUp(Key AS Word, Shift AS INTEGER)
with LangGrid1
	
	if LangGrid1.col>2 then
		.cell(.col,0)=SrcLangEdit.text
	end if
	
end with

End Sub 
'!******************************************
Sub DElRowOnClick

End Sub 
'!******************************************
Sub DElEmptyRowsOnClick

End Sub 
'!******************************************
Sub QPDecode
headerTxtConv$=""
encodedword$=SrcEdit.text
HexCharCount=tally(encodedword$,"=")
for ii=1 to HexCharCount
	HexChar2$=ucase$(field$(encodedword$,"=",ii+1))
	HexChar$=left$(HexChar2$,2)
	Ost1$=right$(HexChar2$,len(HexChar2$)-2)
	headerTxtConv$=headerTxtConv$+chr$(val(CONVBASE$(HexChar$, 16, 10)))+Ost1$
next ii
SrcEdit.text=headerTxtConv$ 
End Sub 

'!******************************************
Sub FileHEXLoad
FMngOpenFlg=OpenDialog.Execute '!!!---
call  AddClrString ("13470:FMngOpenFlg="+str$(FMngOpenFlg), clred, LogEdit)
OldFilter$=OpenDialog.Filter
Hexfilname$=OpenDialog.FileName

End Sub 
'!******************************************
Sub WinBtnOnmove (X%, Y%, Shift%, sender as QLabel)
select case sender.tag
case 61
	sender.color=clr
case 62
	sender.color=clr
	
end select

End Sub 
'!******************************************
SUB Tab25Change
call  AddClrString ("13717:Tab25Change=", cldb, LogEdit)

fle=-1
'EditFlg=1
MarginEdit.color=HLColor(10)

'call  AddClrString ("13195:Tab25Change-   TabListCmbox.Item("+str$(Tab25.TabIndex)+")="+(TabListCmbox.Item(Tab25.TabIndex)), clm, LogEdit)
tbcomboFlname$=trim$(field$(TabListCmbox.Item(Tab25.TabIndex),chr$(160),2))
call  AddClrString ("14356:Tab25.TabIndex="+str$(Tab25.TabIndex), cldg, LogEdit)
call  AddClrString ("13735:tbcomboFlname$="+(tbcomboFlname$), cldp, LogEdit)

for i333=0 to WindowsItemCount-1
	SrcFName$=field$(WindMnu(i333).caption,chr$(160),2) '
'call  AddClrString ("14361:WindMnu("+str$(i333)+").caption="+(WindMnu(i333).caption), cldg, LogEdit)
	'call  AddClrString ("13205:SrcFName$ i="+str$(i)+" "+(SrcFName$), clo, LogEdit)
	WindMnu(i333).checked=0
	if trim$(SrcFName$)=tbcomboFlname$ then '!!! если имя файла во вкладке содержится в списке окон, то загружаем текст из окна в редактор
		fle=i333 ' индекс открытой вкладки
		'!!! EditFlg=0
		'call  AddClrString ("13660:fle="+str$(fle), cldg, LogEdit)
		'call WindChoose (WindMnu(fle) )
		'call  AddClrString ("13219:WindMnu("+str$(fle)+")="+(WindMnu(fle).caption), clred, LogEdit)
		exit for
		
	elseif trim$(field$(TabListCmbox.Item(Tab25.TabIndex),chr$(160),1))="inc" then
		'!!! EditFlg=2
		
	else 'окно не открыто
		
		OpenDialog.FileName=tbcomboFlname$
		'!!! EditFlg=1
		'call  AddClrString ("13669:tbcomboFlname$="+(tbcomboFlname$), cldb, LogEdit)
		
	end if
	
	
next i333

call  AddClrString ("13751:fle="+str$(fle), clred, LogEdit)


if fle=-1 then '!!! вкладка не находится в списке окон - это временная вкладка для просмотра, ее надо снова загрузить из файла
	
	SrcEdit.loadfromFile (tbcomboFlname$)  ' (IncFilesMnu(.MenuIndex).caption)
	StatusPanel5.Caption=tbcomboFlname$
	EditFlg=2
else
	'WindowsIndex=Sender.MenuIndex-InsIdx  ?? !!!! 
	SrcFileName=field$(WindMnu(fle).caption,chr$(160),2) '
	'call  AddClrString ("13221:SrcFileName="+(SrcFileName), clred, LogEdit)
	WindMnu(WindowsIndex).checked=0
	'WindowsIndex=fle
	'!!!!!WindMnu(fle).checked=1
	'call  AddClrString ("13632:WindMnu("+str$(fle)+")checked)="+str$(WindMnu(fle).checked), clred, LogEdit)
	
	WindChoose (WindMnu(fle))
	
end if

'call  AddClrString ("13772:дергаем tab sub list=", clb, LogEdit)
TabRightChange ' дергаем tab sub list обновляем
HtHLBoxClick

endsub13233:
END SUB

'!******************************************
Sub SaveIDEOptions

ThemeChange

End Sub 

'!******************************************
Sub UseTheme


End Sub 


'!******************************************
Sub ThemeChange
call  AddClrString ("11410:ThemeChange="+str$(ThemeChange), clg, LogEdit)
call  AddClrString ("11611:ClrSchComBox.Item("+str$(ClrSchComBox.ItemIndex)+")="+(ClrSchComBox.Item(ClrSchComBox.ItemIndex)), cldp, LogEdit)

if ClrSchEdit.Text="" then 
	ShowMessage("11542: Input Color Schem name"):exit sub
	
	'ClrSchEdit.Text="Classic Windows"
	'call  AddClrString ("11441:Old ClrSchEdit.Text="+(ClrSchEdit.Text), clp, LogEdit)
end if

if ClrSchComBoxText$=""  then ClrSchComBoxText$=ClrSchEdit.Text




with RQdbini
	
	'!!!  сохраняем список схем из комбобокса
	.Section="ColorSchems"
	for i=0 to ClrSchComBox.ItemCount-1
		.write("Scheme_"+str$(i),ClrSchComBox.Item(i))
	next i
	.write("SchemeCount",str$(ClrSchComBox.ItemCount))
	
	' сначала надо сохранить изменения, потом загружать вновь выбранную
	
	.Section="Color Schem"
	.write("Color Schem",ClrSchComBoxText$) ' сохраняем название схемы  
	call  AddClrString ("11397:Save current schem name="+(ClrSchComBoxText$), clred, LogEdit)
	
	.Section=ClrSchComBoxText$ ' старое значение ClrSchComBox.Text
	call  AddClrString ("11356: Save current schem in RQdbini.Section="+(RQdbini.Section), clred, LogEdit)
	
	.write("BackGroung",strl$(HLColor (0)))
	.write("KeyWords",strl$(HLColor (1)))
	.write("Operators",strl$(HLColor (2)))
	.write("Directives",strl$(HLColor (3)))
	.write("Properties",strl$(HLColor (4)))
	.write("Types",strl$(HLColor (5)))
	.write("Comments",strl$(HLColor (6)))
	.write("Strings",strl$(HLColor (7)))
	.write("Numbers",strl$(HLColor (8)))
	.write("Text",strl$(HLColor (9)))
	.write("GutterBG",strl$(HLColor (10)))
	.write("GutterTxt",strl$(HLColor (11)))
	.write("LogEditBG",strl$(HLColor (12)))
	.write("Custom1",strl$(HLColor (13)))
	.write("Custom2",strl$(HLColor (14)))
	.write("Custom3",strl$(HLColor (15)))
	
	.write("srcedit.Font.Name",StrFont.Name)
	.write("srcedit.Font.Size",strl$(StrFont.Size))
	
	' загружаем новую схему
	'ClrSchEdit.Text=ClrSchComBox.Text
	ClrSchEdit.Text=ClrSchComBox.Item(ClrSchComBox.ItemIndex)
	call  AddClrString ("11655:ClrSchComBox.Item("+str$(ClrSchComBox.ItemIndex)+")="+(ClrSchComBox.Item(ClrSchComBox.ItemIndex)), clred, LogEdit)
	call  AddClrString ("11655:ClrSchEdit.Text="+(ClrSchEdit.Text), clm, LogEdit)
	
	
	if ClrSchEdit.Text="" then 
		'ShowMessage("11582: Schem name is empty. Get Classic Window schem"):Exit sub
		ClrSchEdit.Text="Classic Windows"
	end if
	
	.Section=ClrSchEdit.Text ' new schem
	call  AddClrString ("11356: загружаем новую схему RQdbini.Section="+(RQdbini.Section), clred, LogEdit)
	
	
	RQForm.color=val(.get("RQForm.color",strl$(-2147483633)))
	TabPanel.color=val(.get("TabPanel.color",strl$(-2147483633)))
	ToolPanel.color=val(.get("ToolPanel.color",strl$(-2147483633)))
	Toolbar.color=val(.get("Toolbar.color",strl$(-2147483633)))
	RunDebugBar.color=val(.get("RunDebugBar.color",strl$(-2147483633)))
	TabBAr.color=val(.get("TabBAr.color",strl$(-2147483633)))
	TplListBox.color=val(.get("TplListBox.color",strl$(-2147483643)))
	SectionListBox.color=val(.get("SectionListBox.color",strl$(-2147483643)))
	FindGroupBox.color=val(.get("FindGroupBox.color",strl$(-2147483633)))
	SearchCBox.color=val(.get("SearchCBox.color",strl$(-2147483643)))
	ReplaceComBox.color=val(.get("ReplaceComBox.color",strl$(-2147483643)))
	WhooleWordCheckBox.color=val(.get("WhooleWordCheckBox.color",strl$(-2147483643)))
	EditPanelPar.color=val(.get("EditPanelPar.color",strl$(-2147483633)))
	EditPanel.color=val(.get("EditPanel.color",strl$(-2147483633)))
	RichPanel.color=val(.get("RichPanel.color",strl$(-2147483633)))
	LogPanel.color=val(.get("LogPanel.color",strl$(-2147483633)))
	FilePanel.color=val(.get("FilePanel.color",strl$(-2147483633)))
	FilePanelHead.color=val(.get("FilePanelHead.color",strl$(-2147483633)))
	ObjTreePanel.color=val(.get("ObjTreePanel.color",strl$(-2147483633)))
	ObjTreeView.color=val(.get("ObjTreeView.color",strl$(-2147483643)))
	IncTreeView.color=val(.get("IncTreeView.color",strl$(-2147483643)))
	FileListPanel.color=val(.get("FileListPanel.color",strl$(-2147483633)))
	FileListPanelHead.color=val(.get("FileListPanelHead.color",strl$(-2147483633)))
	DirBox.color=val(.get("DirBox.color",strl$(-2147483643)))
	MaskBox.color=val(.get("MaskBox.color",strl$(-2147483643)))
	MaskBox.Font.color=val(.get("MaskBox.Font.color",strl$(-2147483640)))
	FileListBox1.color=val(.get("FileListBox1.color",strl$(-2147483643)))
	FileListBox1.Font.color=val(.get("FileListBox1.Font.color",strl$(-2147483640)))
	StatusPanel5.color=val(.get("StatusPanel5.color",strl$(16711935)))
	
	'end if
	
	' загружаем цвета редактора из выбранной цветовой схемы ClrSchComBox.Text
	'RQdbini.Section="Editor"
	
	'SyntaxHLBox.checked=val(RQdbini.get("SyntaxHighLight","1"))
	'HtHLBox.checked=val(RQdbini.get("HotTabsHighLight","1"))
	
	HLColor(0)=val(.get("BackGroung",str$(&HB5E6EC)))
	HLColor(1)=val(.get("KeyWords",str$(&HFF0000)))
	HLColor(2)=val(.get("Operators",str$(&HFF0082)))
	HLColor(3)=val(.get("Directives",str$(&HFF0000)))
	HLColor(4)=val(.get("Properties",str$(&HFF0000)))
	HLColor(5)=val(.get("Types",str$(&HFF0000)))
	HLColor(6)=val(.get("Comments",str$(&H00BE00)))
	HLColor(7)=val(.get("Strings",str$(&H0077F9)))
	HLColor(8)=val(.get("Numbers",str$(&H0000FF)))
	HLColor(9)=val(.get("Text",str$(0)))
	HLColor(10)=val(.get("GutterBG",str$(&HF9E7FF)))
	HLColor(11)=val(.get("GutterTxt",str$(clBlue)))
	HLColor(12)=val(.get("LogEditBG",str$(&HFFF2F1)))
	HLColor(13)=val(.get("Custom1",str$(&HE2ECf8)))
	HLColor(14)=val(.get("Custom2",str$(&HE2ECB8)))
	HLColor(15)=val(.get("Custom3",str$(&H0000FF)))
	
	
	srcedit.color=HLColor(0)
	BorderRich.color=srcedit.color
	MarginEdit.color=HLColor(10)
	MarginEdit.font.color=HLColor(11)
	LogEdit.color=HLColor(12)
	StrFont.color=HLColor(9)
	srcedit.Font.color=HLColor(9)
	
	srcedit.Font.Name=StrFont.Name'.get("srcedit.Font.Name","FixedSys"))
	srcedit.Font.Size=StrFont.Size 'val(.get("srcedit.Font.Size","10"))
	
	MarginEdit.font.Name=StrFont.Name
	MarginEdit.Font.Size=StrFont.Size 'val(.get("srcedit.Font.Size","10"))
	
	srcedit.visible=0
	call  AddClrString ("11739:srcedit.visible="+str$(srcedit.visible), clred, LogEdit)
	srcedit.visible=1
	call  AddClrString ("11741:srcedit.visible="+str$(srcedit.visible), clred, LogEdit)
	
	'Application. 
	'print"HLColor(9)=";HLColor(9)
	
	brem 0
	=================
	cell(0,0)=" Name"
	cell(1,0)=" Font color"
	cell(2,0)=" BG Color"
	
	cell(0,1)=" BackGroung"
	cell(0,2)=" KeyWords"
	cell(0,3)=" Operators"
	cell(0,4)=" Directives"
	cell(0,5)=" Properties"
	cell(0,6)=" Types"
	cell(0,7)=" Comments"
	cell(0,8)=" Strings"
	cell(0,9)=" Numbers"
	cell(0,10)=" Text"
	cell(0,11)=" GutterBG"
	cell(0,12)=" GutterTxt"
	cell(0,13)=" LogEditBG"
	cell(0,14)=" Custom1"
	cell(0,15)=" Custom1"
	================
	erem
	
	TypeListGrid.repaint
	
	
	
end with

RQdbini.Section="Editor"

RQdbini.write("SyntaxHighLight",str$(SyntaxHLBox.checked))
RQdbini.write("HotTabsHighLight",str$(HtHLBox.checked))
'HLColor (0 TO 15)


ClrSchComBoxText$=ClrSchEdit.Text ' сохраняем имя новой схемы

End Sub 

'!******************************************
Sub AddSchemBtnOnClick
call  AddClrString ("11510:AddSchemBtnOnClick="+str$(AddSchemBtnOnClick), clred, LogEdit)

'!!! dim ClrSchComBoxList as QStringList должен быть глобальный
ClrSchComBoxList.clear

flgg=0

if ClrSchEdit.Text<>"" then
	'ClrSchComBox.Text=ClrSchEdit.Text
else
	ShowMessage ("Input Color Schem name"): exit sub
end if

for i=0 to ClrSchComBox.ItemCount-1
	if ClrSchComBox.Item(i)=ClrSchEdit.Text then flgg=1 
	' если название схемы в тексте есть в списке то выставляем флаг
next i


if flgg=0 then ' если схемы не было в списке, то молча добавляем
	ClrSchComBox.AddItems (ClrSchEdit.Text)
	'ClrSchComBoxList.AddItems (ClrSchEdit.Text)
	flgg=1
	
else ' если схема была в списке, сначала спрашиваем
	IF MessageBox("Color Schem name "+qt+ClrSchEdit.Text+qt+" already exists. Overright this schem with new colors?", "Warning!", 1) = 1 THEN
		
	else
		exit sub
	END IF
	
end if

for i=0 to ClrSchComBox.ItemCount-1 ' переписываем новый список в лист
	ClrSchComBoxList.AddItems ClrSchComBox.Item(i)
next i
ClrSchComBoxList.Savetofile ("ClrSchComBox.lst")


for i=0 to ClrSchComBox.ItemCount-1
	if ClrSchComBox.Item(i)=ClrSchEdit.Text then
		ClrSchComBox.ItemIndex=i
		call  AddClrString ("13283:ClrSchComBox.ItemIndex="+str$(ClrSchComBox.ItemIndex), clm, LogEdit)
		exit for
	end if
	
next i



RQdbini.Section="Color Schem"
RQdbini.write("Color Schem",ClrSchEdit.Text)

RQdbini.Section=ClrSchEdit.Text
with RQdbini
	
	
	'=============
	.write("BackGroung",strl$(HLColor (0)))
	.write("KeyWords",strl$(HLColor (1)))
	.write("Operators",strl$(HLColor (2)))
	.write("Directives",strl$(HLColor (3)))
	.write("Properties",strl$(HLColor (4)))
	.write("Types",strl$(HLColor (5)))
	.write("Comments",strl$(HLColor (6)))
	.write("Strings",strl$(HLColor (7)))
	.write("Numbers",strl$(HLColor (8)))
	.write("Text",strl$(HLColor (9)))
	.write("GutterBG",strl$(HLColor (10)))
	.write("GutterTxt",strl$(HLColor (11)))
	.write("LogEditBG",strl$(HLColor (12)))
	.write("IncEditBG",strl$(HLColor (13)))
	.write("FileMngEditBG",strl$(HLColor (14)))
	'===========
end with

End Sub 
'!******************************************
Sub ClrSchEditOnKeyPress(Key AS BYTE)

IF key = 13 THEN  ' нажали Enter, сохраняем цветовую схему
	
	AddSchemBtnOnClick
	
end if

End Sub 
'!******************************************
Sub SyntaxHLBoxOnClick
SrcEdit.visible=0
SrcEdit.visible=1

End Sub 
'!******************************************
Sub CheckPrjOnClick

for ip=0 to PrjItemCount-1
	call  AddClrString ("11980 :CheckPrjOnClick ip="+str$(ip), cldp, LogEdit)
	
	if instr(PrjMnu(ip).caption,"xxx")>0 then
		PrjMnu(ip).visible=0
		PrjMnu(ip).caption=""
		
	end if
	
next ip
'PrjFreeIndex=VisiblePrj+1


End Sub 


'!******************************************
Function ConvertCodePage(SourceString As String, inPage As long, outPage As long) As String

sslen=len(SourceString)
'call  AddClrString ("14405:sslen="+str$(sslen), clred, LogEdit)
'print"sslen=";sslen
DefStr SrcStr = SourceString
DefStr mbOutStr = "", wcOutStr = ""
DefLng SrcStrLen = Len(SrcStr)
'print"SrcStrLen=";SrcStrLen
DefLng mbReqSize = 0, wcReqSize = 0
DefLng mbStrSize = 0, wcStrSize = 0
DefLng mbOutSize = 0, wcOutSize = 0

defint  ptrSRС=VarPtr(SrcStr)

mbReqSize = MultiByteToWideChar(inPage, 0, ptrSRС, -1, 0, 0)
'call  AddClrString ("14416:mbReqSize="+str$(mbReqSize), clred, LogEdit)
'print"mbReqSize=";mbReqSize
mbStrSize = (mbReqSize * 2)
'call  AddClrString ("14419:mbStrSize="+str$(mbStrSize), clred, LogEdit)
'print"mbStrSize=";mbStrSize

mbOutStr = String$(mbStrSize, Chr$(0))
defint  ptrMOS=VarPtr(mbOutStr)

mbOutSize = MultiByteToWideChar(inPage, 0, ptrSRС, SrcStrLen, ptrMOS, mbStrSize)
'call  AddClrString ("14424:mbOutSize="+str$(mbOutSize), clred, LogEdit)
'print"mbOutSize=";mbOutSize

wcReqSize = WideCharToMultiByte(outPage, 0, ptrMOS, -1, 0, 0, 0, 0)
'call  AddClrString ("14428:wcReqSize="+str$(wcReqSize), clred, LogEdit)
'print"wcReqSize=";wcReqSize

wcStrSize = (wcReqSize * 1)
wcOutStr = String$(wcStrSize, Chr$(0))
defint  ptrWOS=VarPtr(wcOutStr)

wcOutSize = WideCharToMultiByte(outPage, 0, ptrMOS, mbStrSize, ptrWOS, wcStrSize, 0, 0)
'call  AddClrString ("14434:wcOutSize="+str$(wcOutSize), clred, LogEdit)

'print"wcOutSize=";wcOutSize
'zzz$=Left$(wcOutStr, wcOutSize)
zzz11$=Left$(wcOutStr, wcReqSize-1)
lenutf=len(zzz11$)
'call  AddClrString ("14441:lenutf="+str$(lenutf), clred, LogEdit)
'print"lenutf=";lenutf
Result = zzz11$
End Function
'// * End Of Function *// ========================================================= !!!


'!*****************************************
Function ConvertCodePage12(SourceString As String, inPage As long, outPage As long) As String

Dim LenSourсeString As Long
LenSourсeString = Len(SourceString)

Dim str1 As String
Dim str2 As String
Dim RetStrLong As Long

dim tmp as string 
tmp=SourceString
ptrtmp&=varptr(tmp)

str1 = String$(LenSourсeString * 2, chr$(0))
str2 = String$(LenSourсeString * 2, chr$(0))


ptrstr1&=varptr(str1)
ptrstr2&=varptr(str2)

RetStrLong = MultiByteToWideChar(inPage, 0, tmp, LenSourсeString , ptrstr1&, LenSourсeString )
RetStrLong2 = WideCharToMultiByte(outPage, 0, ptrstr1&, RetStrLong, str2, LenSourсeString * 2, 0, 0) ' str2

ConvertCodePage = Left$(str2, RetStrLong2)


End Function

'!*****************************************
Function FromUTF8(ByVal sText As String) As String
Dim nRet As Long, strRet As String

strRet = String$(Len(sText), chr$(0))

ptrstr1&=varptr(strRet)

nRet = MultiByteToWideChar(65001, &H0, sText, Len(sText), ptrstr1&, Len(strRet))
FromUTF8 = Left$(strRet, nRet)
End Function

'!*****************************************
Function ToUTF8(ByVal sText As String) As String
Dim nRet As Long, strRet As String, ssText As String, str2  As String
ssText=sText
strRet = String$(Len(ssText) * 2, chr$(0))
ptrstr1&=varptr(ssText)
ptrstr2&=varptr(strRet)
str2 = String$(LenSourсeString * 8, chr$(0))


nRet = WideCharToMultiByte(65001, &H0, ptrstr1&, Len(ssText), str2, Len(ssText) * 2, 0&, 0&)
' ToUTF8 = Left$(StrConv(strRet, vbUnicode), nRet)
End Function

'!*****************************************
Sub UTF8Decode
dim StrUtf8List as QStringList
dim CP1251Sample as string
CP1251Sample="укенгапролдясмитб"

for i=1 to len (CP1251Sample)
	if instr (Window(WindowsIndex),CP1251Sample[i])>0 then
		ShowMessage ("12169:Code page is not UTF8")
		exit sub
	end if
	
next i


StrUtf8List.Text=Window(WindowsIndex)
'call  AddClrString ("12082:StrUtf8List.Text="+(StrUtf8List.Text), clred, LogEdit)
call  AddClrString ("12083:StrUtf8List.ItemCount="+str$(StrUtf8List.ItemCount), clred, LogEdit)

for i=0 to StrUtf8List.ItemCount-1
	StrUtf8List.Item(i)=ConvertCodePage(StrUtf8List.Item(i),cp_utf8, cp_win)
	'call  AddClrString ("12087:StrUtf8List.Item("+str$(i)+")="+(StrUtf8List.Item(i)), cldp, LogEdit)
	'call  AddClrString ("12087:cp_utf8="+str$(cp_utf8), clred, LogEdit)
next i
Window(WindowsIndex)=StrUtf8List.text  
'call  AddClrString ("12091:StrUtf8List.text="+(StrUtf8List.text), clred, LogEdit)
SrcEdit.text=Window(WindowsIndex)
'SrcEditLineCount=SrcEdit.LineCount
SrcEdit.Modified=Modif(WindowsIndex)


End Sub 

'!*****************************************
Sub VerifyArduinoSketch
logedit.clear
cls

doevents

call SaveFileOnClick

fext$=StripFileExt(SrcFileName)
'print"fext$=";fext$
'print"SrcFileName=";SrcFileName

'call  AddClrString ("12977:SrcFileName="+(SrcFileName), clred, LogEdit)
'!!! CompileMsg.txt

'call  AddClrString ("12971:fext$="+(fext$), clred, LogEdit)

if fext$<>".ino"  and fext$<>".pde" then
	ShowMessage ("14034: File "+SrcFileName+" is not Arduino sketch")
	exit sub
end if


'!!! arduino-cli compile -b arduino:sam:arduino_due_x  
'!!! C:\_F\bas\rapidq\RQIDE\arduino-cli_0.18.1\MySketch\MySketch.ino > compile.lst
' --log-file CompileMsg.txt
ArdlibPath$=ArdLibraryPathEdit.text 'sketchbookPathEdit.text+"libraries"

call  AddClrString ("14369:ArdlibPath$="+(ArdlibPath$), clred, LogEdit)

'call  AddClrString ("13415:uploadSketch="+str$(uploadSketch), clred, LogEdit)

kill StartPath$+"CompileMsg.log"
kill StartPath$+"Compile.log"
kill StartPath$+"CompileMsg.txt"

KillSubDir (StartPath$+"_compiled", "*.*")

sleep 0.2

call  AddClrString ("14384:FQBN$="+(FQBN$), clred, LogEdit)
ardfin1$="echo ardfin1 > "+StartDir+"ardfin"

if uploadSketch=0 then
	'" --libraries "+ ArdlibPath$+_
	
	clicmd$=CliPathEdit.text+" compile -b "+FQBN$+_
	" --warnings  "+ArdWarningCmb.text+" "+_
	"--log-level "+ArdLogLevelCmb.text+" "+_
	"--build-path "+BuildPathEdit.text+" "+_
	"--libraries "+ ArdlibPath$+" "+_
	" --log-file "+ArdLogFileEdit.text+" "+_
	"  "+StripPath(SrcFileName)   +" 1>"+StartDir +"CompileMsg.txt 2>&1"+_
	crlf + ardfin1$
	
	'" --log-file "+StartPath$+"CompileMsg.log "+_
	'" --log-file "+StartPath$+"CompileMsg.log "+_
	'"--build-path "+StartPath$+"_compiled"+_
	
	
elseif uploadSketch=1 then
	uploadSketch=0
	'" --libraries "+ ArdlibPath$+_
	'SrcFileName=SrcFileName-fext$
	call  AddClrString ("14342:SrcFileName="+(SrcFileName), clb, LogEdit)
	
	
	clicmd$=CliPathEdit.text+" compile -b "+FQBN$+_
	" --warnings  "+ArdWarningCmb.text+" "+_
	"--log-level "+ArdLogLevelCmb.text+" "+_
	"--build-path "+BuildPathEdit.text+" "+_
	"--libraries "+ ArdlibPath$+" "+_
	" --warnings none --log-level info  "+_
	"--upload "+_
	"--port "+ PortName$+_
	" --log-file "+StartPath$+"CompileMsg.log "+_
	"  "+StripPath(SrcFileName)   +" 1>"+StartDir +"CompileMsg.txt 2>&1"+_
	crlf + ardfin1$
	
end if

call  AddClrString ("12959:clicmd$="+(clicmd$), clred, LogEdit)
kill (StartDir +"ardfin")
zzf=fileexists(StartDir +"ardfin")
call  AddClrString ("14353:"+StartDir +"ardfin="+str$(zzf), clp, LogEdit)


savestring(clicmd$,StartDir+"clicmd.bat")

sleep 0.5
t1=timer
print "t1=", t1
'if fileexist (StartDir+"clicmd.bat")>0  then

'PID = SHELL(StartDir+"clicmd.bat > compile.log", 1)
run (StartDir+"clicmd.bat > compile.log")
ArdLogEdit.clear
sleep 0.5
wi=0
waitformsgfile:

'zzz=WaitForSingleObject (PID, 35000)
call  AddClrString ("14444:StartDir +ardfin="+(StartDir +"ardfin"), clp, LogEdit)
doevents

call  AddClrString ("14445:"+StartDir +"ardfin="+str$(zzf), cldg, LogEdit)


if fileexists(StartDir+"ardfin")=0 then 
	inc wi
	if fileexists(StartDir +"CompileMsg.txt")>0 then CompileMsg$=loadstring(StartDir+"CompileMsg.txt")
	call  AddClrString ("14464:CompileMsg$="+(CompileMsg$), clred, LogEdit)
	ArdLogEdit.Text=CompileMsg$
	'wi=len(CompileMsg$)
	'call  AddClrString (CompileMsg$, cly, ArdLogEdit)
	call  AddClrString ("==="+str$(wi), cly, ArdLogEdit)
	
	doevents
	if wi>200 then 
		call  AddClrString ("14459: Scetch not compiled="+ExeFileName$, 0, LogEdit)
		exit sub
	end if
	sleep 0.3
	goto waitformsgfile
end if

CompileMsg$=loadstring("CompileMsg.txt")
'call  AddClrString ("14476:CompileMsg$="+(CompileMsg$), cly, LogEdit)
'kill "ardfin"

kill (StartDir +"ardfin")
zzf=fileexists(StartDir +"ardfin")
call  AddClrString ("14353:"+StartDir +"ardfin="+str$(zzf), clp, LogEdit)


ArdLogEdit.Text=CompileMsg$

'sleep 2
'call  AddClrString ("12869:PID="+str$(PID), clred, LogEdit)

ArdLogEdit.Loadfromfile (StartDir +"CompileMsg.txt")
call  AddClrString ("13461:StartDir +compile.log="+(StartDir +"compile.log"), clp, LogEdit)

'kill (StartDir +"ardfin")
'if fileexists(StartDir +"ardfin")>0 then 
'kill "ardfin"
'sleep 1
'end if

call  AddClrString ("14472:ardfin="+str$(ardfin), clred, LogEdit)

'logedit.Loadfromfile (StartDir+"compile.log")

End Sub 
'!*****************************************
Sub UploadArduinoSketch
uploadSketch=1

call VerifyArduinoSketch

End Sub 
'!*****************************************
Sub ArduinoPrefOnClick
PrjPropert.show 
End Sub 
'!*****************************************
Sub ArdBasTabChange

if BasicRBtn.checked then
	RunDebugBar.Visible = True
	ArduinoBar.Visible = False
else
	RunDebugBar.Visible = False
	ArduinoBar.Visible =  True
	BoardCmbBoxOnChange
	
	
end if


End Sub 
'!*****************************************
Sub sketchbookPathBtnonclick
OpenDialog.FilterIndex = 3
OpenDialog.FileName=""
OpenDialog.InitialDir=sketchbookPathEdit.text
call  AddClrString ("12740:OpenDialog.InitialDir="+str$(OpenDialog.InitialDir), clred, LogEdit)
'call  AddClrString ("12735:OpenDialog.FilterIndex="+str$(OpenDialog.FilterIndex), clred, LogEdit)

IF OpenDialog.Execute THEN
	sketchbookPathEdit.text=StripPath(OpenDialog.FileName)
end if

End Sub 
'!*****************************************
Sub CliFileButnonclick
OpenDialog.FilterIndex = 1

'call  AddClrString ("12735:OpenDialog.FilterIndex="+str$(OpenDialog.FilterIndex), clred, LogEdit)
OpenDialog.FileName=CliPathEdit.text
OpenDialog.InitialDir=StripPath(CliPathEdit.text)
IF OpenDialog.Execute THEN
	'CliPathEdit.text=StripPath(OpenDialog.FileName)
	CliPathEdit.text=(OpenDialog.FileName)
	call  AddClrString ("12755:CliPathEdit.text="+(CliPathEdit.text), clred, LogEdit)
end if

End Sub 
'!*****************************************
Sub TabRightChange
print"14867 TabRightChange="

call  AddClrString ("14301:TabRight.TabIndex="+str$(TabRight.TabIndex), clred, LogEdit)
print"14870 TabRight.TabIndex=";TabRight.TabIndex

SELECT CASE TabRight.TabIndex
CASE 0
	SrcEditRight2.VISIBLE=0 
	SubsListOnClick
	print"14875 SubsListOnClick="
CASE 1
	SrcEditRight2.VISIBLE=0 
	ObjTreeOnClick
CASE 2
	SrcEditRight2.VISIBLE=0 
	IncFilesMnuOnClick
CASE 3
	
	SrcEditRight2.VISIBLE=1 
end select

End Sub 

'!*****************************************
Sub BoardCmbBoxOnChange1
call  AddClrString ("13801:BoardCmbBoxOnChange1=", clm, LogEdit)

End Sub 


'!*****************************************
Sub BoardCmbBoxOnChange

logedit.clear
BoardComboBox.clear
BoardComboBox.Text="No boards found"

if FileExists(CliPathEdit.text)=0 then
	ShowMessage "14199: Cli file not found "+CliPathEdit.text
	PropertTab.TabIndex=7
	PrjPropert.Show
	
end if


clicmd$=CliPathEdit.text+" board list"+" > "+StartDir +"BoardList.txt"
savestring(clicmd$,StartDir+"clicmd.bat")

if fileexist (StartDir+"clicmd.bat") then '''
	PID = SHELL(StartDir+"clicmd.bat", 1)
else
	Showmessage ("12872: File not found "+StartDir+"clicmd.bat")
	
end if

call  AddClrString ("12869:WaitForSingleObject PID="+str$(PID), clred, LogEdit)
WaitForSingleObject (PID, 5000)

if fileexist (StartDir+"BoardList.txt") then
	
	ArdBoardList.Loadfromfile (StartDir +"BoardList.txt")
	
	for i=0 to ArdBoardList.ItemCount-1
		ArdBoardList.Item(i)=ConvertCodePage(ArdBoardList.Item(i), cp_win,cp_utf8)
	next i
	
	
	'BoardList$=LoadString(StartDir +"BoardList.txt")
	'BoardList$=ConvertCodePage(BoardList$,cp_utf8, cp_win)
	'StrUtf8List.Item(i)=ConvertCodePage(StrUtf8List.Item(i),cp_utf8, cp_win)
	'logedit.Loadfromfile (StartDir +"BoardList.txt")
	'logedit.text=BoardList$+crlf+"-----------------------------------------------"
	'ArdBoardList.text=BoardList$
	
	'ArdBoardList.Loadfromfile (StartDir +"BoardList.txt")
	
else
	Showmessage ("12884: File not found "+StartDir+"BoardList.txt")
	
end if

PortPos=instr(ArdBoardList.Item(0),"Port")
call  AddClrString ("14569:PortPos="+str$(PortPos), clred, LogEdit)
BrdPos=instr(ArdBoardList.Item(0),"Board")
call  AddClrString ("14571:BrdPos="+str$(BrdPos), clred, LogEdit)
FQBNPos=instr(ArdBoardList.Item(0),"FQBN")
call  AddClrString ("14573:FQBNPos="+str$(FQBNPos), clred, LogEdit)
CorePos=instr(ArdBoardList.Item(0),"Core")
call  AddClrString ("14575:CorePos="+str$(CorePos), clred, LogEdit)

If PortPos=0 or BrdPos=0 or FQBNPos=0 or CorePos= then
	Showmessage ("13137: No boards found "+StartDir+"BoardList.txt")
	
end if


for i=1 to ArdBoardList.ItemCount-1
	
	
	LenList=len(ArdBoardList.Item(i))
	
	call  AddClrString ("13074:CorePos="+str$(CorePos), cldr, LogEdit)
	call  AddClrString ("13075:ArdBoardList.Item("+str$(i)+")="+(ArdBoardList.Item(i)), clb, LogEdit)
	call  AddClrString ("13075:LenList="+str$(LenList), clred, LogEdit)
	
	
	PortName1$=trim$(field$(ArdBoardList.Item(i)," ",1))
	call  AddClrString ("13078:PortName1$="+(PortName1$), clred, LogEdit)
	BoardName1$=trim$(mid$(ArdBoardList.Item(i),BrdPos,FQBNPos-BrdPos))
	call  AddClrString ("13080:BoardName1$="+(BoardName1$), clred, LogEdit)
	FQBN1$=trim$(mid$(ArdBoardList.Item(i),FQBNPos,CorePos-FQBNPos))
	call  AddClrString ("13082:FQBN1$="+(FQBN1$), clred, LogEdit)
	Core1$=trim$(mid$(ArdBoardList.Item(i),CorePos,LenList-CorePos+1))
	call  AddClrString ("13084:Core1$="+(Core1$), clred, LogEdit)
	
	if FQBN1$<>"" and Core1$<>"" then
		BoardComboBox.AddItems (BoardName1$+"/"+PortName1$+" "+FQBN1$+" "+Core1$)
		PortName$=PortName1$
		BoardName$=BoardName1$
		FQBN$=FQBN1$
		Core$=Core1$
		call  AddClrString ("13075:ArdBoardList.Item("+str$(i)+")="+(ArdBoardList.Item(i)), cldg, LogEdit)
		call  AddClrString ("13078:PortName$="+(PortName$), clз, LogEdit)
		call  AddClrString ("13080:BoardName$="+(BoardName$), clз, LogEdit)
		call  AddClrString ("13082:FQBN$="+(FQBN$), clз, LogEdit)
		call  AddClrString ("13084:Core$="+(Core$), clз, LogEdit)
		
	end if
	'PortComboBox.AddItems (PortName$)
next i

BoardComboBox.Text=BoardComboBox.Item(BoardComboBox.ItemCount-1)
'PortComboBox.Text=PortComboBox.Item(PortComboBox.ItemCount-1)


End Sub 
'!*****************************************
Sub CodeSectBtnOnClick
SectionListBox.AddItems ("-")
'call  AddClrString ("13001:AddItems="+str$(AddItems), clred, LogEdit)
'SectionListBox.DelItems (SectionListBox.ItemCount-1)


End Sub 
'!*****************************************
Sub ArduinoCmdCmboxOnChange

End Sub 
'!*****************************************
Sub ArduinoToolsCmboxOnChange

Select case ArduinoToolsCmbox.ItemIndex
	
case 0 ' Board Manager
	
case 1 ' Library Manager
	
case 2 ' Serial Monitor
	
	
case else
	
end select


call  AddClrString ("13264:ArduinoCmdCmboxOnChange="+str$(ArduinoCmdCmboxOnChange), clred, LogEdit)

LibMngForm.Show

End Sub 
'!*****************************************
Sub ArduinoflgCmboxOnChange

End Sub 
'!*****************************************
Sub SearchListFormClose
ListOfSearchChBox.Checked=0 
End Sub 
'!*****************************************
Sub DirTreeFormShow

for i=0 to CBoxHotDir.ItemCount-1
	
	if dirExists(CBoxHotDir.Item(i))=0 then
		CBoxHotDir.DelItems(i)
	end if
	
next i



End Sub 
'!*****************************************
Sub ArduinoCLIHelpClick
if ArdCliHelpForm.left+ArdCliHelpForm.Width>Screen.Width then
	
	ArdCliHelpForm.left=Screen.Width-ArdCliHelpForm.Width
	
end if

ArdCliHelpForm.Show

End Sub 
'!*****************************************
Sub ArdCliHelpFormShow

End Sub 

'!*****************************************
Sub ArdCliCmdHelpComboChange
dim ClihelpMsgList as QStringList
CmdList$=""

' очищаем таблицу
for i=1 to ArdCliHlpGrid.RowCount-1
	for j=0 to ArdCliHlpGrid.ColCount-1
		ArdCliHlpGrid.Cell(j,i)=""
	next i
next i

clicmd$=field$(ArdCliCmdHelpCombo.Item(ArdCliCmdHelpCombo.ItemIndex)," ",1)
call  AddClrString ("121:clicmd$="+(clicmd$), clred, LogEdit)

if crow=ArdCliHlpGrid.RowCount then
	ArdCliHlpGrid.RowCount=ArdCliHlpGrid.RowCount+1
else
end if
ArdCliHlpGrid.Cell(0, crow)= clicmd$
'crow=crow+1

'!!! arduino-cli board  --help
'clicmd$=CliPathEdit.text+" compile -b "+FQBN$+" "+SrcFileName+" >"+StartDir +"CompileMsg.txt"

'clicmd$="C:\_F\bas\rapidq\RQIDE\arduino-cli_0.18.1\arduino-cli.exe "+ clicmd$+" --help "+" >"+StartDir +"ClihelpMsg.txt"
clicmd$=CliPathEdit.text+" "+ clicmd$+" --help "+" >"+StartDir +"ClihelpMsg.txt"


call  AddClrString ("12959:clicmd$="+(clicmd$), clred, LogEdit)

savestring(clicmd$,StartDir+"clihlp.bat")


if fileexist (StartDir+"clihlp.bat") then ' cli help
	PID = SHELL(StartDir+"clihlp.bat", 1)
	call  AddClrString ("140:PID="+str$(PID), clred, LogEdit)
else
	Showmessage ("12872: File not found "+StartDir+"clihlp.bat")
end if

WaitForSingleObject (PID, 500)


if fileexist (StartDir+"ClihelpMsg.txt") then
	ArdCliHelpRichEd.Loadfromfile (StartDir +"ClihelpMsg.txt")
	ClihelpMsgList.Loadfromfile (StartDir +"ClihelpMsg.txt")
	'ArdBoardList.Loadfromfile (StartDir +"BoardList.txt")
	
else
	Showmessage ("12884: File not found "+StartDir+"ClihelpMsg.txt")
	
end if

call  AddClrString ("-------------------------", clb, LogEdit)

flgc=0

for i=0 to ClihelpMsgList.ItemCount-1
	
	if ClihelpMsgList.Item(i)="Available Commands:" then
		flgc=1
	elseif ClihelpMsgList.Item(i)="Flags:"  then
		flgc=0
	else
		if flgc=1 and ClihelpMsgList.Item(i)<> "" then
			CmdList$=CmdList$+ClihelpMsgList.Item(i)+lf
		else
			
		end if
		
	end if
next i

call  AddClrString ("206:CmdList$="+(CmdList$), clb, LogEdit)
ArdCliHlpGrid.ColumnList(1) =CmdList$

End Sub 

'!*****************************************
Sub ArdCliHelpFormClose
crow=1
End Sub 
'!*****************************************
Sub ArdCliOnListDropDown (Col%, Row%, BYREF S AS STRING, Sender AS QSTRINGGRID)

'call  AddClrString ("206:ListString="+(s), cldg, LogEdit)
'call  AddClrString ("206:Cell="+(Sender.cell(Col%, Row%)), cldg, LogEdit)

End Sub 
'!*****************************************
Sub ArdCliOnOnSetEditText (Col%, Row%, Value$, Sender AS QSTRINGGRID)
dim ClihelpMsgList as QStringList


call  AddClrString ("206:Value$="+(Value$), cldg, LogEdit)

ParentCmd$=Sender.Cell(0,Row%)
call  AddClrString ("247:ParentCmd$="+(ParentCmd$), cldr, LogEdit)

clicmd$=field$(trim$(Value$)," ",1)
call  AddClrString ("121:clicmd$="+(clicmd$), cldp, LogEdit)


clicmd$="C:\_F\bas\rapidq\RQIDE\arduino-cli_0.18.1\arduino-cli.exe "+ParentCmd$+" "+ clicmd$+" --help "+" >"+StartDir +"ClihelpMsg.txt"
call  AddClrString ("12959:clicmd$="+(clicmd$), clred, LogEdit)

savestring(clicmd$,StartDir+"clicmd.bat")


if fileexist (StartDir+"clicmd.bat") then
	PID = SHELL(StartDir+"clicmd.bat", 1)
	call  AddClrString ("140:PID="+str$(PID), clred, LogEdit)
else
	Showmessage ("12872: File not found "+StartDir+"clicmd.bat")
end if

WaitForSingleObject (PID, 500)


if fileexist (StartDir+"ClihelpMsg.txt") then
	'logedit.Loadfromfile (StartDir +"ClihelpMsg.txt")
	ClihelpMsgList.Loadfromfile (StartDir +"ClihelpMsg.txt")
	'ArdBoardList.Loadfromfile (StartDir +"BoardList.txt")
	
else
	Showmessage ("12884: File not found "+StartDir+"ClihelpMsg.txt")
	
end if

logedit.text=logedit.text+ClihelpMsgList.text


call  AddClrString ("-------------------------", clp, LogEdit)

End Sub 

'!*****************************************
FUNCTION  StringGridWndProc (hWnd AS LONG, uMsg AS LONG, wParam AS LONG, lParam AS LONG) AS LONG
'print"uMsg=";uMsg
Result =  CallWindowProc(OldWndProc3, hWnd, uMsg, wParam, lParam)

SELECT CASE uMsg
case WM_MOUSEWHEEL
	if wParam> 0 then
		LibMngListGrid.row=LibMngListGrid.row-1
	else
		LibMngListGrid.row=LibMngListGrid.row+1
	end if
	
end select



END FUNCTION



'!*****************************************
Sub LibMngFormShow
dim FakeList as QStringList
dim LibList as QStringList

cls

t1=timer

lib$="" ' данные одной библиотеки

JFakeList.LoadFromFile ("C:\Documents and Settings\Andrei\AppData\Local\Arduino15\library_index1.json" )
t2=timer
print "LoadFromFile t2-t1 ";t2-t1

braceCount=fbTally(JFakeList.text,"{")
print"braceCount=";braceCount

t3=timer
print "fbTally t3-t2=";t3-t2
'call  AddClrString ("172:braceCount="+str$(braceCount), clred, LogEdit)

CurRow=1
jsontxt$=JFakeList.text
'call  AddClrString ("211:len jsontxt$="+str$(len(jsontxt$)), clb, LogEdit)
'print" len(tat$)"; len(tat$)
iold=0


for i=1 to 50'braceCount-1
	'call  AddClrString ("217:i="+str$(i), clred, LogEdit)
	if i-iold>5 then
		'print"218: i=";i
		iold=i
	end if
	addr&=fbFIELD(jsontxt$,"{",i)
	'call  AddClrString ("228:addr&="+str$(addr&), cldp, LogEdit)
	lib$=VARPTR$(addr&)
	'call  AddClrString ("222:lib$="+(lib$), clred, LogEdit)
	
	LibList.text=trim$(lib$-"}"-qt-",")-ht
	'call  AddClrString ("176:lib$= "+str$(i)+" "+(lib$), clb, LogEdit)
	doevents
	'brem 0
	'print"LibList.ItemCount=";LibList.ItemCount
	
	if instr(LibList.text,"name") >0 and instr(LibList.text,"version") >0 then
		for j=0 to LibList.ItemCount-1
			LibList.Item(j)=trim$(LibList.Item(j))
			if instr(LibList.Item(j),"name") >0 then name$=field$(LibList.Item(j),":",2)-qt-","
			if instr(LibList.Item(j),"version") >0 then version$=field$(LibList.Item(j),":",2)-qt-","
			'if instr(LibList.Item(j),"author") >0 then author$=field$(LibList.Item(j),":",2)-qt-","
			'if instr(LibList.Item(j),"maintainer") >0 then maintainer$=field$(LibList.Item(j),":",2)-qt-","
			'if instr(LibList.Item(j),"sentence") >0 then sentence$=field$(LibList.Item(j),":",2)-qt-","
			'if instr(LibList.Item(j),"paragraph") >0 then paragraph$=field$(LibList.Item(j),":",2)-qt-","
			'if instr(LibList.Item(j),"website") >0 then website$=field$(LibList.Item(j),":",2)-qt-","
			'if instr(LibList.Item(j),"category") >0 then category$=field$(LibList.Item(j),":",2)-qt-","
			'if instr(LibList.Item(j),"architectures") >0 then architectures$=field$(LibList.Item(j),":",2)-qt-","
			'if instr(LibList.Item(j),"types") >0 then types$=field$(LibList.Item(j),":",2)-qt-","
			'if instr(LibList.Item(j),"repository") >0 then repository$=field$(LibList.Item(j),":",2)-qt-","
			'if instr(LibList.Item(j),"url") >0 then url$=field$(LibList.Item(j),":",2)-qt-","
			'if instr(LibList.Item(j),"archiveFileName") >0 then archiveFileName$=field$(LibList.Item(j),":",2)-qt-","
			'if instr(LibList.Item(j),"size") >0 then size$=field$(LibList.Item(j),":",2)-qt-","
			'if instr(LibList.Item(j),"checksum") >0 then checksum$=field$(LibList.Item(j),":",2)-qt-","
			
			
			
		next j 
		
		LibMngListGrid.cell(0,CurRow)=str$(CurRow)
		LibMngListGrid.cell(1,CurRow)=name$+" " +version$
		LibMngListGrid.cell(2,CurRow)=LibList.text
		CurRow=CurRow+1
		if CurRow=LibMngListGrid.RowCount then LibMngListGrid.RowCount=LibMngListGrid.RowCount+100
		LibMngListGrid.cell(0,0)=str$(LibMngListGrid.RowCount)
	else
		
		
	end if
	'erem
next i

call  AddClrString ("215:i="+str$(i), clred, LogEdit)

LibMngRichEd.text=LibMngListGrid.cell(2,1)

timerJ.enabled=1
'print"timerJ.enabled=";timerJ.enabled
'call  AddClrString ("277:timer1.enabled="+str$(timer1.enabled), clred, LogEdit)

End Sub 

't2=timer : print t2-t1
't3=timer:  print t3-t2


'!*****************************************
Sub LibMngListGridSelectCell (Col%, Row%, CanSelect%, Sender as QStringGrid)

LibMngRichEd.text=LibMngListGrid.cell(2,Row%)

End Sub 


'!*****************************************
Sub LoadJson
'print"LoadJson=";LoadJson
'call  AddClrString ("309:LoadJson="+str$(LoadJson), clred, LogEdit)
timerJ.enabled=0
'dim FakeList as QStringList
dim LibList as QStringList

'call  AddClrString ("362:braceCount="+str$(braceCount), clred, LogEdit)
CurRow=51
iold=50
addrr1&=0

'call  AddClrString ("323:istatic="+str$(istatic), clred, LogEdit)


for i=istatic to istatic+1500' braceCount-1
	if i>braceCount-1 then 
		'call  AddClrString ("327:i="+str$(i), clred, LogEdit)
		exit for
		timerJ.enabled=0
	end if
	if flgSTOP=1 then exit for
	lib$="+++" ' данные одной библиотеки
	
	addrr&=fbFIELD(jsontxt$,"{",i)
	lib$=VARPTR$(addrr&)
	'call  AddClrString ("339:lib$="+(lib$), clred, LogEdit)
	'call  AddClrString ("335:addrr&="+str$(addrr&), clred, LogEdit)
	
	LibList.clear
	LibList.text=trim$(lib$-"}"-qt-",")-ht
	doevents
	if i-iold>100 then
		iold=i
		t22=timer
	end if
	'brem 0
	'!!! ===========
	if instr(LibList.text,"name") >0 then
		
		for j=0 to LibList.ItemCount-1
			doevents
			LibList.Item(j)=trim$(LibList.Item(j))
			if instr(LibList.Item(j),"name") >0 then name$=field$(LibList.Item(j),":",2)-qt-","
			if instr(LibList.Item(j),"version") >0 then version$=field$(LibList.Item(j),":",2)-qt-","
			'if instr(LibList.Item(j),"author") >0 then author$=field$(LibList.Item(j),":",2)-qt-","
			'if instr(LibList.Item(j),"maintainer") >0 then maintainer$=field$(LibList.Item(j),":",2)-qt-","
			'if instr(LibList.Item(j),"sentence") >0 then sentence$=field$(LibList.Item(j),":",2)-qt-","
			'if instr(LibList.Item(j),"paragraph") >0 then paragraph$=field$(LibList.Item(j),":",2)-qt-","
			'if instr(LibList.Item(j),"website") >0 then website$=field$(LibList.Item(j),":",2)-qt-","
			'if instr(LibList.Item(j),"category") >0 then category$=field$(LibList.Item(j),":",2)-qt-","
			'if instr(LibList.Item(j),"architectures") >0 then architectures$=field$(LibList.Item(j),":",2)-qt-","
			'if instr(LibList.Item(j),"types") >0 then types$=field$(LibList.Item(j),":",2)-qt-","
			'if instr(LibList.Item(j),"repository") >0 then repository$=field$(LibList.Item(j),":",2)-qt-","
			'if instr(LibList.Item(j),"url") >0 then url$=field$(LibList.Item(j),":",2)-qt-","
			'if instr(LibList.Item(j),"archiveFileName") >0 then archiveFileName$=field$(LibList.Item(j),":",2)-qt-","
			'if instr(LibList.Item(j),"size") >0 then size$=field$(LibList.Item(j),":",2)-qt-","
			'if instr(LibList.Item(j),"checksum") >0 then checksum$=field$(LibList.Item(j),":",2)-qt-","
			
			
			
		next j 
		
		LibMngListGrid.cell(0,CurRow)=str$(CurRow)
		'call  AddClrString ("352:CurRow="+str$(CurRow), clred, LogEdit)
		LibMngListGrid.cell(1,CurRow)=name$+" " +version$
		LibMngListGrid.cell(2,CurRow)=LibList.text
		CurRow=CurRow+1
		
		if CurRow=LibMngListGrid.RowCount then 
			LibMngListGrid.RowCount=LibMngListGrid.RowCount+100
			LibMngListGrid.Repaint
		end if
		LibMngListGrid.cell(0,0)=str$(LibMngListGrid.RowCount)
	else
		
		call  AddClrString ("328:LibList.text="+(LibList.text), clred, LogEdit)
		
	end if
	'!!! ===========
	'erem
	
next i
istatic=i
if istatic<braceCount then timerJ.enabled=1
'call  AddClrString ("397:istatic="+str$(istatic), clred, LogEdit)
'timer1.enabled=1
'call  AddClrString ("360:i="+str$(i), clb, LogEdit)


End Sub 

'!*****************************************
Sub StopItOnClick
flgSTOP=1 
End Sub 
'!*****************************************
Sub RQFormonresize
if RQFormShowed=1 then
	LogTopLeftPanel.width=LogTopPanel.width/2+25 
	'print"LogTopPanel.width=";LogTopPanel.width
	'print"LogTopLeftPanel.width=";LogTopLeftPanel.width
end if

End Sub 
'!*****************************************
Sub RQFormonshow
RQFormShowed=1 
'TimerOnce.Enabled = 1 'однократное срабатывание после отрисовки главной формы 

End Sub 
'!*****************************************
Sub LogBtnLeftOnClick
ArdLogEdit.width=LogTopPanel.width-30 
End Sub 
'!*****************************************
Sub LogBtnCntrOnClick
ArdLogEdit.width=ArdLogWidth

End Sub 
'!*****************************************
Sub LogBtnRightOnClick
ArdLogEdit.width=30
End Sub 
'!*****************************************
Sub SplitterV3Moved
ArdLogWidth=ArdLogEdit.width
End Sub 
'!*****************************************
Sub DelSchemBtnOnClick

IF MessageBox("Delete Schem name "+qt+ClrSchEdit.Text+qt+" ?", "Warning!", 1) = 1 THEN
	ClrSchComBox.delItems ClrSchComBox.ItemIndex
	'call  AddClrString ("14085:ClrSchComBox.Item("+str$(ClrSchComBox.ItemIndex)+")="+(ClrSchComBox.Item(ClrSchComBox.ItemIndex)), clred, LogEdit)
	'call  AddClrString ("14085:ClrSchComBox.ItemIndex="+str$(ClrSchComBox.ItemIndex), clred, LogEdit)
	ClrSchComBoxList.clear
	for i=0 to ClrSchComBox.ItemCount-1 ' переписываем новый список в лист
		ClrSchComBoxList.AddItems ClrSchComBox.Item(i)
		'call  AddClrString ("14075:ClrSchComBox.Item("+str$(i)+")="+(ClrSchComBox.Item(i)), clred, LogEdit)
	next i
	ClrSchComBoxList.Savetofile ("ClrSchComBox.lst")
	ClrSchComBox.text=ClrSchComBox.Item(ClrSchComBox.ItemIndex)
	
else
	exit sub
END IF



End Sub 
'!*****************************************
Sub TimerOnceOver
'exit sub

call  AddClrString ("15415:TimerOnce.Enabled="+str$(TimerOnce.Enabled), clp, LogEdit)
TimerOnce.Enabled =0' 1 'True 
'print"15491: Sub TimerOnceOver TimerOnce.Enabled=";TimerOnce.Enabled

call RQFormShow
'call  AddClrString ("15419: After RQFormShow TimerOnce.Enabled="+str$(TimerOnce.Enabled), clp, LogEdit)
'print" 15481 After RQFormShow="

'MarginEdit.visible=0
'MarginEdit.visible=1

TabRightChange
print 15773
MarginEdit.Width=MarginEdit.Width-1
MarginEdit.Width=MarginEdit.Width+1
Timer1.Enabled = 1 'True !!!
print 15779

End Sub 
'!*****************************************
Sub TabPopUpMenuDel

End Sub 
'!*****************************************
Sub TabPopUpMenuAdd

End Sub 
'!*****************************************
Sub CloseTabBnOnClick

If Tab25.TabCount=1 then exit sub

if Tab25.TabIndex>-1  then 
	
	TabListCmbox.DelItems (Tab25.TabIndex)
	Tab25.DeleteTabs(Tab25.TabIndex)
	Tab25.TabIndex=Tab25.TabCount-1
	
	SetTab25length 
	
	call Tab25Change 
	
end if

End Sub 

'!*****************************************
Sub Tab25AddTab

'call  AddClrString ("14752:Tab25AddTab  SrcFileName="+(SrcFileName), clred, LogEdit)


TabIdx=-1

for i=0 to TabListCmbox.ItemCount-1
	if trim$(SrcFileName)=trim$(field$(TabListCmbox.Item(i),chr$(160),2)) then
		TabIdx=i ' индекс открытой вкладки
		exit for
	end if
next i

'call  AddClrString ("14758:TabIdx="+str$(TabIdx), clred, LogEdit)


if TabIdx=-1 then
	Tab25.AddTabs(StripFileName(SrcFileName))
	TabListCmbox.AddItems str$(WindowsIndex)+chr$(160)+(SrcFileName)
	TabListCmbox.ItemIndex=TabListCmbox.ItemCount-1
	Tab25.TabIndex=Tab25.TAbCount-1
	
	SetTab25length 
	TabRightChange
	
else
	Tab25.TabIndex=TabIdx
	TabListCmbox.Text=SrcFileName
	Tab25.Tab(TabIdx)=StripFileName(SrcFileName) '!!! ????????????\
end if

'TabRightChange
print 15841

End Sub 

'!*****************************************
Sub Tab25DelTab

fle=-1
call  AddClrString ("15811:SrcFileName="+(SrcFileName), clo, LogEdit)

FileName$ =field$(WindMnu(WindowsIndex).caption,chr$(160),2)'WindMnu(WindowsIndex).caption
call  AddClrString ("15810:DelWinFileName$="+(DelWinFileName$), cldr, LogEdit)

for i5=0 to TabListCmbox.ItemCount-1
	if trim$(DelWinFileName$)=trim$(field$(TabListCmbox.Item(i5),chr$(160),2)) then
		fle=i5 ' индекс открытой вкладки в TabListCmbox!
call  AddClrString ("14867:Tab25DelTab fle="+str$(fle), clm, LogEdit)
		exit for
	end if
next i5

call  AddClrString ("15818:fle="+str$(fle), clred, LogEdit)

if fle<>-1 then
	
	Tab25.DeleteTabs(fle) 'Delete tabs by index
	TabListCmbox.DelItems (fle)
	
	'!!!!Tab25.TabIndex=Tab25.TAbCount-1
	
	SetTab25length
else
	'Tab25.TabIndex=fle
	'TabListCmbox.Text=SrcFileName
end if

call Tab25Change

End Sub 

'!*****************************************
sub SetTab25Index
fle=-1
for i=0 to TabListCmbox.ItemCount-1
	if trim$(SrcFileName)=trim$(field$(TabListCmbox.Item(i),chr$(160),2)) then
		fle=i ' индекс открытой вкладки
		exit for
	end if
next i

Tab25.TabIndex=fle
TabListCmbox.Text=SrcFileName


End Sub 


'!*****************************************
sub OpenProject

End Sub 


'!*****************************************
Sub ArdLogFileButnonclick

OpenDialog.InitialDir=StartPath$

if fileexists(ArdLogFileEdit.text)>0 then
	OpenDialog.FileName=ArdLogFileEdit.text
end if

'call  AddClrString ("7933:StartPath="+(StartPath$), clred, LogEdit)
OpenDialog.FilterIndex = 7
IF OpenDialog.Execute THEN 
	ArdLogFileEdit.text=OpenDialog.FileName
end if


End Sub 


'!*****************************************
Function CreateDirIfNotExists (DirName$) as long

'call  AddClrString ("14932:DirName$="+(DirName$), clred, LogEdit)
CreateDirIfNotExists=1

if direxists(DirName$)=0 then
	
	IF MessageBox("14958: Directory "+DirName$ + " does not exist. Create new?", "Warning!", 1) = 1 THEN
		mkdir DirName$
		
		if direxists(DirName$)=0 then 
			Showmessage ("Can't create "+ DirName$)
			CreateDirIfNotExists=0
			exit function
		END IF
		
		CreateDirIfNotExists=1
		'call  AddClrString ("14938:CreateDirIfNotExists="+str$(1), clred, LogEdit)
	else
		CreateDirIfNotExists=0
		'call  AddClrString ("14941:CreateDirIfNotExists="+str$(0), clred, LogEdit)
	END IF
	
	
end if


End function 

'!*****************************************
Sub TabListCmboxChange

Tab25.TabIndex=TabListCmbox.ItemIndex
Tab25Change

End Sub 

'!*****************************************
sub SetTab25length
Tab25.width=0
for i=0 to Tab25.TabCount-1 '100
	lentab= len(Tab25.Tab(i))
	
	if lentab<4 then 
		lentab=4
	else
		'lentab=lentab-2
	end if
	
	Tab25.width=Tab25.width+ (Tab25.Font.Size)*(lentab-2)'+44
	
next i

'call  AddClrString ("==========  15014:Tab25.width="+str$(Tab25.width), cldp, LogEdit)


end sub
'!*****************************************
Sub OpenCurrPrjClick

End Sub 
'!*****************************************
Sub RQFormOnMouseMove(X%, Y%, Shift%)
'RQForm.caption=str$(X%)+"/"+str$(Y%)

End Sub 
'!*****************************************
Sub TabPopUpMenuReloadFile


call  AddClrString ("15155:SrcFileName="+(SrcFileName), clred, LogEdit)

LoadFile2Window

End Sub 
'!******************************************
Sub DelWinListOnClick
DelWinListBox.clear
for i=0 to WindowsItemCount-1 
	'if WindMnu(i).caption<>"" then
		if WindMnu(i).enabled=0 then
'call  AddClrString ("15933:WindMnu("+str$(i)+"enabled)="+str$(WindMnu(i).enabled), clred, LogEdit)
			DelWinListBoxClr=clr
			DelWinListBox.AddItems "††† "+WindMnu(i).caption

		else
			DelWinListBoxClr=cly
			DelWinListBox.AddItems WindMnu(i).caption
			
		end if
	'end if
next i
DelWinForm.show
End Sub 


'*****************************************
SUB ListBoxMeasureItem(Index AS INTEGER, Height AS INTEGER)
' This subrountine is needed ONLY if your list box is lbOwnerDrawVariable
'Height = 15 'Bitmap(Index).Height+2
END SUB

'*****************************************
SUB ListBoxDrawItem(Index AS INTEGER, State AS BYTE, Rect AS QRECT)
'DelWinListBox.FillRect(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, DelWinListBoxClr)

DelWinListBox.TextOut(10, Rect.Top+(Rect.Bottom-Rect.Top)/4, DelWinListBox.Item(index), 0, -1)
END SUB


'!******************************************
Sub DelWinBtnOnClick

for idw=0 to WindowsItemCount-1 
'call  AddClrString ("15969:DelWinListBox.Selected("+str$(idw)+")="+str$(DelWinListBox.Selected(idw)), clo, LogEdit)
	
	if DelWinListBox.Selected(idw) then
		WindowsIndex=idw
call  AddClrString ("15972:WindowsIndex="+str$(WindowsIndex), clb, LogEdit)
		CloseWinOnClick
		
	else
		
	end if
	
next idw
DelWinForm.close
End Sub 
'!******************************************
Sub CancelDelWinBtnOnClick
 DelWinForm.close
End Sub 


'!******************************************
Sub FindDisabledFilesOnClick
MainForm.Showmodal
 
End Sub 
