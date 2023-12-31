'=======================================================
' Type Objet
' Classe QPageSetup version 1.2
'=======================================================
$IFNDEF TRUE
  $DEFINE True 1
$ENDIF

$IFNDEF FALSE
  $DEFINE False 0
$ENDIF

$IFNDEF boolean
  $DEFINE boolean integer
$ENDIF

CONST PSD_DEFAULTMINMARGINS=0
CONST PSD_MINMARGINS=&H1
CONST PSD_MARGINS=&H2
CONST PSD_INTHOUSANDTHSOFINCHES=&H4
CONST PSD_INHUNDREDTHSOFMILLIMETERS=&H8
CONST PSD_DISABLEMARGINS=&H10
CONST PSD_DISABLEPRINTER=&H20
CONST PSD_NOWARNING=&H80
CONST PSD_DISABLEORIENTATION=&H100
CONST PSD_DISABLEPAPER=&H200
CONST PSD_RETURNDEFAULT=&H400
CONST PSD_SHOWHELP=&H800
CONST PSD_ENABLEPAGESETUPHOOK=&H2000
CONST PSD_ENABLEPAGESETUPTEMPLATE=&H8000
CONST PSD_ENABLEPAGESETUPTEMPLATEHANDLE=&H20000
CONST PSD_ENABLEPAGEPAINTHOOK=&H40000
CONST PSD_DISABLEPAGEPAINTING=&H80000
CONST PSD_NONETWORKBUTTON=&H200000

TYPE TPSD
  lStructSize AS LONG
  hWndOwner AS LONG
  hDevMode AS LONG
  hDevNames AS LONG
  Flags AS LONG
  ptPaperSizeX AS LONG
  ptPaperSizeY AS LONG
  rtMinMarginLeft AS LONG
  rtMinMarginTop AS LONG
  rtMinMarginRight AS LONG
  rtMinMarginBottom AS LONG
  rtMarginLeft AS LONG
  rtMarginTop AS LONG
  rtMarginRight AS LONG
  rtMarginBottom AS LONG
  hInstance AS LONG
  lParam AS LONG
  lpfnPageSetupHook AS LONG
  lpfnPagePaintHook AS LONG
  lpPageSetupTemplate AS LONG
  hPageSetupTemplate AS LONG
END TYPE

CONST WM_INITDIALOG_PSD=&H110


DECLARE FUNCTION PageSetupDlg LIB "COMDLG32" ALIAS "PageSetupDlgA" (Ptr AS TPSD) AS LONG

DECLARE FUNCTION GetPageText LIB "user32" ALIAS "GetWindowRect" (hwnd AS LONG, lpRect AS QRECT) AS LONG
DECLARE FUNCTION MovePage LIB "user32" ALIAS "MoveWindow" (hwnd AS LONG,x AS LONG,y AS LONG,nWidth AS LONG,nHeight AS LONG,bRepaint AS LONG) AS LONG
DECLARE FUNCTION SetPageText LIB "user32" ALIAS "SetWindowTextA" (hWnd AS LONG,ByRef lpString AS STRING) AS LONG


Type QPageSetup EXTENDS QOBJECT
  Private:
    PSD AS TPSD
  Public:
    Caption as string
    DisablePrinter as boolean
    DisablePaper as boolean
    DisableOrient as boolean
    DisableMargins as boolean
    Orientation as boolean PROPERTY SET SetOrientation
    MarginLeft as long PROPERTY SET SetMarginLeft
    MarginTop as long PROPERTY SET SetMarginTop
    MarginRight as long PROPERTY SET SetMarginRight
    MarginBottom as long PROPERTY SET SetMarginBottom
    PageWidth as long PROPERTY SET SetPageWidth
    PageHeight as long PROPERTY SET SetPageHeight


  Private:

  '=============================================
  ' Procédure boite de dialogue
  '============================================= 
  Function HookProc(hWnd AS LONG, uMsg AS LONG, wParam AS LONG, lParam AS LONG) AS LONG
    dim R as QRECT

    if uMsg=WM_INITDIALOG_PSD then 
      GetPageText(hWnd,R)
      MovePage(hWnd,((Screen.Width-(R.Right-R.Left))/2),((Screen.Height-(R.Bottom-R.Top))/2),(R.Right-R.Left),(R.Bottom-R.Top),0) 
      if LEN(QPageSetup.Caption) then
        SetPageText(hWnd,QPageSetup.Caption)
      end if
      Result=true
    else
      Result=false
    end if
  End Function

  Public:
  
  '=============================================
  ' Proprieté orientation page en lecture seule 
  '============================================= 
  PROPERTY SET SetOrientation(value as boolean)
  END PROPERTY
    
  '===============================================
  ' Proprieté valeur marge gauche 
  '=============================================== 
  PROPERTY SET SetMarginLeft(value as long)
    QPageSetup.PSD.rtMarginLeft=value*100
  END PROPERTY
    
  '===============================================
  ' Proprieté valeur marge haute 
  '=============================================== 
  PROPERTY SET SetMarginTop(value as long)
    QPageSetup.PSD.rtMarginTop=value*100
  END PROPERTY

  '===============================================
  ' Proprieté valeur marge droite 
  '=============================================== 
  PROPERTY SET SetMarginRight(value as long)
    QPageSetup.PSD.rtMarginRight=value*100
  END PROPERTY
   
  '===============================================
  ' Proprieté valeur marge basse 
  '=============================================== 
  PROPERTY SET SetMarginBottom(value as long)
    QPageSetup.PSD.rtMarginBottom=value*100
  END PROPERTY

  '===============================================
  ' Proprieté largeur page en lecture seule 
  '=============================================== 
  PROPERTY SET SetPageWidth(value as boolean)
  END PROPERTY
            
  '===============================================
  ' Proprieté hauteur page en lecture seule 
  '=============================================== 
  PROPERTY SET SetPageHeight(value as boolean)
  END PROPERTY
    
  '===========================================
  ' Méthode affichage mise en page imprimante
  '===========================================
  Function Execute as boolean
    QPageSetup.PSD.lStructSize = SIZEOF(QPageSetup.PSD)
    QPageSetup.PSD.hWndOwner=Application.handle
    QPageSetup.PSD.Flags=PSD_MARGINS+PSD_MINMARGINS
    if QPageSetup.Caption<>"" then
      QPageSetup.PSD.Flags=QPageSetup.PSD.Flags+PSD_ENABLEPAGESETUPHOOK
      QPageSetup.PSD.lpfnPageSetupHook=CODEPTR(QPageSetup.HookProc)
    end if
    if QPageSetup.DisablePrinter then
      QPageSetup.PSD.Flags=QPageSetup.PSD.Flags+PSD_DISABLEPRINTER
    end if
    if QPageSetup.DisablePaper then
      QPageSetup.PSD.Flags=QPageSetup.PSD.Flags+PSD_DISABLEPAPER
    end if
    if QPageSetup.DisableOrient then
      QPageSetup.PSD.Flags=QPageSetup.PSD.Flags+PSD_DISABLEORIENTATION
    end if
    if QPageSetup.DisableMargins then
      QPageSetup.PSD.Flags=QPageSetup.PSD.Flags+PSD_DISABLEMARGINS
    end if
    if PageSetupDlg(QPageSetup.PSD) then
      QPageSetup.Execute=true
      QPageSetup.PageWidth=Round(QPageSetup.PSD.ptPaperSizeX/100)
      QPageSetup.PageHeight=Round(QPageSetup.PSD.ptPaperSizeY/100)
      if QPageSetup.PageWidth>QPageSetup.PageHeight then
        QPageSetup.Orientation=true
      else
        QPageSetup.Orientation=false
      end if
      QPageSetup.MarginLeft=QPageSetup.PSD.rtMarginLeft/100
      QPageSetup.MarginTop=QPageSetup.PSD.rtMarginTop/100
      QPageSetup.MarginRight=QPageSetup.PSD.rtMarginRight/100
      QPageSetup.MarginBottom=QPageSetup.PSD.rtMarginBottom/100
    else
      QPageSetup.Execute=false
    end if
  End Function
    
  Constructor
    Caption=""
    DisablePrinter=false
    DisablePaper=false
    DisableOrient=false
    DisableMargins=false
  End Constructor
End Type
