'=======================================================
' Type Objet
' Classe QRun
'=======================================================

Declare Function FindExecutable Lib "shell32.dll" Alias "FindExecutableA" (lpFile As String,lpDirectory As String, ByRef lpResult As String) As Long
Declare Function ShellExecute Lib "shell32.dll" Alias "ShellExecuteA" (hwnd As Long,lpOperation As String,lpFile As String,lpParameters As String,lpDirectory As String,nShowCmd As Long) As Long

' ShowWindow() Commands
Const SW_HIDE=0
Const SW_SHOWNORMAL=1
Const SW_NORMAL=1
Const SW_SHOWMINIMIZED=2
Const SW_SHOWMAXIMIZED=3
Const SW_MAXIMIZE=3
Const SW_SHOWNOACTIVATE=4
Const SW_SHOW=5
Const SW_MINIMIZE=6
Const SW_SHOWMINNOACTIVE=7
Const SW_SHOWNA=8
Const SW_RESTORE=9
Const SW_SHOWDEFAULT=10
Const SW_MAX=10

Type QRun EXTENDS QObject

  '=============================================================
  ' M�thode trouvant le nom et chemin de l'application associ�
  '=============================================================
  Function FindApplication(FileName as string) as string
    dim ExeName as string
    dim value as long
    
    ExeName=space$(255)
    value=FindExecutable(FileName,"",ExeName)
    if value>32 then
      result=rtrim$(ExeName)
    else
      result=""
    end if
  End Function
  
  '=============================================================
  ' M�thode �xecution d'un fichier associ� ou application
  '=============================================================
  Sub FileRun(FileName as string,cmdShow as short)
    ShellExecute(0,"open",FileName,"","",cmdShow)
  End Sub
  
  '=============================================================
  ' M�thode �xecution application avec ouverture fichier
  '=============================================================
  Sub ExeRun(ExeName as string,Param as string,cmdShow as short)
    ShellExecute(0,"open",ExeName,Param,"",cmdShow)
  End Sub
End Type
