'=======================================================
' Type Objet
' Classe QINI
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

Declare Function GetPrivateProfileString Lib "Kernel32" Alias "GetPrivateProfileStringA" _ 
(lpApplicationName As String,lpKeyName As String, _ 
lpDefault As String,lpReturnedString As long,nSize As Long,lpFileName As String) As Long 

Declare Function WritePrivateProfileString Lib "Kernel32" Alias "WritePrivateProfileStringA" _ 
(lpApplicationName as string,lpKeyName As String,lpString As String,lpFileName as string) As Long 

Declare Function GetWindowsDirectory Lib "kernel32.dll" Alias "GetWindowsDirectoryA" (lpBuffer As long,nSize As Long) As Long
Declare Function GetSystemDirectory Lib "kernel32" Alias "GetSystemDirectoryA" (lpBuffer As long,nSize As Long) As Long
Declare Function GetTempPath Lib "kernel32" Alias "GetTempPathA" (nBufferLength As Long,lpBuffer As Long) As Long


TYPE QINI EXTENDS QObject
  '===========================================
  ' champs et propriet�s
  '===========================================
  FileName as String PROPERTY SET SetFileName
  Exist as boolean
  Section as String
  WinDir as string PROPERTY SET SetWinDir
  SystemDir as string PROPERTY SET SetSystemDir
  TempDir as string PROPERTY SET SetTempDir

  '=========================================
  ' Propriet� WinDir en lecture seule
  '=========================================
  PROPERTY SET SetWinDir(name as string)
  END PROPERTY

  '=========================================
  ' Propriet� SystemDir en lecture seule
  '=========================================
  PROPERTY SET SetSystemDir(name as string)
  END PROPERTY

  '=========================================
  ' Propriet� TempDir en lecture seule
  '=========================================
  PROPERTY SET SetTempDir(name as string)
  END PROPERTY

  '=========================================
  ' Propriet� nom du fichier init
  '=========================================
  PROPERTY SET SetFileName(name as string)
    QINI.FileName=name
    if FileExists(QINI.FileName) then
      QINI.Exist=true
    else
      QINI.Exist=false
    end if
  END PROPERTY

  '=========================================
  ' M�thode recevoir valeur du fichier init
  '=========================================
  Function Get(Key as string,Default as string) as String
    dim LpBuffer as string
    dim size as short

    lpBuffer=Space$(260)
    if QINI.FileName<>"" and QINI.Section<>"" then
      size=GetPrivateProfileString(QINI.Section,Key,Default,varPTR(lpBuffer),260,QINI.FileName)
      if size>0 then
        QINI.Get=Left$(lpBuffer,size)
      else
        QINI.Get=Default
      end if
    end if
  End Function
 
  '=========================================
  ' M�thode �crire valeur dans fichier init
  '=========================================
  Function Write(Key as string,Value as string) as long
    if QINI.FileName<>"" and QINI.Section<>"" then
      QINI.Write=WritePrivateProfileString(QINI.Section,Key,Value,QINI.FileName)
    end if
  end Function

  '====================================================
  ' M�thode trouve les repertoires window+systeme+temp
  '====================================================
  Sub GetIniPath()
    dim lpBuffer as string
    dim size as short
 
    lpBuffer=Space$(260)
    size=GetWindowsDirectory(Varptr(lpBuffer),260)
    QINI.WinDir=Left$(lpBuffer,size)
    lpBuffer=Space$(260)
    size=GetSystemDirectory(Varptr(lpBuffer),260)
    QINI.SystemDir=Left$(lpBuffer,size)
    lpBuffer=Space$(260)
    size=GetTempPath(260,Varptr(lpBuffer))
    QINI.TempDir=Left$(lpBuffer,size)
  End Sub

  CONSTRUCTOR
    FileName = ""
    Section = ""
    Exist=false
    GetIniPath
  END CONSTRUCTOR
End TYPE

