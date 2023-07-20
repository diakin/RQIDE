'=======================================================
' Type Objet
' Classe QProcess version 1.0
'=======================================================
Const ProcessLow=-2
Const ProcessBelowNormal=-1
Const ProcessNormal=0
Const ProcessHight=2
Const ProcessAboveNormal=1
Const ProcessTimeCritical=15
Const ProcessIdle=-15
      
Declare Function CreateProcess Lib "kernel32" Alias "CreateThread" (_
lpThreadAttributes As long, _
dwStackSize As Long, _
lpStartAddress As Long, _
lpParameter As long, _
dwCreationFlags As Long, _
lpThreadId As Long_
) As Long

'BOOL CreateProcess(
'  LPCTSTR lpApplicationName ,                 // имя исполняемого модуля
'  LPTSTR lpCommandLine,                       // командная строка
'  LPSECURITY_ATTRIBUTES lpProcessAttributes , // SD (дескриптор безопасности)
'  LPSECURITY_ATTRIBUTES lpThreadAttributes,   // SD
'  BOOL bInheritHandles,                       // дескриптор параметра наследования
'  DWORD dwCreationFlags,                      // флажки создания
'  LPVOID lpEnvironment,                       // новый блок конфигурации
'  LPCTSTR lpCurrentDirectory,                 // имя текущего каталога
'  LPSTARTUPINFO lpStartupInfo,                // информация предустановки
'  LPPROCESS_INFORMATION lpProcessInformation  // информация о процессе
'); 


Declare Function TerminateProcess Lib "kernel32" Alias "TerminateThread" (ByVal hThread As Long, ByVal dwExitCode As Long) As Long
Declare Function GetProcessPriority Lib "kernel32" Alias "GetThreadPriority" (hThread As Long) As Long
Declare Function SetProcessPriority Lib "kernel32" Alias "SetThreadPriority" (hThread As Long,nPriority As Long) As Long
declare sub OnProcess_event

Type QProcess extends QObject
  Private:
    IdProcess as long
    hProcess as long
  Public:
    OnProcess as Event(OnProcess_event)
    Priority as long Property set SetPriority
  
    '==========================================
    ' proprietй prioritй process
    '==========================================  
    Property set SetPriority(priority as long)
      this.priority=priority
    end property

  Private:
    '==========================================
    ' mйthode appelle process
    '==========================================  
    Sub Process
      if This.OnProcess<>0 then CALLFUNC this.OnProcess
    End Sub
    
  Public:
    '==========================================
    ' mйthode exйcution process
    '==========================================  
    Sub Open
      dim PtVar as long
      dim PtSub as long
    
       if this.hProcess<>0 then TerminateProcess(this.hProcess,0)
       PtVar=varptr(this.IdProcess)
       PtSub=CODEPTR(this.Process)
       this.hProcess=CreateProcess(0,0,PtSub,0,0,PtVar)
       SetProcessPriority(this.hProcess,this.Priority) 
    End Sub

    '==========================================
    ' mйthode fin process
    '==========================================  
    Sub Close
      if this.hProcess<>0 then TerminateProcess(this.hProcess,0)
    End Sub

    Constructor
      priority=0
      IdProcess=0
      hProcess=0    
    End Constructor  
End Type


