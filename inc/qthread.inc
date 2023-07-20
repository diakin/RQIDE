'=======================================================
' Type Objet
' Classe QThread version 1.0
'=======================================================
Const ThreadLow=-2
Const ThreadBelowNormal=-1
Const ThreadNormal=0
Const ThreadHight=2
Const ThreadAboveNormal=1
Const ThreadTimeCritical=15
Const ThreadIdle=-15


'for threads in your program (like SUB and FUNCTIONS) see the thread.bas example on GUIDANCE site
Const MAXLONG = &H7FFFFFFF
CONST THREAD_BASE_PRIORITY_MIN = -2
CONST THREAD_BASE_PRIORITY_MAX = 2
CONST THREAD_BASE_PRIORITY_LOWRT = 15
CONST THREAD_BASE_PRIORITY_IDLE = -15
CONST THREAD_PRIORITY_LOWEST         = THREAD_BASE_PRIORITY_MIN
CONST THREAD_PRIORITY_BELOW_NORMAL   = (THREAD_PRIORITY_LOWEST + 1)
CONST THREAD_PRIORITY_NORMAL         = 0
CONST THREAD_PRIORITY_HIGHEST        = THREAD_BASE_PRIORITY_MAX
CONST THREAD_PRIORITY_ABOVE_NORMAL   = (THREAD_PRIORITY_HIGHEST - 1)
CONST THREAD_PRIORITY_TIME_CRITICAL  = THREAD_BASE_PRIORITY_LOWRT
CONST THREAD_PRIORITY_IDLE           = THREAD_BASE_PRIORITY_IDLE
CONST THREAD_PRIORITY_ERROR_RETURN   = (MAXLONG)


'hThread = CreateThread(
'            NULL,         // атрибуты безопасности по умолчанию
'            0,            // размер стека используется по умолчанию
'            ThreadFunc,   // функция потока
'            &dwThrdParam, // аргумент функции потока - 
'            0,            // флажки создания используются по умолчанию
'            &dwThreadId); // возвращает идентификатор потока Points to a 32-bit variable that receives the thread identifier. !!!
' функция возвращает handle потока 

Declare Function CreateThreadVB Lib "kernel32" Alias "CreateThread" (_
lpThreadAttributes As SECURITY_ATTRIBUTES, _
dwStackSize As Long, _
lpStartAddress As Long, _
lpParameter As Any, _
dwCreationFlags As Long, _
lpThreadId As Long) As Long
      
Declare Function CreateThread Lib "kernel32" Alias "CreateThread" (_
lpThreadAttributes As long, _
dwStackSize As Long, _
lpStartAddress As Long, _
lpParameter As long, _
dwCreationFlags As Long, _
byref lpThreadId As Long_  'возвращает идентификатор потока
) As Long 

Declare Function TerminateThread Lib "kernel32" Alias "TerminateThread" (hThread As Long, dwExitCode As Long) As Long
Declare Function GetThreadPriority Lib "kernel32" Alias "GetThreadPriority" (hThread As Long) As Long
Declare Function SetThreadPriority Lib "kernel32" Alias "SetThreadPriority" (hThread As Long,nPriority As Long) As Long



Declare Sub InitializeCriticalSection Lib "kernel32" Alias "InitializeCriticalSection" (lpCriticalSection As CRITICAL_SECTION)
Declare Sub EnterCriticalSection Lib "kernel32" Alias "EnterCriticalSection" (lpCriticalSection As CRITICAL_SECTION)
Declare Sub LeaveCriticalSection Lib "kernel32" Alias "LeaveCriticalSection" (lpCriticalSection As CRITICAL_SECTION)
Declare Sub DeleteCriticalSection Lib "kernel32" Alias "DeleteCriticalSection" (lpCriticalSection As CRITICAL_SECTION)


TYPE CRITICAL_SECTION
DebugInfo AS LONG  'pointer to RTL_CRITICAL_SECTION_DEBUG

'TYPE RTL_CRITICAL_SECTION_DEBUG
'wType AS WORD
'CreatorBackTraceIndex AS WORD
'CriticalSection AS DWORD ' actually, CRITICAL_SECTION PTR, but we can't do a circular reference
'ProcessLocksList AS LIST_ENTRY
'EntryCount AS DWORD
'ContentionCount AS DWORD
'Spare(0 TO 1) AS DWORD
'END TYPE

' The following three fields control entering and exiting the critical
' section for the resource

LockCount AS LONG
RecursionCount AS LONG
OwningThread AS DWORD ' from the thread's ClientId->UniqueThread
LockSemaphore AS DWORD
SpinCount AS DWORD ' force size on 64-bit systems when packed

END TYPE


Declare Function SetEvent Lib "kernel32" Alias "SetEvent" (ByVal hEvent As Long) As Long
Declare Function ResetEvent Lib "kernel32" Alias "ResetEvent" (ByVal hEvent As Long) As Long
Declare Function PulseEvent Lib "kernel32" Alias "PulseEvent" (ByVal hEvent As Long) As Long
Declare Function ReleaseSemaphore Lib "kernel32" Alias "ReleaseSemaphore" (ByVal hSemaphore As Long, ByVal lReleaseCount As Long, lpPreviousCount As Long) As Long
Declare Function ReleaseMutex Lib "kernel32" Alias "ReleaseMutex" (ByVal hMutex As Long) As Long
Declare Function WaitForSingleObject Lib "kernel32" Alias "WaitForSingleObject" (ByVal hHandle As Long, ByVal dwMilliseconds As Long) As Long
Declare Function WaitForMultipleObjects Lib "kernel32" Alias "WaitForMultipleObjects" (ByVal nCount As Long, lpHandles As Long, ByVal bWaitAll As Long, ByVal dwMilliseconds As Long) As Long
Declare Sub Sleep Lib "kernel32" Alias "Sleep" (ByVal dwMilliseconds As Long)




declare sub OnThread_event

Type QThread extends QObject
  Private:
    IdThread as long
    hThread as long
  Public:
    OnThread as Event(OnThread_event)
    Priority as long Property set SetPriority
  
    '==========================================
    ' proprietй prioritй Thread
    '==========================================  
    Property set SetPriority(priority as long)
      this.priority=priority
    end property

  Private:
    '==========================================
    ' mйthode appelle Thread
    '==========================================  
    Sub Thread
      if This.OnThread<>0 then CALLFUNC this.OnThread
    End Sub
    
  Public:
    '==========================================
    ' mйthode exйcution Thread
    '==========================================  
    Sub Open
      dim PtVar as long
      dim PtSub as long
    
       if this.hThread<>0 then TerminateThread(this.hThread,0)
       PtVar=varptr(this.IdThread)
       PtSub=CODEPTR(this.Thread)
       this.hThread=CreateThread(0,0,PtSub,0,0,PtVar)
       SetThreadPriority(this.hThread,this.Priority) 
    End Sub

    '==========================================
    ' mйthode fin Thread
    '==========================================  
    Sub Close
      if this.hThread<>0 then TerminateThread(this.hThread,0)
    End Sub

    Constructor
      priority=0
      IdThread=0
      hThread=0    
    End Constructor  
End Type


