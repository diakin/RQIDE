'=======================================================
' Type Objet
' Classe QDriveCombobox
'=======================================================

Declare Function GetDriveType Lib "kernel32" Alias "GetDriveTypeA" (nDrive As String) As Long

TYPE QDriveComboBox EXTENDS QComboBox
  '=========================================
  ' m�thode reception des lecteurs pr�sents
  '=========================================
  SUB GetDrives
   dim ASC_A as integer
   dim ASC_Z as integer
   dim i as integer
   dim name as string
   
   ASC_A=65
   ASC_Z=ASC_A+25
   for i=ASC_A to ASC_Z
     if GetDriveType(Chr$(i)&":\")<>1 then
       name=chr$(i)+":\"
       QDriveComboBox.AddItems name
     end if
   next i
   QDriveComboBox.ItemIndex=0
  END SUB
  
  SUB AddItems
  END SUB
  
  SUB DelItems
  END SUB
  
  SUB Clear
  END SUB
  '-- Default values
  CONSTRUCTOR
  END CONSTRUCTOR
END TYPE  
