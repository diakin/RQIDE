'=======================================================
' Type Objet
' Classe QDrive
'=======================================================
' GetDriveType return values
Const DRIVE_REMOVABLE=2
Const DRIVE_FIXED=3
Const DRIVE_REMOTE=4
Const DRIVE_CDROM=5
Const DRIVE_RAMDISK=6

Declare Function GetDriveType Lib "kernel32" Alias "GetDriveTypeA" (nDrive As String) As Long
Declare Function GetDiskFreeSpace Lib "kernel32" Alias "GetDiskFreeSpaceA" (root as string,BYREF sectors as long,BYREF bytes as long,BYREF clusters as long,BYREF total as long) as integer

TYPE QDrive EXTENDS QObject
  Name(26) as string
  Count as integer PROPERTY SET SetCount

  '=========================================
  ' propriet� nombre de lecteur(read only)
  '=========================================
  PROPERTY SET SetCount(value as integer)
  END PROPERTY

  '=========================================
  ' m�thode type de lecteur
  '=========================================
  Function GetType(index as integer) as long
    if index<=QDrive.Count then
      QDrive.GetType=GetDriveType(QDrive.Name(index))
    end if
  End Function

  '=========================================
  ' m�thode espace libre d'un lecteur
  '=========================================
  Function GetFreeSpace(index as integer) as long
    dim SectorsPerCluster as integer
    dim BytesPerSector as integer
    dim NumberOfFreeClusters as integer
    dim TotalClusters as integer
    dim value as integer

    if index<=QDrive.Count then
      value=GetDiskFreeSpace(QDrive.Name(index),SectorsPerCluster,BytesPerSector,NumberOfFreeClusters,TotalClusters)
      if value<>0 Then
        QDrive.GetFreeSpace=SectorsPerCluster*BytesPerSector*NumberOfFreeClusters
      end if
    end if
  End Function

  '=========================================
  ' m�thode taille d'un lecteur
  '=========================================
  Function GetSize(index as integer) as long
    dim SectorsPerCluster as integer
    dim BytesPerSector as integer
    dim NumberOfFreeClusters as integer
    dim TotalClusters as integer
    dim value as integer

    if index<=QDrive.Count then
      value=GetDiskFreeSpace(QDrive.Name(index),SectorsPerCluster,BytesPerSector,NumberOfFreeClusters,TotalClusters)
      if value<>0 Then
        QDrive.GetSize=SectorsPerCluster*BytesPerSector*TotalClusters
      end if
    end if
  End Function

  '=========================================
  ' m�thode reception des lecteurs pr�sents
  '=========================================
  SUB GetDrives
   dim ASC_A as integer
   dim ASC_Z as integer
   dim i as integer
   dim index as integer
   
   index=0
   ASC_A=65
   ASC_Z=ASC_A+25
   for i=ASC_A to ASC_Z
     if GetDriveType(Chr$(i)&":\")<>1 then
       index=index+1
       QDrive.Count=QDrive.Count+1
       QDrive.Name(index)=chr$(i)+":\"
     end if
   next i
  END SUB

  '-- Default values
  CONSTRUCTOR
    Count=0
    GetDrives
  END CONSTRUCTOR
END TYPE  
