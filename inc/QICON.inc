'=======================================================
' Type Objet
' Classe QICON Version 1.1
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

Declare Function ExtractAssociatedIcon Lib "shell32.dll" Alias "ExtractAssociatedIconA" (hInst As Long,ByRef lpIconPath As string ,byref lpiIcon As Long) As Long 
Declare Function ExtractIcon Lib "shell32.dll" Alias "ExtractIconA" (hInst As Long,lpszExeFileName As String,nIconIndex As Long) As Long
Declare Function DestroyIcon Lib "user32" Alias "DestroyIcon" (hIcon As Long) As Long
Declare Function DrawIcon Lib "user32" Alias "DrawIconEx" (hdc As Long,xLeft As Long,yTop As Long,hIcon As Long,cxWidth As Long,cyWidth As Long,istepIfAniCur As Long,hbrFlickerFreeDraw As Long,diFlags As Long) As Long

Const DI_MASK_ICO=&H1
Const DI_IMAGE_ICO=&H2
Const DI_NORMAL_ICO=DI_IMAGE_ICO Or DI_MASK_ICO

defByte IconHead16(1 to 62)={&h0,&h0,&h01,&h0,&h01,&h0,&h20,&h20,&h10,&h0,&h0,&h0,&h0,&h0,&hE8,&h02,&h0,&h0,&h16,&h0,&h0,&h0,&h28,&h0,&h0,&h0,&h20,&h0,&h0,&h0,&h40,&h0,&h0,&h0,&h01,&h0,&h04,&h0,&h0,&h0,&h0,&h0,&h80,&h02,&h0,&h0,&h0,&h0,&h0,&h0,&h0,&h0,&h0,&h0,&h0,&h0,&h0,&h0,&h0,&h0,&h0,&h0}
defByte IconHead256(1 to 62)={&h0,&h0,&h01,&h0,&h01,&h0,&h20,&h20,&h0,&h0,&h01,&h0,&h08,&h0,&hA8,&h08,&h0,&h0,&h16,&h0,&h0,&h0,&h28,&h0,&h0,&h0,&h20,&h0,&h0,&h0,&h40,&h0,&h0,&h0,&h01,&h0,&h08,&h0,&h0,&h0,&h0,&h0,&h0,&h0,&h0,&h0,&h0,&h0,&h0,&h0,&h0,&h0,&h0,&h0,&h0,&h0,&h0,&h0,&h0,&h0,&h0,&h0}
DIM IconMem AS QMemoryStream


Type QICON EXTENDS QOBJECT
  Private:
    head as string
    monochrome as string
    AsciiData as string
    bitmap as Qbitmap
    databmp as string
    dataTrans as string
    maskBit as string
  Public:
    FileName as string PROPERTY SET SetFileName
    count as integer
    handle as long
    Associated as boolean
    index as integer PROPERTY SET SetIndex
    

  '========================================
  ' propriet� nom fichier icone
  '========================================
  PROPERTY SET SetFileName(name as string)
    dim lpIcon as long
    dim Path as string

    if name<>"" then
      if Qicon.handle<>0 then
        DestroyIcon(Qicon.handle)
      end if
      if Qicon.Associated then
        lpIcon=2
        Qicon.FileName=name
        Path=name
        Qicon.handle=ExtractAssociatedIcon(application.handle,Path,lpIcon)
        if Qicon.handle>0 then Qicon.count=1
      else
        Qicon.count=ExtractIcon(application.handle,name,-1)
        Qicon.FileName=name
        if Qicon.count<>0 then
          Qicon.handle=ExtractIcon(application.handle,name,0)
        end if
      end if
    else
      if Qicon.handle<>0 then
        DestroyIcon(Qicon.handle)
      end if
    end if
  END PROPERTY
  
  '========================================
  ' propriet� index icone
  '========================================
  PROPERTY SET SetIndex(value as integer)
    if Qicon.handle<>0 then
      DestroyIcon(Qicon.handle)
    end if
    if value<=Qicon.count then
      Qicon.handle=ExtractIcon(application.handle,Qicon.FileName,value)
    end if
  END PROPERTY

Private:
  '==========================================
  ' m�thode transforme binaire en decimal
  '==========================================
  function BinToDec(bin as string)as long
    dim bit as integer
    dim i as integer
    dim value as integer
  
    bin=REVERSE$(bin)
    bit=1
    value=0
    for i=1 to len(bin)
      if mid$(bin,i,1)="1" then value=value+bit
      bit=bit*2
    next i
    result=value
  end function

  '=============================================
  ' m�thode transforme hexadecimal en decimal
  '=============================================
  function HexToDec(hex as string)as long
    dim bit as long
    dim valbit as integer
    dim i as integer
    dim value as integer
  
    hex=REVERSE$(hex)
    bit=1
    value=0
    for i=1 to len(hex)
      if mid$(hex,i,1)="A" then
        value=value+(10*bit)
      elseif mid$(hex,i,1)="B" then
        value=value+(11*bit)
      elseif mid$(hex,i,1)="C" then
        value=value+(12*bit)
      elseif mid$(hex,i,1)="D" then
        value=value+(13*bit)
      elseif mid$(hex,i,1)="E" then
        value=value+(14*bit)
      elseif mid$(hex,i,1)="F" then
        value=value+(15*bit)
      else
        value=value+(val(mid$(hex,i,1))*bit)
      end if
      if (bit*16)<2147483647 then bit=bit*16
    next i
    result=value
  end function

  '=============================================
  ' m�thode cr�ation format icon 16 couleur
  '=============================================
  Sub CreateFormat16(bitmap as QBitmap,convert as integer)
    dim i as integer

    if convert<>true then
      ' mise au format 16 couleur du bitmap
      bitmap.width=32
      bitmap.height=32
      bitmap.pixelformat=2
      bitmap.fillRect(0,0,32,32,&hffffff)
      ' transfert icon dans bitmap
      DrawIcon(bitmap.handle,0,0,Qicon.handle,32,32,0,0,DI_NORMAL_ICO)
    else
      bitmap.pixelformat=2
    end if
    ' sauvegarde bitmap en memoire
    IconMem.position=0
    IconMem.size=0
    bitmap.savetostream(IconMem)
    IconMem.position=0
    'lecture donn�es bitmap
    Qicon.databmp=IconMem.readStr(630)
    IconMem.close
    Qicon.head=""
    'creation en tete icone
    for i=1 to 62
      Qicon.head=Qicon.head+chr$(IconHead16(i))
    next i
    Qicon.head=Qicon.head+mid$(Qicon.databmp,55,64)
    ' extraction pixel
    Qicon.databmp=right$(Qicon.databmp,512)
  End Sub

  '=============================================
  ' m�thode cr�ation format icon 256 couleur
  '=============================================
  Sub CreateFormat256(bitmap as QBitmap,convert as integer)
    dim i as integer
    dim j as integer

    if convert<>true then
      ' mise au format 256 couleur du bitmap
      bitmap.width=32
      bitmap.height=32
      bitmap.pixelformat=3
      bitmap.fillRect(0,0,32,32,&hffffff)
      ' transfert icon dans bitmap
      DrawIcon(bitmap.handle,0,0,Qicon.handle,32,32,0,0,DI_NORMAL_ICO)
    else
      bitmap.pixelformat=3
    end if
    ' sauvegarde bitmap en memoire
    IconMem.position=0
    IconMem.size=0
    bitmap.savetostream(IconMem)
    IconMem.position=0
    'lecture donn�es asci du bitmap
    Qicon.databmp=IconMem.readStr(2102)
    IconMem.close
    Qicon.head=""
    'creation en tete icone 256 couleur
    for i=1 to 62
      Qicon.head=Qicon.head+chr$(IconHead256(i))
    next i
    'ajout de la palette de couleur
    Qicon.head=Qicon.head+mid$(Qicon.databmp,55,1024)
    ' extraction pixel du bitmap
    Qicon.databmp=right$(Qicon.databmp,1024)
  End Sub

  '=================================================
  ' m�thode cr�ation mask transparence 256 couleur
  '=================================================
  Sub CreateMask256
    dim i as integer

    ' transformation data du bitmap pour la transparence
    Qicon.datatrans=""
    for i=1 to len(Qicon.databmp)
      Qicon.AsciiData=hex$(asc(mid$(Qicon.databmp,i,1)))
      if instr(Qicon.AsciiData,"13")>0 then
        Qicon.AsciiData=replacesubstr$(Qicon.AsciiData,"13","0")
        Qicon.datatrans=Qicon.datatrans+chr$(Qicon.hexToDec(Qicon.AsciiData))
      else
        Qicon.datatrans=Qicon.datatrans+mid$(Qicon.databmp,i,1)
      end if   
    next i
    ' creation image monochrome pour le mask
    Qicon.maskBit=""
    Qicon.monochrome=""
    for i=1 to len(Qicon.databmp)
      Qicon.AsciiData=hex$(asc(mid$(Qicon.databmp,i,1)))
      if instr(Qicon.AsciiData,"13")>0 then   
        Qicon.maskBit=Qicon.maskBit+"1"
      else
        Qicon.maskBit=Qicon.maskBit+"0"
      end if
    next i 
    ' transformation du mask en 8 bit
    for i=1 to len(Qicon.maskBit) step 8
      Qicon.monochrome=Qicon.monochrome+chr$(Qicon.BinToDec(mid$(Qicon.maskBit,i,8)))
    next i
  End Sub

  '=================================================
  ' m�thode cr�ation mask transparence 16 couleur
  '=================================================
  Sub CreateMask16
    dim i as integer

   Qicon.datatrans=""
   for i=1 to len(Qicon.databmp)
     Qicon.AsciiData=hex$(asc(mid$(Qicon.databmp,i,1)))
     if instr(Qicon.AsciiData,"F")>0 then
       Qicon.AsciiData=replacesubstr$(Qicon.AsciiData,"F","0")
       Qicon.datatrans=Qicon.datatrans+chr$(Qicon.hexToDec(Qicon.AsciiData))
     else
       Qicon.datatrans=Qicon.datatrans+mid$(Qicon.databmp,i,1)
     end if   
   next i
   ' creation mask en bit monochrome
   Qicon.maskBit=""
   Qicon.monochrome=""
   for i=1 to len(Qicon.databmp)
     Qicon.AsciiData=hex$(asc(mid$(Qicon.databmp,i,1)))
     if mid$(Qicon.AsciiData,7,1)="F" then   
       Qicon.maskBit=Qicon.maskBit+"1"
     else
       Qicon.maskBit=Qicon.maskBit+"0"
     end if
     if mid$(Qicon.AsciiData,8,1)="F" then   
       Qicon.maskBit=Qicon.maskBit+"1"
     else
       Qicon.maskBit=Qicon.maskBit+"0"
     end if
   next i 
   for i=1 to len(Qicon.maskBit) step 8
     Qicon.monochrome=Qicon.monochrome+chr$(Qicon.BinToDec(mid$(Qicon.maskBit,i,8)))
   next i
  End Sub

  '=================================================
  ' m�thode cr�ation mask opaque
  '=================================================
  Sub CreateMaskOpaque
    dim i as integer

    Qicon.monochrome=""
    for i=1 to 128
      Qicon.monochrome=Qicon.monochrome+chr$(0)
    next i
  End Sub

Public:

  '=================================================
  ' m�thode sauvegarde icone
  '=================================================
  Sub SaveToFile(FileName as string,pixelFormat as integer,mask as integer)
    dim file as qfilestream
 
    if Qicon.handle<>0 then
      if pixelFormat=2 then
        Qicon.CreateFormat16(Qicon.bitmap,false)
        if mask then
          Qicon.CreateMask16
        else
          Qicon.CreateMaskOpaque
        end if
      else
        Qicon.CreateFormat256(Qicon.bitmap,false)
        if mask then
          Qicon.CreateMask256
        else
          Qicon.CreateMaskOpaque
        end if
      end if
      file.open(FileName,65535)
      file.WriteStr(Qicon.head,len(Qicon.head))
      if mask then 
        file.WriteStr(Qicon.datatrans,len(Qicon.datatrans)) 
      else
        file.WriteStr(Qicon.databmp,len(Qicon.databmp)) 
      end if
      file.WriteStr(Qicon.monochrome,len(Qicon.monochrome)) 
      file.close 
    end if
  End Sub

  '=================================================
  ' m�thode sauvegarde bitmap au format icone
  '=================================================
  Sub SaveBmpToFile(bitmap as QBitmap,FileName as string,pixelFormat as integer,mask as integer)
    dim file as qfilestream
 
    if bitmap.width=32 and bitmap.height=32 then
      if pixelFormat=2 then
        Qicon.CreateFormat16(bitmap,true)
        if mask then
          Qicon.CreateMask16
        else
          Qicon.CreateMaskOpaque
        end if
      else
        Qicon.CreateFormat256(bitmap,true)
        if mask then
          Qicon.CreateMask256
        else
          Qicon.CreateMaskOpaque
        end if
      end if
      file.open(FileName,65535)
      file.WriteStr(Qicon.head,len(Qicon.head))
      if mask then 
        file.WriteStr(Qicon.datatrans,len(Qicon.datatrans)) 
      else
        file.WriteStr(Qicon.databmp,len(Qicon.databmp)) 
      end if
      file.WriteStr(Qicon.monochrome,len(Qicon.monochrome)) 
      file.close 
    end if
  End Sub
End Type
