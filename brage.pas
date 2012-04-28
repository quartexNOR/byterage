  unit brage;

(* ############################################################################
   # Author:  Jon Lennart Aasenden                                            #
   # Company: Jolead EM                                                       #
   # License: Copyright Jon Lennart Aasenden, all rights reserved under the   #
   #          international software apartment ownership act                  #
   ############################################################################

 /$$$$$$$              /$$               /$$$$$$$
| $$__  $$            | $$              | $$__  $$
| $$  \ $$ /$$   /$$ /$$$$$$    /$$$$$$ | $$  \ $$  /$$$$$$   /$$$$$$   /$$$$$$
| $$$$$$$ | $$  | $$|_  $$_/   /$$__  $$| $$$$$$$/ |____  $$ /$$__  $$ /$$__  $$
| $$__  $$| $$  | $$  | $$    | $$$$$$$$| $$__  $$  /$$$$$$$| $$  \ $$| $$$$$$$$
| $$  \ $$| $$  | $$  | $$ /$$| $$_____/| $$  \ $$ /$$__  $$| $$  | $$| $$_____/
| $$$$$$$/|  $$$$$$$  |  $$$$/|  $$$$$$$| $$  | $$|  $$$$$$$|  $$$$$$$|  $$$$$$$
|_______/  \____  $$   \___/   \_______/|__/  |__/ \_______/ \____  $$ \_______/
           /$$  | $$                                         /$$  \ $$
          |  $$$$$$/                                        |  $$$$$$/
           \______/                                          \______/

  BUFFER CONCEPT
  ==============

  As a concept a buffer could resemble a stream, in that it represents an
  abstract storage-mechanism. However a buffer is far more agile than
  normal delphi streams, supporting functions like Insert(), Remove()
  push() and poll(). Functions that are vital to compilers for instance.

  FLEXIBLE OPERATIONS
  ===================

  As an example: Inserting data is not a simple overwrite like you
  might expect with a stream. When you insert data into a buffer at any
  position, the following happens:
    1.  the buffersize grows to recieve the new data
    2.  the trailing data is moved forward
    3.  the new data is written to your target position.

  DIFFERENT STORAGE MEDIUMS
  =========================

  The basic buffer-class <TBufferCustom> is an abstract class that knows
  nothing about the actual storage medium. It will call a set of virtual
  methods to do the actual reading, writing or manipulation of data.
  This means that you can easily create decendants for any type of storage
  you like:

            x.  Memory
            x.  Files
            x.  Streams
            x.  Blobs
            x.  Online files

  It would also be equally simple to derive a buffer class for Bitmap
  memory (linear), thus isolating read & write operations to the buffer.
  ###########################################################################

  Version 1.0.0

  - [12.10.11] - Added support for LoadFromUrl

  ########################################################################### *)

  {$DEFINE BR_DEBUG}
  {$DEFINE BR_SUPPORT_INTERNET}

  interface

  uses sysutils, classes, math;

  type

  (* Custom exceptions *)
  EBRBufferError    = Class(Exception);
  EBRStreamAdapter  = Class(Exception);

  (* Forward declarations *)
  TBRBuffer         = Class;
  TBRBufferFile     = Class;
  TBRBufferMemory   = Class;
  TBRStreamAdapter  = Class;

  (* custom 3byte record for our FillTriple class procedure *)
  PBRTripleByte = ^TBRTripleByte;
  TBRTripleByte = Packed record
    a,b,c:Byte;
  End;

  (* This class allows you to access a buffer regardless of how its
     implemented, through a normal stream. This makes it very easy to
     use buffers with standard VCL components and classes *)
  TBRStreamAdapter = Class(TStream)
  Private
    FBufObj:    TBRBuffer;
    FOffset:    Int64;
  Protected
    function    GetSize:Int64;override;
    procedure   SetSize(const NewSize:Int64);override;
  Public
    Property    BufferObj:TBRBuffer read FBufObj;
  Public
    function    Read(var Buffer;Count:Longint):Longint;override;
    function    Write(const Buffer;Count:Longint): Longint;override;
    function    Seek(const Offset:Int64;Origin:TSeekOrigin):Int64;override;
    Constructor Create(Const aBuffer:TBRBuffer);reintroduce;
  End;

  (* Buffer capability enum. Different buffer types can have different
     rules. Some buffers can be read only, depending on the application,
     while others may be static - meaning that it cannot grow in size *)
  TBRBufferCapabilities  = set of (mcScale,mcOwned,mcRead,mcWrite);

  (* Data cache byte array type. Read and write operations process data in
     fixed chunks of this type *)
  TBRIOCache = packed Array [1..2048] of Byte;

  (* This is the basic buffer class. It is an abstract class and should not be
     created. Create objects from classes that decend from this and that
     implements the buffering you need to work with (memory, stream, temp etc *)
  TBRBuffer = Class(TPersistent)
  private
    (* Buffer capabilities. I.E: Readable, writeable etc. *)
    FCaps:      TBRBufferCapabilities;
    Procedure   SetSize(Const aNewSize:Int64);
  Protected
    (*  Standard persistence. Please note that we call the function
        ObjectHasData() to determine if there is anything to save.
        See extended persistence below for more information.

        NOTE: Do not override these methods, use the functions defined
              in extended persistance when modifying this object *)
    Procedure   ReadObjBin(Stream:TStream);
    procedure   WriteObjBin(Stream:TStream);
    procedure   DefineProperties(Filer: TFiler);override;
  Protected
    (*  Extended persistence.
        The function ObjectHasData() is called by the normal VCL
        DefineProperties to determine if there is any data to save.
        The other methods are invoked before and after either loading or
        saving object data.

        NOTE: To extend the functionality of this object please override
        ReadObject() and WriteObject(). The object will take care of
        everything else. *)
    Function    ObjectHasData:Boolean;
    Procedure   BeforeReadObject;virtual;
    Procedure   AfterReadObject;virtual;
    Procedure   BeforeWriteObject;virtual;
    Procedure   AfterWriteObject;virtual;
    Procedure   ReadObject(Reader:TReader);virtual;
    Procedure   WriteObject(Writer:TWriter);virtual;
  Protected
    (* Call this to determine if the object is empty (holds no data) *)
    Function    GetEmpty:Boolean;virtual;
  Protected
    (* Actual Buffer implementation. Decendants must override and
       implement these methods. It does not matter where or how the
       data is stored - this is up to the implementation. *)
    Function    DoGetCapabilities:TBRBufferCapabilities;virtual;abstract;
    Function    DoGetDataSize:Int64;virtual;abstract;
    Procedure   DoReleaseData;virtual;abstract;
    Procedure   DoGrowDataBy(Const Value:Integer);virtual;abstract;
    Procedure   DoShrinkDataBy(Const Value:Integer);virtual;abstract;
    Procedure   DoReadData(Start:Int64;var Buffer;
                BufLen:Integer);virtual;abstract;
    Procedure   DoWriteData(Start:Int64;Const Buffer;
                BufLen:Integer);virtual;abstract;
    Procedure   DoFillData(Start:Int64;FillLength:Int64;
                Const Data;DataLen:Integer);virtual;
    Procedure   DoZeroData;virtual;
  Public
    Property    Empty:Boolean read GetEmpty;
    Property    Capabilities:TBRBufferCapabilities read FCaps;
    Property    Size:Int64 read DoGetDataSize write SetSize;

    Procedure   Assign(Source:TPersistent);Override;

    (* Read from buffer into memory *)
    Function    Read(Const aByteIndex:Int64;
                aLength:Integer;var aValue):Integer;overload;

    (* Write to buffer from memory *)
    Function    Write(Const aByteIndex:Int64;
                aLength:Integer;Const aValue):Integer;overload;

    (* Append data to end-of-buffer from various sources *)
    Procedure   Append(Const aBuffer:TBRBuffer);overload;
    Procedure   Append(Const aStream:TStream);overload;
    Procedure   Append(Const aValue;Const aLength:Integer);overload;

    (* Fill the buffer with a repeating pattern of data *)
    Function    Fill(Const aByteIndex:Int64;
                Const aFillLength:Int64;
                Const aSource;Const aLength:Integer):Int64;

    (* Fill the buffer with the value zero *)
    Procedure   Zero;

    (*  Insert data into the buffer. Note: This is not a simple "overwrite"
        insertion. It will inject the new data and push whatever data is
        successive to the byteindex forward *)
    Procedure   Insert(Const aByteIndex:Int64;
                Const aSource;aLength:Integer);overload;
    Procedure   Insert(aByteIndex:Int64;Const aSource:TBRBuffer);overload;

    (* Remove X number of bytes from the buffer at any given position *)
    Procedure   Remove(Const aByteIndex:Int64;aLength:Integer);

    (* Simple binary search inside the buffer *)
    Function    Search(Const aSource;Const aLength:Integer;
                out aFoundByteIndex:Int64):Boolean;

    (*  Push data into the buffer from the beginning, which moves the current
        data already present forward *)
    Function    Push(Const aSource;aLength:Integer):Integer;

    (* Poll data out of buffer, again starting at the beginning of the buffer.
       The polled data is removed from the buffer *)
    Function    Poll(Var aTarget;aLength:Integer):Integer;

    (* Generate a normal DWORD ELF-hashcode from the content *)
    Function    HashCode:Longword;virtual;

    (* Standard IO methods. Please note that these are different from those
       used by persistence. These methods does not tag the content but loads
       it directly. The methods used by persistentse will tag the data with
       a length variable *)
    Procedure   LoadFromFile(Filename:String);
    procedure   SaveToFile(Filename:String);
    Procedure   SaveToStream(Stream:TStream);
    procedure   LoadFromStream(Stream:TStream);

    (* Export data from the buffer into various output targets *)
    Function    ExportTo(aByteIndex:Int64;
                aLength:Integer;Const Writer:TWriter):Integer;

    (* Import data from various input sources *)
    Function    ImportFrom(aByteIndex:Int64;
                aLength:Integer;Const Reader:TReader):Integer;

    (* release the current content of the buffer *)
    Procedure   Release;
    Procedure   AfterConstruction;Override;
    Procedure   BeforeDestruction;Override;

    Class Procedure FillByte(dstAddr:PByte;
          const inCount:Integer;const aValue:Byte);

    Class Procedure FillWord(dstAddr:PWord;
          Const inCount:Integer;Const Value:Word);

    Class Procedure FillTriple(dstAddr:PBRTripleByte;
          Const inCount:Integer;Const Value:TBRTripleByte);

    Class Procedure FillLong(dstAddr:PLongword;Const inCount:Integer;
          Const Value:Longword);
  End;

  (* This class implements a memory buffer. It allows you to allocate and
     manipulate data in-memory, more or less like AllocMem but with
     extended functionality *)
  TBRBufferMemory = Class(TBRBuffer)
  Private
    FDataPTR:   PByte;
    FDataLen:   Integer;
  Protected
    Function    BasePTR:PByte;
    Function    AddrOf(Const ByteIndex:Int64):PByte;
  Protected
    Function    DoGetCapabilities:TBRBufferCapabilities;override;
    Function    DoGetDataSize:Int64;override;
    Procedure   DoReleaseData;override;

    Procedure   DoReadData(Start:Int64;var Buffer;
                BufLen:Integer);override;

    Procedure   DoWriteData(Start:Int64;Const Buffer;
                BufLen:Integer);override;

    Procedure   DoFillData(Start:Int64;FillLength:Int64;
                Const Data;DataLen:Integer);override;

    Procedure   DoGrowDataBy(Const Value:Integer);override;
    Procedure   DoShrinkDataBy(Const Value:Integer);override;
    Procedure   DoZeroData;override;
  public
    {$IFDEF BR_SUPPORT_INTERNET}
    Procedure   LoadFromURL(RemoteURL:String);
    {$ENDIF}
  End;

  (* This class implements a file-based buffer *)
  TBRBufferFile = Class(TBRBuffer)
  Private
    FFilename:  String;
    FFile:      TStream;
  Protected
    Function    GetActive:Boolean;
    Function    GetEmpty:Boolean;override;
  Protected
    Function    DoGetCapabilities:TBRBufferCapabilities;override;
    Function    DoGetDataSize:Int64;override;
    Procedure   DoReleaseData;override;
    Procedure   DoReadData(Start:Int64;var Buffer;
                BufLen:Integer);override;
    Procedure   DoWriteData(Start:Int64;Const Buffer;
                BufLen:Integer);override;
    Procedure   DoGrowDataBy(Const Value:Integer);override;
    Procedure   DoShrinkDataBy(Const Value:Integer);override;
  Public
    Property    Filename:String read FFilename;
    Property    Active:Boolean read GetActive;
    Procedure   Open(aFilename:String;StreamFlags:Word);virtual;
    Procedure   Close;virtual;
  Public
    Procedure   BeforeDestruction;Override;
    Constructor Create(aFilename:String;StreamFlags:Word);overload;virtual;
  End;

  implementation

  {$IFDEF BR_SUPPORT_INTERNET}
  {$IFDEF MSWINDOWS}
  uses wintypes, comObj, ActiveX, UrlMon;
  {$ENDIF}
  {$ENDIF}

  Const
  CNT_ERR_BTRG_BASE  = 'Method %s threw exception %s with %s';

  CNT_ERR_BTRG_RELEASENOTSUPPORTED
  = 'Buffer capabillities does not allow release';

  CNT_ERR_BTRG_SCALENOTSUPPORTED
  = 'Buffer capabillities does not allow scaling';

  CNT_ERR_BTRG_READNOTSUPPORTED
  = 'Buffer capabillities does not allow read';

  CNT_ERR_BTRG_WRITENOTSUPPORTED
  = 'Buffer capabillities does not allow write';

  CNT_ERR_BTRG_SOURCEREADNOTSUPPORTED
  = 'Capabillities of datasource does not allow read';

  CNT_ERR_BTRG_TARGETWRITENOTSUPPORTED
  = 'Capabillities of data-target does not allow write';

  CNT_ERR_BTRG_SCALEFAILED
  = 'Memory scale operation failed: %s';

  CNT_ERR_BTRG_BYTEINDEXVIOLATION
  = 'Memory byte index violation, expected %d..%d not %d';

  CNT_ERR_BTRG_INVALIDDATASOURCE
  = 'Invalid data-source for operation';

  CNT_ERR_BTRG_INVALIDDATATARGET
  = 'Invalid data-target for operation';

  CNT_ERR_BTRG_EMPTY
  = 'Memory resource contains no data error';

  CNT_ERR_BTRG_NOTACTIVE
  = 'File is not active error';

  CNT_ERR_BTRGSTREAM_INVALIDBUFFER
  = 'Invalid buffer error, buffer is NIL';


  //##########################################################################
  // TBRBufferFile
  //##########################################################################

  Constructor TBRBufferFile.Create(aFilename:String;StreamFlags:Word);
  Begin
    inherited Create;

    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}

    self.Open(aFilename,StreamFlags);

    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['Create',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  Procedure TBRBufferFile.BeforeDestruction;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}

    If FFile<>NIL then
    begin
      FreeAndNIL(FFile);
      FFilename:='';
    end;

    inherited;

    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['BeforeDestruction',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  Function TBRBufferFile.DoGetCapabilities:TBRBufferCapabilities;
  Begin
    Result:=[mcScale,mcOwned,mcRead,mcWrite];
  end;

  Function TBRBufferFile.GetEmpty:Boolean;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}

    if FFile<>NIL then
    result:=(FFile.Size=0) else
    result:=True;

    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['GetEmpty',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  Function TBRBufferFile.GetActive:Boolean;
  Begin
    result:=assigned(FFile);
  end;

  Procedure TBRBufferFile.Open(aFilename:String;StreamFlags:Word);
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}
    If assigned(FFile) then
    Close;

    try
      FFile:=TFileStream.Create(aFilename,StreamFlags);
    except
      on e: exception do
      Raise EBRBufferError.Create(e.message);
    end;

    FFileName:=aFilename;

    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['Open',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  Procedure TBRBufferFile.Close;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}

    If FFile<>NIL then
    begin
      FreeAndNIL(FFile);
      FFilename:='';
    end;

    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['Close',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  Function TBRBufferFile.DoGetDataSize:Int64;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}

    if FFile<>NIL then
    result:=FFile.Size else
    result:=0;

    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['DoGetDataSize',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  Procedure TBRBufferFile.DoReleaseData;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}

    if FFile<>NIL then
    FFile.Size:=0;

    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['DoGetDataSize',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  Procedure TBRBufferFile.DoReadData(Start:Int64;var Buffer;BufLen:Integer);
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}

    if assigned(FFile) then
    Begin
      FFile.Position:=Start;
      FFile.ReadBuffer(Buffer,BufLen);
    end else
    Raise EBRBufferError.Create(CNT_ERR_BTRG_NOTACTIVE);

    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['DoReadData',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  Procedure TBRBufferFile.DoWriteData(Start:Int64;Const Buffer;
            BufLen:Integer);
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}

    if assigned(FFile) then
    Begin
      FFile.Position:=Start;
      FFile.WriteBuffer(Buffer,BufLen);
    end else
    Raise EBRBufferError.Create(CNT_ERR_BTRG_NOTACTIVE);

    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['DoWriteData',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  Procedure TBRBufferFile.DoGrowDataBy(Const Value:Integer);
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}

    if assigned(FFile) then
    FFile.Size:=FFile.Size + Value else
    Raise EBRBufferError.Create(CNT_ERR_BTRG_NOTACTIVE);

    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['DoGrowDataBy',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  Procedure TBRBufferFile.DoShrinkDataBy(Const Value:Integer);
  var
    mNewSize: Int64;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}

    if assigned(FFile) then
    Begin
      mNewSize:=FFile.Size - Value;
      If mNewSize>0 then
      FFile.Size:=mNewSize else
      DoReleaseData;
    end else
    Raise EBRBufferError.Create(CNT_ERR_BTRG_NOTACTIVE);

    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['DoShrinkDataBy',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  //##########################################################################
  // TBRStreamAdapter
  //##########################################################################

  Function TBRBufferMemory.BasePTR:PByte;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}

    (* Any pointer to return? *)
    If FDataPTR<>NIL then
    Result:=FDataPTR else
    Raise EBRBufferError.Create(CNT_ERR_BTRG_EMPTY);

    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['BasePTR',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  Function TBRBufferMemory.AddrOf(Const ByteIndex:Int64):PByte;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}

    (* Are we within range of the buffer? *)
    If (ByteIndex>=0) and (ByteIndex<FDataLen) then
    Begin
      (* Note: If you have modified this class to working with
      memory-mapped files and go beyond MAXINT in the byteindex,
      Delphi can raise an EIntOverFlow exception *)
      result:=FDataPTR;
      inc(result,ByteIndex);
    end else
    Raise EBRBufferError.CreateFmt
    (CNT_ERR_BTRG_BYTEINDEXVIOLATION,[0,FDataLEN,ByteIndex]);

    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['AddrOf',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  Function TBRBufferMemory.DoGetCapabilities:TBRBufferCapabilities;
  Begin
    Result:=[mcScale,mcOwned,mcRead,mcWrite];
  end;

  Function TBRBufferMemory.DoGetDataSize:Int64;
  Begin
    If FDataPTR<>NIL then
    Result:=FDataLen else
    result:=0;
  end;

  Procedure TBRBufferMemory.DoReleaseData;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}

    try
      If FDataPTR<>NIL then
      Freemem(FDataPTR);
    finally
      FDataPTR:=NIL;
      FDataLEN:=0;
    end;

    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['DoReleaseData',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  Procedure TBRBufferMemory.DoReadData(Start:Int64;
            var Buffer;BufLen:Integer);
  var
    mSource:    PByte;
    mTarget:    PByte;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}

    (* Calculate PTR's *)
    mSource:=AddrOf(Start);
    mTarget:=Addr(Buffer);
    move(mSource^,mTarget^,BufLen);

    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['DoReadData',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  Procedure TBRBufferMemory.DoWriteData(Start:Int64;
            Const Buffer;BufLen:Integer);
  var
    mSource:    PByte;
    mTarget:    PByte;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}

    (* Calculate PTR's *)
    mSource:=Addr(Buffer);
    mTarget:=AddrOf(start);
    move(mSource^,mTarget^,BufLen);

    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['DoWriteData',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  Procedure TBRBufferMemory.DoGrowDataBy(Const Value:Integer);
  var
    mNewSize: Integer;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}

    try
      If FDataPTR<>NIL then
      Begin
        (* Re-scale current memory *)
        mNewSize:=FDataLEN + Value;
        ReAllocMem(FDataPTR,mNewSize);
        FDataLen:=mNewSize;
      end else
      Begin
        (* Allocate new memory *)
        FDataPTR:=AllocMem(Value);
        FDataLen:=Value;
      end;
    except
      on e: exception do
      Begin
        FDataLen:=0;
        FDataPTR:=NIL;
        Raise EBRBufferError.CreateFmt
        (CNT_ERR_BTRG_SCALEFAILED,[e.message]);
      end;
    end;

    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['DoGrowDataBy',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  Procedure TBRBufferMemory.DoShrinkDataBy(Const Value:Integer);
  var
    mNewSize: Integer;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}

    If FDataPTR<>NIL then
    Begin
      mNewSize:=math.EnsureRange(FDataLEN - Value,0,FDataLen);
      If mNewSize>0 then
      Begin
        if mNewSize<>FDataLen then
        Begin
          try
            ReAllocMem(FDataPTR,mNewSize);
            FDataLen:=mNewSize;
          except
            on e: exception do
            Begin
              Raise EBRBufferError.CreateFmt
              (CNT_ERR_BTRG_SCALEFAILED,[e.message]);
            end;
          end;
        end;
      end else
      DoReleaseData;
    end else
    Raise EBRBufferError.Create(CNT_ERR_BTRG_EMPTY);

    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['DoShrinkDataBy',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  Procedure TBRBufferMemory.DoZeroData;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}

    If FDataPTR<>NIL then
    TBRBuffer.FillByte(FDataPTR,FDataLen,0) else
    Raise EBRBufferError.Create(CNT_ERR_BTRG_EMPTY);

    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['DoZeroData',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  Procedure TBRBufferMemory.DoFillData(Start:Int64;
            FillLength:Int64;Const Data;DataLen:Integer);
  var
    FSource:    PByte;
    FTarget:    PByte;
    FLongs:     Integer;
    FSingles:   Integer;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}
    (* Initialize pointers *)
    FSource:=Addr(Data);
    FTarget:=self.AddrOf(Start);

    (* EVEN copy source into destination *)
    FLongs:=FillLength div DataLen;
    While FLongs>0 do
    Begin
      Move(FSource^,FTarget^,DataLen);
      inc(FTarget,DataLen);
      dec(FLongs);
    end;

    (* ODD copy of source into destination *)
    FSingles:=FillLength mod DataLen;
    If FSingles>0 then
    Begin
      Case FSingles of
      1: FTarget^:=FSource^;
      2: PWord(FTarget)^:=PWord(FSource)^;
      3: PBRTripleByte(FTarget)^:=PBRTripleByte(FSource)^;
      4: PLongword(FTarget)^:=PLongword(FSource)^;
      else
        Move(FSource^,FTarget^,FSingles);
      end;
    end;
    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['DoFillData',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  {$IFDEF BR_SUPPORT_INTERNET}
  Procedure TBRBufferMemory.LoadFromURL(RemoteURL:String);
  {$IFDEF MSWINDOWS}
  var
    ppStream: IStream;
    statstg:  TStatStg;
    lpBuffer: Pointer;
    dwRead:   Integer;
    mRes:     HResult;
  {$ENDIF}
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}

    {$IFDEF MSWINDOWS}
    mRes:=URLOpenBlockingStream(nil,PChar(RemoteURL),ppStream,0,nil);
    if mRes = S_OK then
    begin
      try
        if ppStream.Stat(statstg, STATFLAG_NONAME)=S_OK then
        begin
          if (statstg.cbSize > 0) then
          begin
            lpBuffer:=AllocMem(statstg.cbSize);
            try
              (* This should be optimized.
                 Here we read the whole file into a secondary buffer.
                 It would be wiser to read the file in chunks to avoid
                 out of memory errors. *)
              if ppStream.Read(lpBuffer, statstg.cbSize, @dwRead) = S_OK then
              Append(lpBuffer^,dwRead) else
              Raise Exception.Create('Failed to read from network stream');
            finally
              FreeMem(lpBuffer);
            end;
          end;
        end;
      finally
        ppStream:=nil;
      end;
    end else
    if mRes=E_OUTOFMEMORY then
    Raise Exception.Create('Not enough memory for operation error');
    {$ENDIF}

    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['DoFillData',e.classname,e.message]);
    end;
    {$ENDIF}
  end;
  {$ENDIF}

  //##########################################################################
  // TBRBuffer
  //##########################################################################

  Procedure TBRBuffer.BeforeDestruction;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}

    (* release memory if capabillities allow it *)
    Release;
    inherited;

    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['BeforeDestruction',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  Procedure TBRBuffer.AfterConstruction;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}

    inherited;

    (* Get memory capabillities *)
    FCaps:=DoGetCapabilities;

    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['AfterConstruction',e.classname,e.message]);
    end;
    {$ENDIF}
  end;


  Class Procedure TBRBuffer.FillByte(dstAddr:PByte;
        const inCount:Integer;const aValue:Byte);
  var
    mBytes: Integer;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}

    mBytes:=inCount;
    While mBytes>0 do
    Begin
      dstAddr^:=aValue;
      dec(mBytes);
      inc(dstAddr);
    end;

    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['FillByte',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  Class Procedure TBRBuffer.FillWord(dstAddr:system.PWord;
            Const inCount:Integer;Const Value:System.Word);
  var
    FTemp:  Longword;
    FLongs: Integer;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}
    FTemp:=Value shl 16 or Value;
    FLongs:=inCount shr 3;
    while FLongs>0 do
    Begin
      PLongword(dstAddr)^:=FTemp; inc(PLongword(dstAddr));
      PLongword(dstAddr)^:=FTemp; inc(PLongword(dstAddr));
      PLongword(dstAddr)^:=FTemp; inc(PLongword(dstAddr));
      PLongword(dstAddr)^:=FTemp; inc(PLongword(dstAddr));
      dec(FLongs);
    end;

    Case inCount mod 8 of
    1:  dstAddr^:=Value;
    2:  PLongword(dstAddr)^:=FTemp;
    3:  Begin
          PLongword(dstAddr)^:=FTemp; inc(PLongword(dstAddr));
          dstAddr^:=Value;
        end;
    4:  Begin
          PLongword(dstAddr)^:=FTemp; inc(PLongword(dstAddr));
          PLongword(dstAddr)^:=FTemp;
        end;
    5:  Begin
          PLongword(dstAddr)^:=FTemp; inc(PLongword(dstAddr));
          PLongword(dstAddr)^:=FTemp; inc(PLongword(dstAddr));
          dstAddr^:=Value;
        end;
    6:  Begin
          PLongword(dstAddr)^:=FTemp; inc(PLongword(dstAddr));
          PLongword(dstAddr)^:=FTemp; inc(PLongword(dstAddr));
          PLongword(dstAddr)^:=FTemp;
        end;
    7:  Begin
          PLongword(dstAddr)^:=FTemp; inc(PLongword(dstAddr));
          PLongword(dstAddr)^:=FTemp; inc(PLongword(dstAddr));
          PLongword(dstAddr)^:=FTemp; inc(PLongword(dstAddr));
          dstAddr^:=Value;
        end;
    end;
    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['FillWord',e.classname,e.message]);
    end;
    {$ENDIF}
  end;


  Class Procedure TBRBuffer.FillTriple(dstAddr:PBRTripleByte;
            Const inCount:Integer;Const Value:TBRTripleByte);
  var
    FLongs: Integer;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}
    FLongs:=inCount shr 3;
    While FLongs>0 do
    Begin
      dstAddr^:=Value;inc(dstAddr);
      dstAddr^:=Value;inc(dstAddr);
      dstAddr^:=Value;inc(dstAddr);
      dstAddr^:=Value;inc(dstAddr);
      dstAddr^:=Value;inc(dstAddr);
      dstAddr^:=Value;inc(dstAddr);
      dstAddr^:=Value;inc(dstAddr);
      dstAddr^:=Value;inc(dstAddr);
      dec(FLongs);
    end;

    Case (inCount mod 8) of
    1:  dstAddr^:=Value;
    2:  Begin
          dstAddr^:=Value;inc(dstAddr);
          dstAddr^:=Value;
        end;
    3:  Begin
          dstAddr^:=Value;inc(dstAddr);
          dstAddr^:=Value;inc(dstAddr);
          dstAddr^:=Value;
        end;
    4:  Begin
          dstAddr^:=Value;inc(dstAddr);
          dstAddr^:=Value;inc(dstAddr);
          dstAddr^:=Value;inc(dstAddr);
          dstAddr^:=Value;
        end;
    5:  Begin
          dstAddr^:=Value;inc(dstAddr);
          dstAddr^:=Value;inc(dstAddr);
          dstAddr^:=Value;inc(dstAddr);
          dstAddr^:=Value;inc(dstAddr);
          dstAddr^:=Value;
        end;
    6:  Begin
          dstAddr^:=Value;inc(dstAddr);
          dstAddr^:=Value;inc(dstAddr);
          dstAddr^:=Value;inc(dstAddr);
          dstAddr^:=Value;inc(dstAddr);
          dstAddr^:=Value;inc(dstAddr);
          dstAddr^:=Value;
        end;
    7:  Begin
          dstAddr^:=Value;inc(dstAddr);
          dstAddr^:=Value;inc(dstAddr);
          dstAddr^:=Value;inc(dstAddr);
          dstAddr^:=Value;inc(dstAddr);
          dstAddr^:=Value;inc(dstAddr);
          dstAddr^:=Value;inc(dstAddr);
          dstAddr^:=Value;
        end;
    end;
    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['FillTriple',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  Class Procedure TBRBuffer.FillLong(dstAddr:PLongword;
        Const inCount:Integer;Const Value:Longword);
  var
    FLongs: Integer;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}
    FLongs:=inCount shr 3;
    While FLongs>0 do
    Begin
      dstAddr^:=Value; inc(dstAddr);
      dstAddr^:=Value; inc(dstAddr);
      dstAddr^:=Value; inc(dstAddr);
      dstAddr^:=Value; inc(dstAddr);
      dstAddr^:=Value; inc(dstAddr);
      dstAddr^:=Value; inc(dstAddr);
      dstAddr^:=Value; inc(dstAddr);
      dstAddr^:=Value; inc(dstAddr);
      dec(FLongs);
    end;

    Case inCount mod 8 of
    1:  dstAddr^:=Value;
    2:  Begin
          dstAddr^:=Value; inc(dstAddr);
          dstAddr^:=Value;
        end;
    3:  Begin
          dstAddr^:=Value; inc(dstAddr);
          dstAddr^:=Value; inc(dstAddr);
          dstAddr^:=Value;
        end;
    4:  Begin
          dstAddr^:=Value; inc(dstAddr);
          dstAddr^:=Value; inc(dstAddr);
          dstAddr^:=Value; inc(dstAddr);
          dstAddr^:=Value;
        end;
    5:  Begin
          dstAddr^:=Value; inc(dstAddr);
          dstAddr^:=Value; inc(dstAddr);
          dstAddr^:=Value; inc(dstAddr);
          dstAddr^:=Value; inc(dstAddr);
          dstAddr^:=Value;
        end;
    6:  Begin
          dstAddr^:=Value; inc(dstAddr);
          dstAddr^:=Value; inc(dstAddr);
          dstAddr^:=Value; inc(dstAddr);
          dstAddr^:=Value; inc(dstAddr);
          dstAddr^:=Value; inc(dstAddr);
          dstAddr^:=Value;
        end;
    7:  Begin
          dstAddr^:=Value; inc(dstAddr);
          dstAddr^:=Value; inc(dstAddr);
          dstAddr^:=Value; inc(dstAddr);
          dstAddr^:=Value; inc(dstAddr);
          dstAddr^:=Value; inc(dstAddr);
          dstAddr^:=Value; inc(dstAddr);
          dstAddr^:=Value;
        end;
    end;
    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['FillLong',e.classname,e.message]);
    end;
    {$ENDIF}
  end;


  Procedure TBRBuffer.Assign(Source:TPersistent);
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}
    If Source<>NIl then
    Begin
      If (Source is TBRBuffer) then
      Begin
        Release;
        Append(TBRBuffer(source));
      end else
      Inherited;
    end else
    Inherited;
    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['Assign',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  Function TBRBuffer.ObjectHasData:Boolean;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}
    result:=DoGetDataSize>0;
    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['ObjectHasData',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  Function TBRBuffer.GetEmpty:Boolean;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}
    result:=DoGetDataSize<=0;
    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['GetEmpty',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  Procedure TBRBuffer.BeforeReadObject;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}
    if (mcOwned in FCaps) then
    Release else
    Raise EBRBufferError.Create(CNT_ERR_BTRG_RELEASENOTSUPPORTED);
    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['BeforeReadObject',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  Procedure TBRBuffer.AfterReadObject;
  Begin
  end;

  Procedure TBRBuffer.BeforeWriteObject;
  Begin
  end;

  Procedure TBRBuffer.AfterWriteObject;
  Begin
  end;

  Procedure TBRBuffer.ReadObject(Reader:TReader);
  var
    mTotal: Int64;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}
    Reader.Read(mTotal,SizeOf(mTotal));
    If mTotal>0 then
    ImportFrom(0,mTotal,Reader);
    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['ReadObject',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  Procedure TBRBuffer.WriteObject(Writer:TWriter);
  var
    mSize:  Int64;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}
    mSize:=Size;
    Writer.write(mSize,SizeOf(mSize));
    if mSize>0 then
    self.ExportTo(0,mSize,Writer);
    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['WriteObject',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  Procedure TBRBuffer.ReadObjBin(Stream:TStream);
  var
    mReader:  TReader;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}
    mReader:=TReader.Create(Stream,1024);
    try
      BeforeReadObject;
      ReadObject(mReader);
    finally
      mReader.free;
      AfterReadObject;
    end;
    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['ReadObjBin',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  procedure TBRBuffer.WriteObjBin(Stream:TStream);
  var
    mWriter:  TWriter;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}
    mWriter:=TWriter.Create(Stream,1024);
    try
      BeforeWriteObject;
      WriteObject(mWriter);
    finally
      mWriter.FlushBuffer;
      mWriter.free;
      AfterWriteObject;
    end;
    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['WriteObjBin',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  procedure TBRBuffer.DefineProperties(Filer:TFiler);
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}
    inherited;
    Filer.DefineBinaryProperty('IO_BIN',ReadObjBin,WriteObjBin,ObjectHasData);
    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['DefineProperties',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  Procedure TBRBuffer.DoFillData(Start:Int64;FillLength:Int64;
            Const Data;DataLen:Integer);
  var
    mToWrite: Integer;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}
    While FillLength>0 do
    Begin
      mToWrite:=math.EnsureRange(Datalen,1,FillLength);
      DoWriteData(Start,Data,mToWrite);
      FillLength:=FillLength - mToWrite;
      Start:=Start + mToWrite;
    end;
    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['DoFillData',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  Function  TBRBuffer.Fill(Const aByteIndex:Int64;
            Const aFillLength:Int64;
            Const aSource;Const aLength:Integer):Int64;
  var
    mTotal: Int64;
    mTemp:  Int64;
    mAddr:  pByte;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}
    (* Initialize *)
    result:=0;

    (* Are we empty? *)
    If not Empty then
    Begin
      (* Check write capabilities *)
      If mcWrite in FCaps then
      Begin
        (* Check length[s] of data *)
        If  (aFillLength>0)
        and (aLength>0) then
        Begin
          (* check data-source *)
          mAddr:=addr(aSource);
          If mAddr<>NIl then
          Begin

            (* Get total range *)
            mTotal:=DoGetDataSize;

            (* Check range entrypoint *)
            if (aByteIndex>=0) and (aByteIndex<mTotal) then
            Begin

              (* Does fill exceed range? *)
              mTemp:=aByteIndex + aFillLength;
              if mTemp>mTotal then
              mTemp:=(mTotal - aByteIndex) else // Yes, clip it
              mTemp:=aFillLength;               // No, length is fine

              (* fill range *)
              DoFillData(aByteIndex,mTemp,mAddr^,aLength);

              (* return size of region filled *)
              result:=mTemp;

            end else
            Raise EBRBufferError.CreateFmt
            (CNT_ERR_BTRG_BYTEINDEXVIOLATION,[0,mTotal-1,aByteIndex]);

          end else
          Raise EBRBufferError.Create(CNT_ERR_BTRG_INVALIDDATASOURCE);

        end;
      end else
      Raise EBRBufferError.Create(CNT_ERR_BTRG_WRITENOTSUPPORTED);
    end else
    Raise EBRBufferError.Create(CNT_ERR_BTRG_EMPTY);
    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['Fill',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  Procedure TBRBuffer.DoZeroData;
  var
    mSize:  Int64;
    mAlign: Int64;
    mCache: TBRIOCache;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}
    (* Get size in bytes of buffer *)
    mSize:=DoGetDataSize;

    (* Take widechar into account *)
    mAlign:=mSize div SizeOf(char);

    (* fill our temp buffer *)
    TBRBuffer.FillByte(@mCache,mAlign,0);

    (* Perform internal fill *)
    Fill(0,mSize,mCache,SizeOf(mCache));
    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['DoZeroData',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  Procedure TBRBuffer.Zero;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}
    If not Empty then
    Begin
      If mcWrite in FCaps then
      DoZeroData else
      Raise EBRBufferError.Create(CNT_ERR_BTRG_WRITENOTSUPPORTED);
    end else
    Raise EBRBufferError.Create(CNT_ERR_BTRG_EMPTY);
    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['Zero',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  Procedure TBRBuffer.Append(Const aBuffer:TBRBuffer);
  var
    mOffset:      Int64;
    mTotal:       Int64;
    mRead:        Integer;
    mBytesToRead: Integer;
    mCache:       TBRIOCache;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}
    If mcScale in FCaps then
    Begin
      If mcWrite in FCaps then
      Begin
        If aBuffer<>NIL then
        Begin
          (* does the source support read caps? *)
          If (mcRead in aBuffer.Capabilities) then
          Begin

            mOffset:=0;
            mTotal:=aBuffer.Size;

            Repeat
              mBytesToRead:=Math.EnsureRange(SizeOf(mCache),0,mTotal);
              mRead:=aBuffer.Read(mOffset,mBytesToRead,mCache);
              If mRead>0 then
              Begin
                Append(mCache,mRead);
                mTotal:=mTotal - mRead;
                mOffset:=mOffset + mRead;
              end;
            Until (mBytesToRead<1) or (mRead<1);

          end else
          Raise EBRBufferError.Create(CNT_ERR_BTRG_SOURCEREADNOTSUPPORTED);

        end else
        Raise EBRBufferError.Create(CNT_ERR_BTRG_INVALIDDATASOURCE);
      end else
      Raise EBRBufferError.Create(CNT_ERR_BTRG_WRITENOTSUPPORTED);
    end else
    Raise EBRBufferError.Create(CNT_ERR_BTRG_SCALENOTSUPPORTED);
    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['Append',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  Procedure TBRBuffer.Append(Const aStream:TStream);
  var
    mTotal:       Int64;
    mRead:        Integer;
    mBytesToRead: Integer;
    mCache:       TBRIOCache;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}
    If mcScale in FCaps then
    Begin
      If mcWrite in FCaps then
      Begin
        If aStream<>NIL then
        Begin
          mTotal:=(aStream.Size-aStream.Position);
          If mTotal>0 then
          Begin

            Repeat
              mBytesToRead:=Math.EnsureRange(SizeOf(mCache),0,mTotal);
              mRead:=aStream.Read(mCache,mBytesToRead);
              If mRead>0 then
              Begin
                Append(mCache,mRead);
                mTotal:=mTotal - mRead;
              end;
            Until (mBytesToRead<1) or (mRead<1);

          end;
        end else
        Raise EBRBufferError.Create(CNT_ERR_BTRG_INVALIDDATASOURCE);
      end else
      Raise EBRBufferError.Create(CNT_ERR_BTRG_WRITENOTSUPPORTED);
    end else
    Raise EBRBufferError.Create(CNT_ERR_BTRG_SCALENOTSUPPORTED);
    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['Append',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  Procedure TBRBuffer.Append(Const aValue;Const aLength:Integer);
  var
    mOffset: Int64;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}
    If mcScale in FCaps then
    Begin
      If mcWrite in FCaps then
      Begin
        If aLength>0 then
        Begin
          mOffset:=DoGetDataSize;
          DoGrowDataBy(aLength);
          DoWriteData(mOffset,aValue,aLength);
        end;
      end else
      Raise EBRBufferError.Create(CNT_ERR_BTRG_WRITENOTSUPPORTED);
    end else
    Raise EBRBufferError.Create(CNT_ERR_BTRG_SCALENOTSUPPORTED);
    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['Append',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  (*  Method:   Release()
      Purpose:  This method releases any content contained by the
                buffer. It is equal to Freemem in function *)
  Procedure TBRBuffer.Release;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}
    (* Is the content owned/managed by us? *)
    If mcOwned in FCaps then
    DoReleaseData else
    Raise EBRBufferError.Create(CNT_ERR_BTRG_RELEASENOTSUPPORTED);
    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['Release',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  (*  Method:   Search()
      Purpose:  This method is used to define a new size of the current
                buffer. It will scale the buffer to fit the new size,
                including grow or shrink the data *)
  Procedure TBRBuffer.SetSize(Const aNewSize:Int64);
  var
    mFactor:  Int64;
    mOldSize: Int64;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}
    If (mcScale in FCaps) then
    Begin
      If aNewSize>0 then
      Begin
        (* Get current size *)
        mOldSize:=DoGetDataSize;

        (* Get difference between current size & new size *)
        mFactor:=abs(mOldSize-aNewSize);

        (* only act if we need to *)
        If mFactor>0 then
        Begin
          try
            (* grow or shrink? *)
            If aNewSize>mOldSize then
            DoGrowDataBy(mFactor) else

            if aNewSize<mOldSize then
            DoShrinkDataBy(mFactor);
          except
            on e: exception do
            Raise EBRBufferError.CreateFmt
            (CNT_ERR_BTRG_SCALEFAILED,[e.message]);
          end;
        end;
      end else
      Release;
    end else
    Raise EBRBufferError.Create(CNT_ERR_BTRG_SCALENOTSUPPORTED);
    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['SetSize',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  (*  Method:   Search()
      Purpose:  The search method allows you to perform a simple binary
                search inside the buffer.
      Comments: The search does not yet deploy caching of data, so on
                larger buffers it may be slow *)
  Function  TBRBuffer.Search(Const aSource;Const aLength:Integer;
            out aFoundByteIndex:Int64):Boolean;
  var
    mTotal:   Int64;
    mToScan:  Int64;
    src:      PByte;
    mByte:    Byte;
    mOffset:  Int64;
    x:        Int64;
    y:        Int64;
    mRoot:    PByte;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}
    (* Initialize *)
    result:=False;
    aFoundByteIndex:=-1;

    (* Get search pointer *)
    mRoot:=addr(aSource);

    (* Valid pointer? *)
    if (mRoot<>NIl) and (aLength>0) then
    Begin

      (* Check read capabilities of buffer *)
      if (mcRead in FCaps) then
      Begin
        (* get total size to scan *)
        mTotal:=doGetDataSize;

        (* do we have anything to work with? *)
        if (mTotal>0) and (mTotal>=aLength) then
        Begin

          (* how many bytes must we scan? *)
          mToScan:=mTotal - aLength;

          x:=0;
          While (x<=mToScan) do
          Begin
            (* setup source PTR *)
            src:=Addr(aSource);

            (* setup target offset *)
            mOffset:=x;

            (* check memory by sampling *)
            y:=1;
            while y<aLength do
            Begin
              (* break if not equal *)
              Read(mOffset,1,mByte);
              Result:=src^=mByte;
              If not result then
              break;

              inc(src);
              mOffset:=mOffset + 1;
              Y:=Y + 1;
            end;

            (* success? *)
            if Result then
            Begin
              aFoundByteIndex:=x;
              Break;
            end;

            x:=x + 1;
          end;
        end;
      end else
      Raise EBRBufferError.Create(CNT_ERR_BTRG_READNOTSUPPORTED);
    end;
    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['Search',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  (*  Method:   Insert()
      Purpose:  This function allows you to insert the content of another
                buffer anywhere in the current buffer. This operation does
                not overwrite any data currently in the buffer, but rather
                the new data is injected into the buffer - expanding the size
                and pushing the succeeding data forward *)
  Procedure TBRBuffer.Insert(aByteIndex:Int64;
            Const aSource:TBRBuffer);
  var
    mTotal:   Int64;
    mCache:   TBRIOCache;
    mRead:    Integer;
    mToRead:  Integer;
    mEntry:   Int64;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}
    (* Validate source PTR *)
    if aSource<>NIl then
    Begin
      (* Check that buffer supports scaling *)
      If (mcScale in FCaps) then
      Begin
        (* Check that buffer support write access *)
        If (mcWrite in FCaps) then
        Begin
          (* Check for read-access *)
          If (mcRead in aSource.Capabilities) then
          Begin
            (* Get size of source *)
            mTotal:=aSource.Size;

            (* Validate entry index *)
            if (aByteIndex>=0) then
            Begin

              (* anything to work with? *)
              if mTotal>0 then
              Begin

                mEntry:=0;
                While mTotal>0 do
                Begin
                  (* Clip data to read *)
                  mToRead:=SizeOf(mCache);
                  If mToRead>mTotal then
                  mToRead:=mTotal;

                  (* Read data from buffer *)
                  mRead:=aSource.Read(mEntry,mToRead,mCache);
                  If mRead>0 then
                  Begin
                    (* Insert data into our buffer *)
                    Insert(aByteIndex,mCache,mRead);

                    (* update positions *)
                    mEntry:=mEntry + mRead;
                    aByteIndex:=aByteIndex + mRead;
                    mTotal:=mTotal - mRead;
                  end else
                  Break;
                end;
              end;

            end else
            Raise EBRBufferError.CreateFmt
            (CNT_ERR_BTRG_BYTEINDEXVIOLATION,[0,mTotal-1,aByteIndex]);

          end else
          Raise EBRBufferError.Create(CNT_ERR_BTRG_READNOTSUPPORTED);
        end else
        Raise EBRBufferError.Create(CNT_ERR_BTRG_WRITENOTSUPPORTED);
      end else
      Raise EBRBufferError.Create(CNT_ERR_BTRG_SCALENOTSUPPORTED);
    end else
    Raise EBRBufferError.Create(CNT_ERR_BTRG_INVALIDDATASOURCE);
    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['Insert',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  (*  Method:   Insert()
      Purpose:  This function allows you to insert X number of bytes
                anywhere in the buffer. This operation does not overwrite
                any data currently in the buffer, but rather the new
                data is injected into the buffer - expanding the size
                and pushing the succeeding data forward *)
  Procedure TBRBuffer.Insert(Const aByteIndex:Int64;
            Const aSource;aLength:Integer);
  var
    mTotal:       Int64;
    mBytesToPush: Int64;
    mBytesToRead: Integer;
    mPosition:    Int64;
    mFrom:        Int64;
    mTo:          Int64;
    mData:        PByte;
    mCache:       TBRIOCache;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}
    (* Check that buffer supports scaling *)
    If (mcScale in FCaps) then
    Begin
      (* Check that buffer support write access *)
      If (mcWrite in FCaps) then
      Begin
        (* Make sure buffer supports read operations *)
        If (mcRead in FCaps) then
        Begin
          (* Check length *)
          If aLength>0 then
          Begin
            (* Check data-source *)
            mData:=@aSource;
            If mData<>NIL then
            Begin

              (* get current size *)
              mTotal:=DoGetDataSize;

              (* Insert into data? *)
              If (aByteIndex>=0) and (aByteIndex<mTotal) then
              Begin
                (* How many bytes should we push? *)
                mBytesToPush:=mTotal - aByteIndex;
                If mBytesToPush>0 then
                Begin
                  (* grow media to fit new data *)
                  DoGrowDataBy(aLength);

                  (* calculate start position *)
                  mPosition:=aByteIndex + mBytesToPush;

                  While mBytesToPush>0 do
                  Begin
                    (* calculate how much data to read *)
                    mBytesToRead:=Math.EnsureRange
                    (SizeOf(mCache),0,mBytesToPush);

                    (* calculate read & write positions *)
                    mFrom:=mPosition - mBytesToRead;
                    mTo:=mPosition - (mBytesToRead - aLength);

                    (* read data from the end *)
                    DoReadData(mFrom,mCache,mBytesToRead);

                    (* write data upwards *)
                    DoWriteData(mTo,mCache,mBytesToRead);

                    (* update offset values *)
                    mPosition:=mPosition - mBytesToRead;
                    mBytesToPush:=mBytesToPush - mBytesToRead;
                  end;

                  (* insert new data *)
                  DoWriteData(mPosition,aSource,aLength);

                end else
                DoWriteData(mTotal,aSource,aLength);
              end else

              (* if @ end, use append instead *)
              If aByteIndex=mTotal then
              Append(aSource,aLength) else

              (* outside of memory scope, raise exception *)
              Raise EBRBufferError.CreateFmt
              (CNT_ERR_BTRG_BYTEINDEXVIOLATION,[0,mTotal-1,aByteIndex]);

            end else
            Raise EBRBufferError.Create(CNT_ERR_BTRG_INVALIDDATASOURCE);

          end; {:length}
        end else
        Raise EBRBufferError.Create(CNT_ERR_BTRG_READNOTSUPPORTED);
      end else
      Raise EBRBufferError.Create(CNT_ERR_BTRG_WRITENOTSUPPORTED);
    end else
    Raise EBRBufferError.Create(CNT_ERR_BTRG_SCALENOTSUPPORTED);
    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['Insert',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  (*  Method:   Remove()
      Purpose:  This function allows you to remove X bytes of data from
                anywhere within the buffer. This is extremely handy
                when working with binary files, cabinet-files and other
                advanced file operations *)
  Procedure TBRBuffer.Remove(Const aByteIndex:Int64;aLength:Integer);
  var
    mTemp:      Integer;
    mTop:       Int64;
    mBottom:    Int64;
    mToRead:    Integer;
    mToPoll:    Int64;
    mPosition:  Int64;
    mTotal:     Int64;
    mCache:     TBRIOCache;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}
    (* Check that buffer supports scaling *)
    If (mcScale in FCaps) then
    Begin
      (* Check that buffer support write access *)
      If (mcWrite in FCaps) then
      Begin
        (* Make sure buffer supports read operations *)
        If mcRead in FCaps then
        Begin
          (* Validate remove length *)
          If aLength>0 then
          Begin
            (* get current size *)
            mTotal:=DoGetDataSize;

            (* Remove from data, or the whole thing? *)
            If (aByteIndex>=0) and (aByteIndex<mTotal) then
            Begin
              mTemp:=aByteIndex + aLength;
              If aLength<>mTotal then
              Begin
                If mTemp<mTotal then
                Begin
                  mToPoll:=mTotal - (aByteIndex + aLength);
                  mTop:=aByteIndex;
                  mBottom:=aByteIndex + aLength;

                  While mToPoll>0 do
                  Begin
                    mPosition:=mBottom;
                    mToRead:=Math.EnsureRange(SizeOf(mCache),0,mToPoll);

                    DoReadData(mPosition,mCache,mToRead);
                    DoWriteData(mTop,mCache,mToRead);

                    mTop:=mTop + mToRead;
                    mBottom:=mBottom + mToRead;
                    mToPoll:=mToPoll - mToRead;
                  end;
                  DoShrinkDataBy(aLength);
                end else
                Release;
              end else
              Begin
                (* Release while buffer? Or just clip at the end? *)
                If mTemp>mTotal then
                Release else
                DoShrinkDataBy(mTotal - aLength);
              end;

            end else
            Raise EBRBufferError.CreateFmt
            (CNT_ERR_BTRG_BYTEINDEXVIOLATION,[0,mTotal-1,aByteIndex]);
          end;
        end else
        Raise EBRBufferError.Create(CNT_ERR_BTRG_READNOTSUPPORTED);
      end else
      Raise EBRBufferError.Create(CNT_ERR_BTRG_WRITENOTSUPPORTED);
    end else
    Raise EBRBufferError.Create(CNT_ERR_BTRG_SCALENOTSUPPORTED);
    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['Remove',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  (*  Method:   Push()
      Purpose:  Allows you to insert X number of bytes at the beginning
                of the buffer. This is very handy and allows a buffer to
                be used in a "stack" fashion. *)
  Function TBRBuffer.Push(Const aSource;aLength:Integer):Integer;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}
    If not Empty then
    Insert(0,aSource,aLength) else
    Append(aSource,aLength);
    Result:=aLength;
    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['Push',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  (*  Method:   Poll()
      Purpose:  Allows you to extract X number of bytes from the buffer,
                the buffer will then re-scale itself and remove the bytes
                you polled automatically. Very handy "stack" function *)
  Function TBRBuffer.Poll(Var aTarget;aLength:Integer):Integer;
  var
    mTotal:   Int64;
    mRemains: Int64;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}
    (* Initialize *)
    Result:=0;

    (* Make sure buffer supports scaling *)
    If mcScale in FCaps then
    Begin
      (* check write rights *)
      If mcWrite in FCaps then
      Begin
        (* check read rights *)
        If mcRead in FCaps then
        Begin
          (* validate length of data to poll *)
          If aLength>0 then
          Begin
            (* get current size *)
            mTotal:=DoGetDataSize;
            If mTotal>0 then
            Begin
              (* calc how much data will remain *)
              mRemains:=mTotal - aLength;

              (* anything left afterwards? *)
              If mRemains>0 then
              Begin
                (* return data, keep the stub *)
                Result:=Read(0,aLength,aTarget);
                Remove(0,aLength);
              end else
              Begin
                (* return data, deplete buffer *)
                Result:=mTotal;
                DoReadData(0,aTarget,mTotal);
                Release;
              end;
            end;
          end;
        end else
        Raise EBRBufferError.Create(CNT_ERR_BTRG_READNOTSUPPORTED);
      end else
      Raise EBRBufferError.Create(CNT_ERR_BTRG_WRITENOTSUPPORTED);
    end else
    Raise EBRBufferError.Create(CNT_ERR_BTRG_SCALENOTSUPPORTED);
    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['Poll',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  (*  Method:   HashCode()
      Purpose:  Generate an Elf-hashcode [long] from buffer. *)
  Function TBRBuffer.HashCode:Longword;
  var
    i:        integer;
    x:        Longword;
    mTotal:   Int64;
    mRead:    Integer;
    mToRead:  Integer;
    mIndex:   Int64;
    mCache:   TBRIOCache;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}
    (* Initialize *)
    Result:=0;

    (* Check that buffer supports reading *)
    If (mcRead in FCaps) then
    Begin
      (* Get current datasize *)
      mTotal:=DoGetDataSize;

      (* anything to work with? *)
      If mTotal>0 then
      Begin
        (* start at the beginning *)
        mIndex:=0;

        (* keep going while we have data *)
        while mTotal>0 do
        Begin
          (* clip prefetch to cache range *)
          mToRead:=SizeOf(mCache);
          if mToRead>mTotal then
          mToRead:=mTotal;

          (* read a chunk of data *)
          mRead:=read(mIndex,mToRead,mCache);

          (* anything to work with? *)
          If mRead>0 then
          Begin
            (* go through the cache *)
            for i:=0 to mRead do
            begin
              Result := (Result shl 4) + mCache[i];
              x := Result and $F0000000;
              if (x <> 0) then
              Result := Result xor (x shr 24);
              Result := Result and (not x);
            end;

            (* update variables *)
            mTotal:=mTotal - mRead;
            mIndex:=mIndex + mRead;
          end else
          Break;
        end;

      end else
      Raise EBRBufferError.Create(CNT_ERR_BTRG_EMPTY);
    end else
    Raise EBRBufferError.Create(CNT_ERR_BTRG_READNOTSUPPORTED);
    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['HashCode',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  (*  Method:   LoadFromFile()
      Purpose:  Loads the content of a file into our buffer *)
  Procedure TBRBuffer.LoadFromFile(Filename:String);
  var
    mFile:  TFileStream;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}
    mFile:=TFileStream.Create(filename,fmOpenRead or fmShareDenyNone);
    try
      LoadFromStream(mFile);
    finally
      mFile.free;
    end;
    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['LoadFromFile',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  (*  Method:   SaveToFile()
      Purpose:  Saves the current content of the buffer to a file. *)
  procedure TBRBuffer.SaveToFile(Filename:String);
  var
    mFile:  TFileStream;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}
    mFile:=TFileStream.Create(filename,fmCreate or fmShareDenyNone);
    try
      SaveToStream(mFile);
    finally
      mFile.free;
    end;
    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['SaveToFile',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  (*  Method:   SaveToStream()
      Purpose:  Saves the current content of the buffer to a stream. *)
  Procedure TBRBuffer.SaveToStream(Stream:TStream);
  var
    mWriter:  TWriter;
    mTotal:   Int64;
    mToRead:  Integer;
    mRead:    Integer;
    mOffset:  Int64;
    mCache:   TBRIOCache;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}
    (* Check that buffer supports reading *)
    If (mcRead in FCaps) then
    Begin
      (* make sure targetstream is valid *)
      if (Stream<>NIL) then
      Begin
        (* create a TWriter to benefit from cache *)
        mWriter:=TWriter.Create(Stream,1024);
        try
          (* get current buffersize *)
          mTotal:=DoGetDataSize;
          mOffset:=0;

          (* Keep going while there is data *)
          While mTotal>0 do
          begin
            (* Clip prefetch size so not to exceed range *)
            mToRead:=SizeOf(mCache);
            If mToRead>mTotal then
            mToRead:=mTotal;

            (* attempt to read the spec. size *)
            mRead:=Read(mOffset,mToRead,mCache);
            If mRead>0 then
            Begin
              (* output data to our writer *)
              mWriter.Write(mCache,mRead);

              (* update variables *)
              mOffset:=mOffset + mRead;
              mTotal:=mTotal - mRead;
            end else
            Break;
          end;
        finally
          (* flush our stream cache to medium *)
          mWriter.FlushBuffer;

          (* release writer object *)
          mWriter.free;
        end;
      end else
      Raise EBRBufferError.Create(CNT_ERR_BTRG_INVALIDDATATARGET);
    end else
    Raise EBRBufferError.Create(CNT_ERR_BTRG_READNOTSUPPORTED);
    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['SaveToStream',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  (*  Method:   LoadFromStream()
      Purpose:  Loads the entire content of a stream into the current buffer.
      Comments: This method releases the current buffer and use Append()
                to insert data. Also, it takes height for the current
                position of the source stream - so make sure position is
                set to zero if you want to load the whole content. *)
  procedure TBRBuffer.LoadFromStream(Stream:TStream);
  var
    mReader:  TReader;
    mTotal:   Int64;
    mToRead:  Integer;
    mCache:   TBRIOCache;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}
    (* Check that buffer supports writing *)
    If (mcWrite in FCaps) then
    Begin
      (* Check that buffer supports scaling *)
      if (mcScale in FCaps) then
      Begin
        (* Validate source PTR *)
        if Stream<>NIL then
        Begin
          (* Release current buffer *)
          Release;

          (* create our reader object to benefit from cache *)
          mReader:=TReader.Create(Stream,1024);
          try
            mTotal:=(Stream.Size - Stream.Position);
            While mTotal>0 do
            begin
              (* Clip chunk to read *)
              mToRead:=SizeOf(mCache);
              If mToRead>mTotal then
              mToRead:=mTotal;

              (* Read data *)
              mReader.read(mCache,mToRead);

              (* Append data to current *)
              self.Append(mCache,mToRead);

              (* Update count *)
              mTotal:=mTotal - mToRead;
            end;
          finally
            mReader.free;
          end;
        end else
        Raise EBRBufferError.Create(CNT_ERR_BTRG_INVALIDDATASOURCE);
      end else
      Raise EBRBufferError.Create(CNT_ERR_BTRG_SCALENOTSUPPORTED);
    end else
    Raise EBRBufferError.Create(CNT_ERR_BTRG_WRITENOTSUPPORTED);
    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['LoadFromStream',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  (*  Method:   ExportTo()
      Purpose:  The ExportTo method allows you to read from the buffer,
                but output the data to an alternative target. In this case
                a TWriter class, which comes in handy when working
                with persistence.
      Comments: This method calls Write() to do the actual reading *)
  Function  TBRBuffer.ExportTo(aByteIndex:Int64;
            aLength:Integer;Const Writer:TWriter):Integer;
  var
    mToRead:  Integer;
    mRead:    Integer;
    mCache:   TBRIOCache;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}
    (* Initialize *)
    Result:=0;

    (* Check that buffer supports reading *)
    If (mcRead in FCaps) then
    Begin
      (* Check length of export *)
      If aLength>0 then
      Begin
        (* Validate writer PTR *)
        If Writer<>NIL then
        Begin
          (* Keep going while there is data *)
          While aLength>0 do
          Begin
            (* Clip prefetch to actual length *)
            mToRead:=Math.EnsureRange(SizeOf(mCache),0,aLength);

            (* read from our buffer *)
            mRead:=Read(aByteIndex,mToRead,mCache);

            (* Anything read? *)
            If mRead>0 then
            Begin
              (* output data to writer *)
              Writer.Write(mCache,mRead);

              (* update variables *)
              aByteIndex:=aByteIndex + mRead;
              aLength:=aLength - mRead;
              result:=result + mRead;
            end else
            Break;
          end;

          (* flush writer cache to medium, this is important *)
          Writer.FlushBuffer;
        end else
        Raise EBRBufferError.Create(CNT_ERR_BTRG_INVALIDDATATARGET);
      end else
      Raise EBRBufferError.Create(CNT_ERR_BTRG_EMPTY);
    end else
    Raise EBRBufferError.Create(CNT_ERR_BTRG_READNOTSUPPORTED);
    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['ExportTo',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  (*  Method:   Read()
      Purpose:  The read method allows you to read from the buffer into
                an untyped targetbuffer. *)
  Function  TBRBuffer.Read(Const aByteIndex:Int64;
            aLength:Integer;var aValue):Integer;
  var
    mTotal:   Int64;
    mRemains: Int64;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}
    (* Initialize *)
    Result:=0;

    (* Check that our buffer supports reading *)
    If (mcRead in FCaps) then
    Begin
      (* Check length of read *)
      If aLength>0 then
      Begin
        (* Get current size of buffer *)
        mTotal:=DoGetDataSize;

        (* anything to read from? *)
        If mTotal>0 then
        Begin
          (* make sure entry is within range *)
          If (aByteIndex>=0) and (aByteIndex<mTotal) then
          Begin
            (* Check that copy results in data move *)
            mRemains:=mTotal - aByteIndex;
            If mRemains>0 then
            Begin
              (* clip copylength to edge of buffer if we need to *)
              If aLength>mRemains then
              aLength:=mRemains;

              (* Read data into buffer *)
              DoReadData(aByteIndex,aValue,aLength);

              (* return bytes moved *)
              Result:=aLength;
            end;
          end;
        end else
        Raise EBRBufferError.Create(CNT_ERR_BTRG_EMPTY);
      end;
    end else
    Raise EBRBufferError.Create(CNT_ERR_BTRG_READNOTSUPPORTED);
    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['Read',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  (*  Method:   ImportFrom()
      Purpose:  The ImportFrom method allows you to write to the buffer,
                but using an alternative datasource. In this case a TReader
                class, which comes in handy when working with persistence.
      Comments: This method calls Write() to do the actual writing  *)
  Function  TBRBuffer.ImportFrom(aByteIndex:Int64;
            aLength:Integer;Const Reader:TReader):Integer;
  var
    mToRead:  Integer;
    mCache:   TBRIOCache;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}
    (* Initialize *)
    Result:=0;

    (* Check that buffer supports writing *)
    If (mcWrite in FCaps) then
    Begin
      (* Check that Reader PTR is valid *)
      If (Reader<>NIL) then
      Begin
        (* keep going until no more data *)
        While aLength>0 do
        Begin
          (* Clip prefetch to make sure we dont read to much *)
          mToRead:=Math.EnsureRange(SizeOf(mCache),0,aLength);

          (* Anything to read after clipping? *)
          if mToRead>0 then
          Begin
            (* read from source *)
            Reader.Read(mCache,mToRead);

            (* write to target *)
            Write(aByteIndex,mToRead,mCache);

            (* update variables *)
            result:=result + mToRead;
            aByteIndex:=aByteIndex + mToRead;
            aLength:=aLength - mToRead;
          end else
          Break;
        end;
      end else
      Raise EBRBufferError.Create(CNT_ERR_BTRG_INVALIDDATASOURCE);
    end else
    Raise EBRBufferError.Create(CNT_ERR_BTRG_WRITENOTSUPPORTED);
    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['ImportFrom',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  (*  Method:   Write()
      Purpose:  The write method allows you to write data into the buffer.
                It uses the internal mechanisms to do the actual writing,
                which means that how the data is written depends on the
                buffer implementation and medium.
      Comments: This method supports scaling and will automatically
                resize the buffer to fit the new data. If the byteindex is
                within range of the current buffer - the trailing data will
                be overwritten (same as a normal MOVE operation in memory). *)
  Function  TBRBuffer.Write(Const aByteIndex:Int64;
            aLength:Integer;Const aValue):Integer;
  var
    mTotal:   Int64;
    mEnding:  Int64;
    mExtra:   Int64;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}
    (* Initialize *)
    Result:=0;

    (* Check that buffer supports writing *)
    If (mcWrite in FCaps) then
    Begin
      (* Make sure there is data to write *)
      If aLength>0 then
      Begin
        (* Get current data size. If it's empty we will go for
           a straight append instead, see further down *)
        mTotal:=DoGetDataSize;
        If mTotal>0 then
        Begin
          (* offset within range of allocation? *)
          If (aByteIndex>=0) and (aByteIndex<mTotal) then
          Begin
            (* does this write exceed the current buffer size? *)
            mEnding:=aByteIndex + aLength;
            If mEnding>mTotal then
            Begin
              (* by how much? *)
              mExtra:=mEnding - mTotal;

              (* Check that we support scaling, grow if we can.
                 Otherwise just clip the data to the current buffer size *)
              If (mcScale in FCaps) then
              DoGrowDataBy(mExtra) else
              aLength:=math.EnsureRange(aLength - mExtra,0,MAXINT);
            end;

            (* Anything to work with? *)
            If aLength>0 then
            DoWriteData(aByteIndex,aValue,aLength);

            (* retun bytes written *)
            Result:=aLength;
          end else
          Raise EBRBufferError.CreateFmt
          (CNT_ERR_BTRG_BYTEINDEXVIOLATION,[0,mTotal-1,aByteIndex]);
        end else
        Begin
          (* Check that buffer supports scaling *)
          If (mcScale in FCaps) then
          Begin
            (* Grow the current buffer to new size *)
            DoGrowDataBy(aLength);

            (* write data *)
            DoWriteData(0,aValue,aLength);

            (* return bytes written *)
            Result:=aLength;
          end else
          Raise EBRBufferError.Create(CNT_ERR_BTRG_SCALENOTSUPPORTED);
        end;
      end;
    end else
    Raise EBRBufferError.Create(CNT_ERR_BTRG_WRITENOTSUPPORTED);
    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['Write',e.classname,e.message]);
    end;
    {$ENDIF}
  end;


  //##########################################################################
  // TBRStreamAdapter
  //##########################################################################

  Constructor TBRStreamAdapter.Create(Const aBuffer:TBRBuffer);
  Begin
    inherited Create;
    If aBuffer<>NIl then
    FBufObj:=aBuffer else
    Raise EBRStreamAdapter.Create(CNT_ERR_BTRGSTREAM_INVALIDBUFFER);
  end;

  function TBRStreamAdapter.GetSize:Int64;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}
    result:=FBufObj.Size;
    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['GetSize',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  procedure TBRStreamAdapter.SetSize(Const NewSize:Int64);
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}
    FBufObj.Size:=NewSize;
    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['SetSize',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  function TBRStreamAdapter.Read(var Buffer;Count:Longint):Longint;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}
    result:=FBufObj.Read(FOffset,Count,Buffer);
    inc(FOffset,Result);
    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['Read',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  function TBRStreamAdapter.Write(const Buffer;Count:Longint): Longint;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}
    If FOffset=FBufObj.Size then
    Begin
      FBufObj.Append(Buffer,Count);
      Result:=Count;
    end else
    result:=FBufObj.Write(FOffset,Count,Buffer);
    inc(FOffset,Result);
    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['Write',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  function TBRStreamAdapter.Seek(const Offset:Int64;
           Origin:TSeekOrigin):Int64;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}
    Case Origin of
    soBeginning:
      Begin
        if Offset>=0 then
        FOffset:=Math.EnsureRange(Offset,0,FBufObj.Size);
      end;
    soCurrent:
      Begin
        FOffset:=math.EnsureRange(FOffset + Offset,0,FBufObj.Size);
      end;
    soEnd:
      Begin
        If Offset>0 then
        FOffset:=FBufObj.Size-1 else
        FOffset:=math.EnsureRange(FOffset-(abs(Offset)),0,FBufObj.Size);
      end;
    end;
    result:=FOffset;
    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['Seek',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  end.
