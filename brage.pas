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
  abstract storage-medium with a unified set of methods. However a buffer
  is far more agile than normal delphi streams, supporting functions like
  Insert(), Remove() push() and poll(). Functions that are vital technologies
  that operate on low-level data.

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

  Version 1.1.1

  - [12.10.2011]  - Added support for LoadFromUrl
  - [25.10.2013]  - Added support for Reader and Writer classes
  - [26.10.2013]  - Added support for Bit-access and Part Access
  - [28.10.2013]  - Added support for Records and field management
  - [03.03.2014]  - Fixed ReadString problem (wrote int length, read word length)
                  - Added ZLib support
                  - Added CompressTo / DecompressFrom methods
                  - Added Compress() and Decompress() method
                  - Added TBRObject and TBRPersistent

  ########################################################################### *)

  {.$DEFINE BR_SUPPORT_INTERNET}
  {$DEFINE BR_SUPPORT_VARIANTS}
  {.$DEFINE BR_DEBUG}
  {$DEFINE BR_SUPPORT_ZLIB}

  interface

  uses
  System.Sysutils, System.classes,
  System.Math,
  System.Generics.Defaults,
  System.Generics.Collections,
  {$IFDEF BR_SUPPORT_ZLIB}
  System.Zlib,
  {$ENDIF}
  {$IFDEF BR_SUPPORT_VARIANTS}
  System.Variants,
  {$ENDIF}
  System.Contnrs
  ;

  const
  CNT_BR_KILOBYTE  = 1024;
  CNT_BR_MEGABYTE  = 1048576;
  CNT_BR_2MEGABYTE = CNT_BR_MEGABYTE * 2;
  CNT_BR_TAB       = #09;
  CNT_BR_CR        = #13;
  CNT_BR_LF        = #10;
  CNT_BR_CRLF      = CNT_BR_CR + CNT_BR_LF;
  CNT_BR_MAXSTRING = CNT_BR_MEGABYTE;

  CNT_BR_IOCACHE_SIZE     = CNT_BR_KILOBYTE * 64;

  CNT_BR_ASTRING_HEADER   = $DB07;
  CNT_BR_WSTRING_HEADER   = $DB09;
  CNT_BR_USTRING_HEADER   = $DB0A;
  CNT_BR_VARIANT_HEADER   = $DB0B;
  CNT_BR_COMPONT_HEADER   = $DBCA;
  CNT_RECORD_HEADER       = $ABBABABE;

  type

  (* Custom exceptions *)
  EBRBufferError      = Class(Exception);
  EBRStreamAdapter    = Class(Exception);
  EBRReader           = Class(Exception);
  EBRWriter           = Class(Exception);
  EBRRecordFieldError = Class(Exception);
  EBRRecordError      = Class(Exception);

  EBRObject           = Class(Exception);
  EBRPersistent       = Class(EBRObject);

  (* Forward declarations *)
  TBRBuffer           = Class;
  TBRReader           = Class;
  TBRWriter           = class;
  TBRBufferFile       = Class;
  TBRBufferMemory     = Class;
  TBRStreamAdapter    = Class;
  TBRBitAccess        = Class;
  TBRPartsAccess      = Class;
  TBRRecordField      = Class;

  TBRObject           = Class;
  TBRPersistent       = Class;

  TBRObjectClass      = class of TBRObject;

  TBRRecordFieldClass = Class of TBRRecordField;
  TBRRecordFieldArray = Array of TBRRecordFieldClass;

  TBRObjectState      = set of (
                        osCreating,
                        osDestroying,
                        osUpdating,
                        osReadWrite,
                        osSilent
                        );

  IBROwnedObject = Interface
  ['{286A5B65-D5F6-4008-8095-7978CE74C666}']
    Function  GetParent:TObject;
    Procedure SetParent(Const Value:TObject);
  End;

  IBRObject = Interface
    ['{FFF8E7BF-ADC9-4245-8717-E9A6C567DED9}']
    Procedure SetObjectState(Const Value:TBRObjectState);
    Procedure AddObjectState(Const Value:TBRObjectState);
    Procedure RemoveObjectState(Const Value:TBRObjectState);
    Function  GetObjectState:TBRObjectState;
    Function  QueryObjectState(Const Value:TBRObjectState):Boolean;
    Function  GetObjectClass:TBRObjectClass;
  End;

  IBRPersistent = Interface
    ['{282CC310-CD3B-47BF-8EB0-017C1EDF0BFC}']
    Procedure   ObjectFrom(Const Reader:TBRReader);
    Procedure   ObjectFromStream(Const Stream:TStream;
                Const Disposable:Boolean);
    Procedure   ObjectFromData(Const Data:TBRBuffer;
                Const Disposable:Boolean);
    Procedure   ObjectFromFile(Const Filename:String);

    Procedure   ObjectTo(Const Writer:TBRWriter);
    Function    ObjectToStream:TStream;overload;
    Procedure   ObjectToStream(Const Stream:TStream);overload;
    Function    ObjectToData:TBRBuffer;overload;
    Procedure   ObjectToData(Const Data:TBRBuffer);overload;
    Procedure   ObjectToFile(Const Filename:String);
  End;

  (* Generic progress events *)
  TBRProcessBeginsEvent = Procedure
    (Const Sender:TObject;Const Primary:Integer) of Object;

  TBRProcessUpdateEvent = Procedure
    (Const Sender:TObject;Const Value,Primary:Integer) of Object;

  TBRProcessEndsEvent = Procedure
    (Const Sender:TObject;Const Primary,Secondary:Integer) of Object;

  {$IFDEF BR_SUPPORT_ZLIB}
  TBRDeflateEvents = Record
    OnComprBegins:  TBRProcessBeginsEvent;
    OnComprUpdate:  TBRProcessUpdateEvent;
    OnComprEnds:    TBRProcessEndsEvent;
  End;

  TBRInflateEvents = Record
    OnComprBegins:  TBRProcessBeginsEvent;
    OnComprUpdate:  TBRProcessUpdateEvent;
    OnComprEnds:    TBRProcessEndsEvent;
  End;
  {$ENDIF}

  (* custom 3byte record for our FillTriple class procedure *)
  PBRTripleByte = ^TBRTripleByte;
  TBRTripleByte = Packed record
    a,b,c:Byte;
  End;


  TBRObject = Class(TPersistent,IBROwnedObject,IBRObject)
  Strict private
    FState:     TBRObjectState;
    FParent:    TObject;
  Strict protected
    (* Implements:: IBROwnedObject *)
    Function    GetParent:TObject;virtual;
    Property    Parent:TObject read GetParent;
    Procedure   SetParent(Const Value:TObject);
  Strict protected
    (* Implements:: IBRObject *)
    Procedure   SetObjectState(Const Value:TBRObjectState);
    Procedure   AddObjectState(Const Value:TBRObjectState);
    Procedure   RemoveObjectState(Const Value:TBRObjectState);
    Function    QueryObjectState(Const Value:TBRObjectState):Boolean;
    Function    GetObjectState:TBRObjectState;
    Function    GetObjectClass:TBRObjectClass;
  Protected
    (* implements:: IUnknown *)
    function    QueryInterface(const IID:TGUID;
                out Obj):HResult;virtual;stdcall;
    function    _AddRef: Integer;virtual;stdcall;
    function    _Release: Integer;virtual;stdcall;
  Public
    Class Function ObjectPath:String;
    Procedure   AfterConstruction;override;
    Procedure   BeforeDestruction;Override;
    Constructor Create;virtual;
  End;

  TBRPersistent = Class(TBRObject,IBRPersistent)
  Private
    FObjId:     Longword;
    FUpdCount:  Integer;
  strict protected
    (* Implements:: IBRPersistent *)
    Procedure   ObjectTo(Const Writer:TBRWriter);
    Procedure   ObjectFrom(Const Reader:TBRReader);
    Procedure   ObjectFromStream(Const Stream:TStream;
                Const Disposable:Boolean);
    Function    ObjectToStream:TStream;overload;
    Procedure   ObjectToStream(Const Stream:TStream);overload;
    Procedure   ObjectFromData(Const Binary:TBRBuffer;
                Const Disposable:Boolean);
    Function    ObjectToData:TBRBuffer;overload;
    Procedure   ObjectToData(Const Binary:TBRBuffer);overload;
    Procedure   ObjectFromFile(Const Filename:String);
    Procedure   ObjectToFile(Const Filename:String);
  Protected
    Procedure   BeforeUpdate;virtual;
    procedure   AfterUpdate;virtual;
  Protected
    (* Persistency Read/Write methods *)
    Procedure   BeforeReadObject;virtual;
    Procedure   AfterReadObject;Virtual;
    Procedure   BeforeWriteObject;Virtual;
    procedure   AfterWriteObject;Virtual;
    Procedure   WriteObject(Const Writer:TBRWriter);virtual;
    Procedure   ReadObject(Const Reader:TBRReader);virtual;
  protected
    (* Standard persistence *)
    function    ObjectHasData:Boolean;virtual;
    Procedure   ReadObjBin(Stream:TStream);virtual;
    procedure   WriteObjBin(Stream:TStream);virtual;
    procedure   DefineProperties(Filer: TFiler);override;
  Public
    Property    UpdateCount:Integer read FUpdCount;

    Procedure   Assign(Source:TPersistent);override;

    Function    ObjectIdentifier:Longword;

    Function    BeginUpdate:Boolean;
    procedure   EndUpdate;

    Class Function ClassIdentifier:Longword;

    Constructor Create;override;
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
  TBRIOCache = packed Array [1..CNT_BR_IOCACHE_SIZE] of Byte;


  (* This is the basic buffer class. It is an abstract class and should not be
     created. Create objects from classes that decend from this and that
     implements the buffering you need to work with (memory, stream, temp etc *)
  TBRBuffer = Class(TBRPersistent)
  private
    (* Buffer capabilities. I.E: Readable, writeable etc. *)
    FCaps:      TBRBufferCapabilities;

    {$IFDEF BR_SUPPORT_ZLIB}
    FZDEvents:  TBRDeflateEvents;
    FZIEvents:  TBRInflateEvents;
    {$ENDIF}

    Procedure   SetSize(Const aNewSize:Int64);
  Protected
    (*  Standard persistence. Please note that we call the function
        ObjectHasData() to determine if there is anything to save.
        See extended persistence below for more information.

        NOTE: Do not override these methods, use the functions defined
              in extended persistance when modifying this object *)
    //Procedure   ReadObjBin(Stream:TStream);
    //procedure   WriteObjBin(Stream:TStream);
    //procedure   DefineProperties(Filer: TFiler);override;
    Function    ObjectHasData:Boolean;override;
  Protected
    (*  Extended persistence.
        The function ObjectHasData() is called by the normal VCL
        DefineProperties to determine if there is any data to save.
        The other methods are invoked before and after either loading or
        saving object data.

        NOTE: To extend the functionality of this object please override
        ReadObject() and WriteObject(). The object will take care of
        everything else. *)
    Procedure   BeforeReadObject;override;
    (* Procedure   AfterReadObject;override;
    Procedure   BeforeWriteObject;override;
    Procedure   AfterWriteObject;override; *)
    Procedure   ReadObject(Const Reader:TBRReader);override;
    Procedure   WriteObject(Const Writer:TBRWriter);override;
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


    {$IFDEF BR_SUPPORT_ZLIB}
    Property    ZLibDeflateEvents:TBRDeflateEvents read FZDEvents write FZDEvents;
    Property    ZLibInflateEvents:TBRInflateEvents read FZIEvents write FZIEvents;
    {$ENDIF}

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
                aLength:Integer;Const Writer:TWriter):Integer;overload;

    Function    ExportTo(aByteIndex:Int64;
                aLength:Integer;Const Writer:TBRWriter):Integer;overload;

    (* Import data from various input sources *)
    Function    ImportFrom(aByteIndex:Int64;
                aLength:Integer;Const Reader:TReader):Integer;overload;

    function    ImportFrom(aByteIndex:Int64;
                aLength:Integer;Const Reader:TBRReader):Integer;overload;

    {$IFDEF BR_SUPPORT_ZLIB}
    Procedure   CompressTo(Const Target:TBRBuffer);overload;
    Procedure   DeCompressFrom(Const Source:TBRBuffer);overload;
    function    CompressTo:TBRBuffer;overload;
    function    DecompressTo:TBRBuffer;overload;
    Procedure   Compress;
    Procedure   Decompress;
    {$ENDIF}

    (* release the current content of the buffer *)
    Procedure   Release;
    Procedure   AfterConstruction;Override;
    Procedure   BeforeDestruction;Override;

    (* Generic ELF-HASH methods *)
    Class function ElfHash(const aData;aLength:Integer):LongWord;overload;
    class function ElfHash(aText:String):LongWord;overload;

    (* Generic memory fill methods *)
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
    property    Data:PByte read FDataPTR;
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

  (* Abstract reader class *)
  TBRReader = Class
  Private
    FOffset:    Int64;
    Function    DoReadAnsiString:AnsiString;
    Function    DoReadWideString:WideString;
    function    doReadString:String;
  protected
    Procedure   Advance(Const Value:Integer);
  Public
    Property    Position:Int64 read FOffset;
    Function    ReadNone(Length:Integer):Integer;
    Function    ReadPointer:Pointer;
    Function    ReadByte:Byte;
    Function    ReadBool:Boolean;
    Function    ReadWord:Word;
    Function    ReadSmall:Smallint;
    Function    ReadInt:Integer;
    Function    ReadLong:longword;
    Function    ReadInt64:Int64;
    Function    ReadCurrency:Currency;
    Function    ReadDouble:Double;
    Function    ReadShort:ShortInt;
    Function    ReadSingle:Single;
    Function    ReadDateTime:TDateTime;
    Function    ReadGUID:TGUID;

    Function    ReadToEOL:AnsiString;virtual;
    Function    ReadAsc(Const Terminator:AnsiString
                =CNT_BR_CRLF):AnsiString;overload;
    Function    ReadAsc(Const Length:Integer):AnsiString;overload;
    Function    ReadString:String;
    Function    ReadWideString:WideString;

    Function    CopyTo(Const Writer:TBRWriter;
                CopyLen:Integer):Integer;Overload;

    Function    CopyTo(Const Stream:TStream;
                Const CopyLen:Integer):Integer;overload;

    Function    CopyTo(Const Binary:TBRBuffer;
                Const CopyLen:Integer):Integer;overload;

    Function    ReadStream:TStream;
    Function    ReadData:TBRBuffer;
    Function    ContentToStream:TStream;
    Function    ContentToData:TBRBuffer;

    {$IFDEF BR_SUPPORT_VARIANTS}
    Function    ReadVariant:Variant;
    {$ENDIF}
    Procedure   Reset;Virtual;

    Function    Read(var Data;DataLen:Integer):Integer;virtual;abstract;
  End;

  (* Abstract writer class *)
  TBRWriter = Class
  Private
    FOffset:    Int64;
  protected
    Procedure   Advance(Const Value:Integer);
  Public
    Procedure   WritePointer(Const Value:Pointer);
    {$IFDEF BR_SUPPORT_VARIANTS}
    Procedure   WriteVariant(Const Value:Variant);
    {$ENDIF}
    Procedure   WriteCRLF(Const Times:Integer=1);
    Procedure   WriteByte(Const Value:Byte);
    Procedure   WriteBool(Const Value:Boolean);
    Procedure   WriteWord(Const Value:Word);
    Procedure   WriteShort(Const Value:Shortint);
    Procedure   WriteSmall(Const Value:SmallInt);
    procedure   WriteInt(Const Value:Integer);
    Procedure   WriteLong(Const Value:Longword);
    procedure   WriteInt64(Const Value:Int64);
    Procedure   WriteCurrency(Const Value:Currency);
    procedure   WriteDouble(Const Value:Double);
    Procedure   WriteSingle(Const Value:Single);
    procedure   WriteDateTime(Const Value:TDateTime);
    Procedure   WriteString(Const Value:String);
    Procedure   WriteWideString(Const Value:WideString);
    Procedure   WriteAsc(Value:AnsiString;
                Const Delimiter:AnsiString=CNT_BR_CRLF);
    Procedure   WriteStreamContent(Const Value:TStream;
                Const Disposable:Boolean=False);
    Procedure   WriteDataContent(Const Value:TBRBuffer;
                Const Disposable:Boolean=False);
    procedure   WriteStream(Const Value:TStream;
                Const Disposable:Boolean);
    Procedure   WriteData(Const Value:TBRBuffer;
                Const Disposable:Boolean);
    Procedure   WriteGUID(Const Value:TGUID);
    Procedure   WriteFile(Const Filename:String);
    Function    CopyFrom(Const Reader:TBRReader;
                DataLen:Int64):Int64;overload;
    Function    CopyFrom(Const Stream:TStream;
                Const DataLen:Int64):Int64;overload;
    Function    CopyFrom(Const Data:TBRBuffer;
                Const DataLen:Int64):Int64;overload;
    Function    Write(Const Data;DataLen:Integer):Integer;virtual;abstract;
    Property    Position:Int64 read FOffset;
    Procedure   Reset;Virtual;
  End;

  (* Writer class for buffers *)
  TBRWriterBuffer = Class(TBRWriter)
  Private
    FData:      TBRBuffer;
  Public
    Property    Data:TBRBuffer read FData;
    Function    Write(Const Data;DataLen:Integer):Integer;override;
    Constructor Create(Const Target:TBRBuffer);reintroduce;
  End;

  (* Reader class for buffers *)
  TBRReaderBuffer = Class(TBRReader)
  Private
    FData:      TBRBuffer;
  Public
    Function    Read(var Data;DataLen:Integer):Integer;override;
    Constructor Create(Const Source:TBRBuffer);reintroduce;
  End;

  (* writer class for stream *)
  TBRWriterStream = Class(TBRWriter)
  Private
    FStream:    TStream;
  Public
    Property    DataStream:TStream read FStream;
    Function    Write(Const Data;DataLen:Integer):Integer;override;
    Constructor Create(Const Target:TStream);reintroduce;
  End;

  TBRWriterMemory = Class(TBRWriter)
  Private
    FMemory:    PByte;
    FSize:      Integer;
    FOffset:    Integer;
  public
    Property    Memory:PByte read FMemory;
    Property    MemorySize:Integer read FSize;
    Function    Write(Const Data;DataLen:Integer):Integer;override;
    Constructor Create(Const aMemory:PByte;aDataLen:Integer);reintroduce;
  End;

  (* reader class for stream *)
  TBRReaderStream = Class(TBRReader)
  Private
    FStream:    TStream;
  Public
    Function    Read(var Data;DataLen:Integer):Integer;override;
    Constructor Create(Const Source:TStream);reintroduce;
  End;

  (* Bit-level access to a buffer *)
  TBRBitAccess = class(TObject)
  private
    FBuffer:    TBRBuffer;
    function    readBit(const aIndex:NativeInt):Boolean;
    procedure   writeBit(const aIndex:NativeInt;
                const aValue:Boolean);
    function    getCount:NativeUInt;
    function    getEmpty:Boolean;
  public
    Property    Buffer:TBRBuffer read FBuffer;
    Property    Bits[const aIndex:NativeInt]:Boolean
                read readBit write writeBit;
    Property    Count:NativeUInt read getCount;
    Property    Empty:Boolean read getEmpty;
    function    FindIdleBit(const aFromIndex:NativeUInt;
                out aIdleBitIndex:NativeUInt):Boolean;
    Procedure   SetBitRange(First,Last:NativeUInt;
                Const Bitvalue:Boolean);
    function    asString(const aLineLen:Integer=32):String;
    constructor Create(Const aBuffer:TBRBuffer);virtual;
  End;

  (* fixed-part access to a buffer *)
  TBRPartsAccess = class(TObject)
  private
    FBuffer:    TBRBuffer;
    FheadSize:  Integer;
    FPartSize:  Integer;
  protected
    function    getPartCount:Integer;
    function    getOffsetForPart(const aPartIndex:Integer):Int64;
  public
    Property    Buffer:TBRBuffer read FBuffer;
    Property    ReservedHeaderSize:Integer read FheadSize;
    Property    PartSize:Integer read FPartSize;

    property    Count:Integer read getPartCount;

    Procedure   ReadPart(const aIndex:Integer;
                var aData);overload;

    procedure   ReadPart(const aIndex:Integer;
                const aBuffer:TBRBuffer);overload;

    procedure   WritePart(const aIndex:Integer;
                const aData; Const aLength:Integer);overload;

    procedure   WritePart(Const aIndex:Integer;
                const aData:TBRBuffer);overload;

    procedure   AppendPart(const aData;aLength:Integer);overload;
    procedure   AppendPart(const aData:TBRBuffer);overload;

    function    CalcPartsForData(const aDataSize:Int64):NativeInt;

    Constructor Create(Const aBuffer:TBRBuffer;
                const aReservedHeaderSize:Integer;
                const aPartSize:Integer);virtual;
  End;

  TBRRecordField = Class(TBRBufferMemory)
  Private
    FName:      String;
    FNameHash:  Int64;
    Procedure   SetName(Value:String);
  Protected
    Function    GetDisplayName:String;virtual;
    Procedure   BeforeReadObject;override;
    Procedure   ReadObject(const Reader:TBRReader);override;
    Procedure   WriteObject(const Writer:TBRWriter);override;
    Procedure   DoReleaseData;override;
  Protected
    Procedure   SignalWrite;
    Procedure   SignalRead;
    procedure   SignalRelease;
  Public
    function    asString:String;virtual;abstract;
    Property    DisplayName:String read GetDisplayName;
    Property    FieldSignature:Int64 read FNameHash;
    Property    FieldName:String read FName write SetName;
  End;

  TBRFieldBoolean = Class(TBRRecordField)
  Private
    Function    GetValue:Boolean;
    Procedure   SetValue(Const NewValue:Boolean);
  Protected
    Function    GetDisplayName:String;Override;
  Public
    Property    Value:Boolean read GetValue write SetValue;
    Function    asString:String;override;
  End;

  TBRFieldByte = Class(TBRRecordField)
  Private
    Function    GetValue:Byte;
    Procedure   SetValue(Const NewValue:Byte);
  Protected
    Function    GetDisplayName:String;Override;
  Public
    Property    Value:Byte read GetValue write SetValue;
    Function    asString:String;override;
  End;

  TBRFieldCurrency = Class(TBRRecordField)
  Private
    Function    GetValue:Currency;
    Procedure   SetValue(Const NewValue:Currency);
  Protected
    Function    GetDisplayName:String;Override;
  Public
    Property    Value:Currency read GetValue write SetValue;
    Function    asString:String;override;
  End;

  TBRFieldData = Class(TBRRecordField)
  Protected
    Function    GetDisplayName:String;Override;
  Public
    Function    asString:String;override;
  End;

  TBRFieldDateTime = Class(TBRRecordField)
  Private
    Function    GetValue:TDateTime;
    Procedure   SetValue(Const NewValue:TDateTime);
  Protected
    Function    GetDisplayName:String;Override;
  Public
    Property    Value:TDateTime read GetValue write SetValue;
    Function    asString:String;override;
  End;

  TBRFieldDouble = Class(TBRRecordField)
  Private
    Function    GetValue:Double;
    Procedure   SetValue(Const NewValue:Double);
  Protected
    Function    GetDisplayName:String;Override;
  Public
    Property    Value:Double read GetValue write SetValue;
    Function    asString:String;override;
  End;

  TBRFieldGUID = Class(TBRRecordField)
  Private
    Function    GetValue:TGUID;
    Procedure   SetValue(Const NewValue:TGUID);
  Protected
    Function    GetDisplayName:String;Override;
  Public
    Property    Value:TGUID read GetValue write SetValue;
    Function    asString:String;override;
  End;

  TBRFieldInteger = Class(TBRRecordField)
  Private
    Function    GetValue:Integer;
    Procedure   SetValue(Const NewValue:Integer);
  Protected
    Function    GetDisplayName:String;Override;
  Public
    Property    Value:Integer read GetValue write SetValue;
    Function    asString:String;override;
  End;

  TBRFieldInt64 = Class(TBRRecordField)
  Private
    Function    GetValue:Int64;
    Procedure   SetValue(Const NewValue:Int64);
  Protected
    Function    GetDisplayName:String;Override;
  Public
    Property    Value:Int64 read GetValue write SetValue;
    Function    asString:String;override;
  End;

  TBRFieldString = Class(TBRRecordField)
  Private
    FLength:    Integer;
    FExplicit:  Boolean;
    Function    GetValue:String;
    Procedure   SetValue(NewValue:String);
    Procedure   SetFieldLength(Value:Integer);
  Protected
    Function    GetDisplayName:String;Override;
  Public
    Property    Value:String read GetValue write SetValue;
    Property    Length:Integer read FLength write SetFieldLength;
    Property    Explicit:Boolean read FExplicit write FExplicit;
    Function    asString:String;override;
    Constructor Create;override;
  End;

  TBRFieldLong = Class(TBRRecordField)
  Private
    Function    GetValue:Longword;
    Procedure   SetValue(Const NewValue:Longword);
  Protected
    Function    GetDisplayName:String;Override;
  Public
    Property    Value:Longword read GetValue write SetValue;
    Function    asString:String;override;
  End;


  TBRCustomRecord = Class(TBRPersistent)
  Private
    FObjects:   TObjectList;
    Function    GetCount:Integer;
    Function    GetItem(const Index:Integer):TBRRecordField;
    Procedure   SetItem(const Index:Integer;
                const Value:TBRRecordField);
    Function    GetField(const AName:String):TBRRecordField;
    Procedure   SetField(const AName:String;
                const Value:TBRRecordField);
  Protected
    Property    Fields[const aName:String]:TBRRecordField
                read GetField write SetField;
    Property    Items[const index:Integer]:TBRRecordField
                read GetItem write SetItem;
    Property    Count:Integer read GetCount;
  protected
    Procedure   BeforeReadObject;Override;
    Procedure   WriteObject(Const Writer:TBRWriter);Override;
    Procedure   ReadObject(Const Reader:TBRReader);Override;
  Public
    Function    Add(const aName:String;
                Const aFieldClass:TBRRecordFieldClass):TBRRecordField;
    Function    AddInteger(const aName:String):TBRFieldInteger;
    Function    AddStr(const aName:String):TBRFieldString;
    Function    AddByte(const aName:String):TBRFieldByte;
    Function    AddBool(const aName:String):TBRFieldBoolean;
    Function    AddCurrency(const aName:String):TBRFieldCurrency;
    Function    AddData(const aName:String):TBRFieldData;
    Function    AddDateTime(const aName:String):TBRFieldDateTime;
    Function    AddDouble(const aName:String):TBRFieldDouble;
    Function    AddGUID(const aName:String):TBRFieldGUID;
    Function    AddInt64(const aName:String):TBRFieldInt64;
    Function    AddLong(const aName:String):TBRFieldLong;

    Procedure   WriteInt(const aName:String;const Value:Integer);
    procedure   WriteStr(const aName:String;const Value:String);
    Procedure   WriteByte(const aName:String;const Value:Byte);
    procedure   WriteBool(const aName:String;const Value:Boolean);
    procedure   WriteCurrency(const aName:String;const Value:Currency);
    procedure   WriteData(const aName:String;const Value:TStream);
    procedure   WriteDateTime(const aName:String;const Value:TDateTime);
    procedure   WriteDouble(const aName:String;const Value:Double);
    Procedure   WriteGUID(const aName:String;const Value:TGUID);
    Procedure   WriteInt64(const aName:String;const Value:Int64);
    Procedure   WriteLong(const aName:String;const Value:Longword);

    Procedure   Clear;virtual;

    //procedure   Assign(source:TPersistent);override;
    //function    toStream:TStream;virtual;
    //function    toBuffer:TBRBuffer;virtual;

    //Procedure   SaveToStream(Const stream:TStream);virtual;
    //procedure   LoadFromStream(Const stream:TStream);virtual;

    Function    IndexOf(const aName:String):Integer;
    Function    ObjectOf(const aName:String):TBRRecordField;
    Constructor Create;virtual;
    Destructor  Destroy;Override;
  End;

  TBRRecord = Class(TBRCustomRecord)
  Public
    Property  Fields;
    property  Items;
    Property  Count;
  End;


Procedure BRRegisterRecordField(AClass:TBRRecordFieldClass);

Function  BRRecordFieldKnown(AClass:TBRRecordFieldClass):Boolean;

Function  BRRecordFieldClassFromName(aName:String;
          out AClass:TBRRecordFieldClass):Boolean;

Function  BRRecordInstanceFromName(aName:String;
          out Value:TBRRecordField):Boolean;


function BRStrToGUID(const Value:AnsiString):TGUID;
Function BRGUIDToStr(const GUID:TGUID):AnsiString;



implementation

  {$IFDEF BR_SUPPORT_INTERNET}
  {$IFDEF MSWINDOWS}
  uses wintypes, comObj, ActiveX, UrlMon;
  {$ELSE}
  uses 'Include your networking library unit here';
  {$ENDIF}
  {$ENDIF}

  const

  //###########################################################################
  // Persistancy errors
  //###########################################################################

  ERR_BR_PERSISTENCY_ASSIGNCONFLICT
  = '%s can not be assigned to %s ';

  ERR_BR_PERSISTENCY_INVALIDSIGNATURE
  = 'Invalid signature, found %s, expected %s';

  ERR_BR_PERSISTENCY_INVALIDREADER
  = 'Invalid reader object error';

  ERR_BR_PERSISTENCY_INVALIDWRITER
  = 'Invalid writer object error';

  //###########################################################################
  // Record management errors
  //###########################################################################

  ERR_RECORDFIELD_INVALIDNAME =
  'Invalid field name [%s] error';

  ERR_RECORDFIELD_FailedSet =
  'Writing to field buffer [%s] failed error';

  ERR_RECORDFIELD_FailedGet =
  'Reading from field buffer [%s] failed error';

  ERR_RECORDFIELD_FieldIsEmpty
  = 'Record field is empty [%s] error';

  //###########################################################################
  // Byterage errors
  //###########################################################################

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


  //###########################################################################
  // Error messages for reader decendants
  //###########################################################################

  ERR_BR_READER_INVALIDSOURCE      = 'Invalid source medium error';
  ERR_BR_READER_FAILEDREAD         = 'Read failed on source medium';
  ERR_BR_READER_INVALIDDATASOURCE  = 'Invalid data source for read operation';
  ERR_BR_READER_INVALIDOBJECT      = 'Invalid object for read operation';
  ERR_BR_READER_INVALIDHEADER      = 'Invalid header, expected %d not %d';

  (* Error messages for TBRWriter decendants *)
  ERR_BR_WRITER_INVALIDTARGET       = 'Invalid target medium error';
  ERR_BR_WRITER_FAILEDWRITE         = 'Write failed on target medium';
  ERR_BR_WRITER_INVALIDDATASOURCE   = 'Invalid data source for write operation';
  ERR_BR_WRITER_INVALIDOBJECT       = 'Invalid object for write operation';

  CNT_PARTACCESS_BUFFERISNIL = 'Buffer cannot be NIL error';
  CNT_PARTACCESS_PARTSIZEINVALID  = 'Invalid partsize error';
  CNT_PARTACCESS_TARGETBUFFERINVALID  = 'Invalid target buffer error';


Var
_FieldClasses:  TBRRecordFieldArray;


Function BRGUIDToStr(const GUID:TGUID):AnsiString;
begin
  SetLength(Result, 38);
  StrLFmt(@Result[1],38,'{%.8x-%.4x-%.4x-%.2x%.2x-%.2x%.2x%.2x%.2x%.2x%.2x}',
  [GUID.D1, GUID.D2, GUID.D3, GUID.D4[0], GUID.D4[1], GUID.D4[2], GUID.D4[3],
  GUID.D4[4], GUID.D4[5], GUID.D4[6], GUID.D4[7]]);
end;

function BRStrToGUID(const Value:AnsiString):TGUID;
const
  ERR_InvalidGUID = '[%s] is not a valid GUID value';
var
  i:  Integer;
  src, dest: PAnsiChar;

  function _HexChar(Const C: AnsiChar): Byte;
  begin
    case C of
      '0'..'9': Result := Byte(c) - Byte('0');
      'a'..'f': Result := (Byte(c) - Byte('a')) + 10;
      'A'..'F': Result := (Byte(c) - Byte('A')) + 10;
    else        Raise Exception.CreateFmt(ERR_InvalidGUID,[Value]);
    end;
  end;

  function _HexByte(Const P:PAnsiChar): AnsiChar;
  begin
    Result:=AnsiChar((_HexChar(p[0]) shl 4)+_HexChar(p[1]));
  end;

begin
  If Length(Value)=38 then
  Begin
    dest := @Result;
    src := PAnsiChar(Value);
    Inc(src);

    for i := 0 to 3 do
    dest[i] := _HexByte(src+(3-i)*2);

    Inc(src, 8);
    Inc(dest, 4);
    if src[0] <> '-' then
    Raise Exception.CreateFmt(ERR_InvalidGUID,[Value]);

    Inc(src);
    for i := 0 to 1 do
    begin
      dest^ := _HexByte(src+2);
      Inc(dest);
      dest^ := _HexByte(src);
      Inc(dest);
      Inc(src, 4);
      if src[0] <> '-' then
      Raise Exception.CreateFmt(ERR_InvalidGUID,[Value]);
      inc(src);
    end;

    dest^ := _HexByte(src);
    Inc(dest);
    Inc(src, 2);
    dest^ := _HexByte(src);
    Inc(dest);
    Inc(src, 2);
    if src[0] <> '-' then
    Raise Exception.CreateFmt(ERR_InvalidGUID,[Value]);

    Inc(src);
    for i := 0 to 5 do
    begin
      dest^:=_HexByte(src);
      Inc(dest);
      Inc(src, 2);
    end;
  end else
  Raise Exception.CreateFmt(ERR_InvalidGUID,[Value]);
end;

Procedure BRRegisterRecordField(AClass:TBRRecordFieldClass);
var
  FLen: Integer;
Begin
  if (AClass<>NIL)
  and (BRRecordFieldKnown(AClass)=False) then
  Begin
    FLen:=Length(_FieldClasses);
    Setlength(_FieldClasses,FLen+1);
    _FieldClasses[FLen]:=AClass;
  end;
end;

Function BRRecordFieldKnown(AClass:TBRRecordFieldClass):Boolean;
var
  x:  Integer;
Begin
  result:=AClass<>NIl;
  if result then
  Begin
    result:=Length(_FieldClasses)>0;
    If result then
    begin
      result:=False;
      for x:=low(_FieldClasses) to high(_FieldClasses) do
      Begin
        result:=_FieldClasses[x]=AClass;
        if result then
        break;
      end;
    end;
  End;
end;

Function BRRecordFieldClassFromName(aName:String;
          out AClass:TBRRecordFieldClass):Boolean;
var
  x:  Integer;
Begin
  AClass:=NIL;
  result:=Length(_FieldClasses)>0;
  If result then
  begin
    result:=False;
    for x:=low(_FieldClasses) to high(_FieldClasses) do
    Begin
      result:=_FieldClasses[x].ClassName=aName;
      If result then
      Begin
        AClass:=_FieldClasses[x];
        break;
      end;
    end;
  end;
end;

Function  BRRecordInstanceFromName(aName:String;
          out Value:TBRRecordField):Boolean;
var
  FClass: TBRRecordFieldClass;
Begin
  result:=BRRecordFieldClassFromName(aName,FClass);
  if result then
  Value:=FClass.Create;
end;

  { Function JL_RightStr(Const Value:String;Count:Integer):String;
  var
    FLen: Integer;
  Begin
    FLen:=Length(Value);
    If (FLen>0) and (Count>0) then
    Begin
      If Count>FLen then
      Count:=FLen;
      Result:=Copy(Value,(FLen-Count)+1,Count);
    end else
    result:='';
  end;           }

  Procedure JL_FillWord(dstAddr:PWord;Const inCount:Integer;Const Value:Word);
  var
    FTemp:  Longword;
    FLongs: Integer;
  Begin
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
  end;

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
      mNewSize:=EnsureRange(FDataLEN - Value,0,FDataLen);
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

  class function TBRBuffer.ElfHash(const aData;aLength:Integer):LongWord;
  var
    i:    Integer;
    x:    Cardinal;
    FSrc: PByte;
  Begin
    Result:=0;
    If aLength>0 then
    Begin
      FSrc:=@aData;
      for i:=1 to aLength do
      begin
        Result := (Result shl 4) + FSrc^;
        x := Result and $F0000000;
        if (x <> 0) then
        Result := Result xor (x shr 24);
        Result := Result and (not x);
        inc(FSrc);
      end;
    end;
  end;

  class function TBRBuffer.ElfHash(aText:String):LongWord;
  var
    FAddr:  Pointer;
    FLen:   Integer;
  Begin
    Result:=0;
    FLen:=Length(aText);
    If FLen>0 then
    Begin
      FAddr:=@aText[1];
      Result:=ElfHash(FAddr^,FLen * Sizeof(Char));
    end;
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

  (* Procedure TBRBuffer.AfterReadObject;
  Begin
  end;

  Procedure TBRBuffer.BeforeWriteObject;
  Begin
  end;

  Procedure TBRBuffer.AfterWriteObject;
  Begin
  end;      *)

  Procedure TBRBuffer.ReadObject(const Reader:TBRReader);
  var
    mTotal: Int64;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}

    inherited;

    mTotal:=Reader.ReadInt64;
    If mTotal>0 then
    Begin
      ImportFrom(0,mTotal,Reader);
    end;

    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['ReadObject',e.classname,e.message]);
    end;
    {$ENDIF}
  end;

  Procedure TBRBuffer.WriteObject(const Writer:TBRWriter);
  var
    mSize:  Int64;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}

    inherited;

    mSize:=Size;
    Writer.WriteInt64(mSize);
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

  (* Procedure TBRBuffer.ReadObjBin(Stream:TStream);
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
  end;  *)

  (* procedure TBRBuffer.WriteObjBin(Stream:TStream);
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
  end; *)

  (* procedure TBRBuffer.DefineProperties(Filer:TFiler);
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
  end;   *)

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
      mToWrite:=EnsureRange(Datalen,1,FillLength);
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
              mBytesToRead:=EnsureRange(SizeOf(mCache),0,mTotal);
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
              mBytesToRead:=EnsureRange(SizeOf(mCache),0,mTotal);
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
                    mBytesToRead:=EnsureRange
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
                    mToRead:=EnsureRange(SizeOf(mCache),0,mToPoll);

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

  {$IFDEF BR_SUPPORT_ZLIB}
  function TBRBuffer.CompressTo:TBRBuffer;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}
    result:=TBRBufferMemory.Create;
    self.CompressTo(result);
    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['CompressTo',e.classname,e.message]);
    end;
    {$ENDIF}
  end;
  {$ENDIF}

  {$IFDEF BR_SUPPORT_ZLIB}
  Procedure TBRBuffer.Compress;
  var
    mTemp:  TBRBuffer;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}
    mTemp:=CompressTo;
    try
      self.Release;
      self.Append(mTemp);
    finally
      mTemp.Free;
    end;
    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['Compress',e.classname,e.message]);
    end;
    {$ENDIF}
  end;
  {$ENDIF}

  {$IFDEF BR_SUPPORT_ZLIB}
  Procedure TBRBuffer.Decompress;
  var
    mTemp:  TBRBuffer;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}
    mTemp:=self.DecompressTo;
    try
      self.Release;
      self.Append(mTemp);
    finally
      mTemp.Free;
    end;
    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['Decompress',e.classname,e.message]);
    end;
    {$ENDIF}
  end;
  {$ENDIF}

  {$IFDEF BR_SUPPORT_ZLIB}
  function TBRBuffer.DecompressTo:TBRBuffer;
  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}
    result:=TBRBufferMemory.Create;
    result.DeCompressFrom(self);
    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['DecompressTo',e.classname,e.message]);
    end;
    {$ENDIF}
  end;
  {$ENDIF}

  {$IFDEF BR_SUPPORT_ZLIB}
  Procedure TBRBuffer.DeCompressFrom(Const Source:TBRBuffer);
  var
    FZRec:      TZStreamRec;
    FInput:     Packed array [word] of Byte;
    FOutput:    Packed array [word] of Byte;
    FReader:    TBRReaderBuffer;
    FWriter:    TBRWriterBuffer;
    FBytes:     Integer;

    Procedure CCheck(Const Code:Integer);
    begin
      if Code<0 then
      Begin
        Case Code of
        Z_STREAM_ERROR:
          Raise EBRBufferError.CreateFmt('ZLib stream error #%d',[code]);
        Z_DATA_ERROR:
          Raise EBRBufferError.CreateFmt('ZLib data error #%d',[code]);
        Z_BUF_ERROR:
          Raise EBRBufferError.CreateFmt('ZLib buffer error #%d',[code]);
        Z_VERSION_ERROR:
          Raise EBRBufferError.CreateFmt('ZLib version conflict [#%d]',[code]);
        else
          Raise EBRBufferError.CreateFmt('Unspecified ZLib error #%d',[Code]);
        end;
      end;
    end;

  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}

    (* Populate ZLIB header *)
    Fillchar(FZRec,SizeOf(FZRec),0);
    FZRec.zalloc:=zlibAllocMem;
    FZRec.zfree:=zlibFreeMem;

    FzRec.next_in:=Addr(FInput);
    FzRec.next_out:=Addr(FOutput);

    (* release current content if any *)
    If not Empty then
    Release;

    (* initialize ZLIB compression *)
    CCheck(inflateInit_(FZRec,zlib_version,sizeof(FZRec)));
    try
      FReader:=TBRReaderBuffer.Create(Source);
      try
        FWriter:=TBRWriterBuffer.Create(self);
        try

          (* Signal Uncompress Begins *)
          if assigned(FZIEvents.OnComprBegins) then
          FZIEvents.OnComprBegins(self,size);

          Repeat
            (* Get more input *)
            If FzRec.avail_in=0 then
            Begin
              FzRec.avail_in:=FReader.Read(FInput,SizeOf(FInput));
              If FzRec.avail_in>0 then
              FzRec.next_in:=Addr(FInput) else
              Break;
            end;

            (* decompress input *)
            Repeat
              FzRec.next_out:=Addr(FOutput);
              FzRec.avail_out:=SizeOf(FOutput);
              CCheck(inflate(FZRec,Z_NO_FLUSH));
              FBytes:=SizeOf(FOutput) - FzRec.avail_out;
              if FBytes>0 then
              Begin
                FWriter.Write(FOutput,FBytes);
                FzRec.next_out:=Addr(FOutput);
                FzRec.avail_out:=SizeOf(FOutput);
              end;
            Until FzRec.avail_in=0;

            (* Signal Inflate progress *)
            if assigned(FZIEvents.OnComprUpdate) then
            FZIEvents.OnComprUpdate(self,FReader.Position,Size);
          Until False;

          (* Signal Compression Ends event *)
          if assigned(FZIEvents.OnComprEnds) then
          FZIEvents.OnComprEnds(self,FReader.Position,FWriter.Position);
        finally
          FWriter.free;
        end;
      finally
        FReader.free;
      end;
    finally
      (* end Zlib compression *)
      inflateEnd(FZRec);
    end;
    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['DeCompressFrom',e.classname,e.message]);
    end;
    {$ENDIF}
  end;
  {$ENDIF}

  {$IFDEF BR_SUPPORT_ZLIB}
  Procedure TBRBuffer.CompressTo(Const Target:TBRBuffer);
  var
    FZRec:      TZStreamRec;
    FInput:     Packed array [Word] of Byte;
    FOutput:    Packed array [Word] of Byte;
    FReader:    TBRReaderBuffer;
    FWriter:    TBRWriterBuffer;
    FMode:      Integer;
    FBytes:     Integer;

    Procedure CCheck(Const Code:Integer);
    begin
      if Code<0 then
      Begin
        Case Code of
        Z_STREAM_ERROR:
          Raise EBRBufferError.CreateFmt('ZLib stream error #%d',[code]);
        Z_DATA_ERROR:
          Raise EBRBufferError.CreateFmt('ZLib data error #%d',[code]);
        Z_BUF_ERROR:
          Raise EBRBufferError.CreateFmt('ZLib buffer error #%d',[code]);
        Z_VERSION_ERROR:
          Raise EBRBufferError.CreateFmt('ZLib version conflict [#%d]',[code]);
        else
          Raise EBRBufferError.CreateFmt('Unspecified ZLib error #%d',[Code]);
        end;
      end;
    end;

  Begin
    {$IFDEF BR_DEBUG}
    try
    {$ENDIF}
    (* Populate ZLIB header *)
    Fillchar(FZRec,SizeOf(FZRec),0);
    FZRec.zalloc:=zlibAllocMem;
    FZRec.zfree:=zlibFreeMem;
    FzRec.next_in:=Addr(FInput);
    FZRec.next_out := Addr(FOutput);
    FZRec.avail_out := sizeof(FOutput);

    (* initialize ZLIB compression *)
    CCheck(deflateInit_(FZRec,Z_BEST_COMPRESSION,
    zlib_version,sizeof(FZRec)));

    try
      FReader:=TBRReaderBuffer.Create(self);
      try
        FWriter:=TBRWriterBuffer.Create(target);
        try

          FMode:=Z_NO_Flush;

          (* Signal Compression Begins *)
          if assigned(FZDEvents.OnComprBegins) then
          FZDEvents.OnComprBegins(self,Size);

          Repeat
            (* more data required? If not, finish *)
            If FzRec.avail_in=0 then
            Begin
              If FReader.Position<Size then
              Begin
                FzRec.avail_in:=FReader.Read(FInput,SizeOf(FInput));
                FzRec.next_in:=@FInput
              end else
              FMode:=Z_Finish;
            end;

            (* Continue compression operation *)
            CCheck(deflate(FZRec,FMode));

            (* Write compressed data if any.. *)
            FBytes:=SizeOf(FOutput) - FzRec.avail_out;
            if FBytes>0 then
            Begin
              FWriter.Write(FOutput,FBytes);
              FzRec.next_out:=@FOutput;
              FzRec.avail_out:=SizeOf(FOutput);
            end;

            (* Signal Compression Progress Event *)
            if assigned(FZDEvents.OnComprUpdate) then
            FZDEvents.OnComprUpdate(self,FReader.Position,Size);

          Until (FBytes=0) and (FMode=Z_Finish);

          (* Signal Compression Ends event *)
          if assigned(FZDEvents.OnComprEnds) then
          FZDEvents.OnComprEnds(self,FReader.Position,Size);

        finally
          FWriter.free;
        end;
      finally
        FReader.free;
      end;
    finally
      (* end Zlib compression *)
      deflateEnd(FZRec);
    end;
    {$IFDEF BR_DEBUG}
    except
      on e: exception do
      Raise EBRBufferError.CreateFmt
      (CNT_ERR_BTRG_BASE,['CompressTo',e.classname,e.message]);
    end;
    {$ENDIF}
  end;
  {$ENDIF}

  (*  Method:   ExportTo()
      Purpose:  The ExportTo method allows you to read from the buffer,
                but output the data to an alternative target. In this case
                a TWriter class, which comes in handy when working
                with persistence.
      Comments: This method calls Write() to do the actual reading *)
  Function  TBRBuffer.ExportTo(aByteIndex:Int64;
            aLength:Integer;Const Writer:TBRWriter):Integer;
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
            mToRead:=EnsureRange(SizeOf(mCache),0,aLength);

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
            mToRead:=EnsureRange(SizeOf(mCache),0,aLength);

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
            aLength:Integer;Const Reader:TBRReader):Integer;
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
          mToRead:=EnsureRange(SizeOf(mCache),0,aLength);

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
          mToRead:=EnsureRange(SizeOf(mCache),0,aLength);

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
              aLength:=EnsureRange(aLength - mExtra,0,MAXINT);
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
        FOffset:=EnsureRange(Offset,0,FBufObj.Size);
      end;
    soCurrent:
      Begin
        FOffset:=EnsureRange(FOffset + Offset,0,FBufObj.Size);
      end;
    soEnd:
      Begin
        If Offset>0 then
        FOffset:=FBufObj.Size-1 else
        FOffset:=EnsureRange(FOffset-(abs(Offset)),0,FBufObj.Size);
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


  //##########################################################################
  // TBRWriterStream
  //##########################################################################

  Constructor TBRWriterStream.Create(Const Target:TStream);
  Begin
    inherited Create;
    if Target<>NIL then
    FStream:=target else
    Raise EBRWriter.Create(ERR_BR_WRITER_INVALIDTARGET);
  end;

  Function TBRWriterStream.Write(Const Data;
           DataLen:Integer):Integer;
  Begin
    If DataLen>0 then
    Begin
      Result:=FStream.Write(Data,DataLen);
      Advance(Result);
    end else
    result:=0;
  end;

  //##########################################################################
  // TBRReaderStream
  //##########################################################################

  Constructor TBRReaderStream.Create(Const Source:TStream);
  Begin
    inherited Create;
    if source<>NIL then
    FStream:=Source else
    Raise EBRReader.Create(ERR_BR_READER_INVALIDSOURCE);
  end;

  Function TBRReaderStream.Read(var Data;DataLen:Integer):Integer;
  Begin
    If DataLen>0 then
    Begin
      Result:=FStream.Read(Data,DataLen);
      Advance(Result);
    end else
    result:=0;
  end;

  //##########################################################################
  // TSRLDataWriter
  //##########################################################################

  Constructor TBRWriterBuffer.Create(Const Target:TBRBuffer);
  Begin
    inherited Create;
    If Target<>NIl then
    FData:=Target else
    Raise EBRWriter.Create(ERR_BR_WRITER_INVALIDTARGET);
  end;

  Function TBRWriterBuffer.Write(Const Data;DataLen:Integer):Integer;
  var
    FTotal: Int64;
  Begin
    If DataLen>0 then
    Begin
      FTotal:=FData.Size;
      If (FTotal>0) and (Position<FTotal) then
      FData.Write(Position,DataLen,Data) else
      FData.Append(Data,DataLen);

      Advance(DataLen);
      Result:=DataLen;
    end else
    result:=0;
  end;
     //4 +4 + 2 + 4 + 30
  //##########################################################################
  // TBRReaderBuffer
  //##########################################################################

  Constructor TBRReaderBuffer.Create(Const Source:TBRBuffer);
  Begin
    inherited Create;
    If Source=NIl then
    Raise EBRReader.Create(ERR_BR_READER_INVALIDSOURCE) else
    FData:=Source;
  end;

  Function TBRReaderBuffer.Read(var Data;DataLen:Integer):Integer;
  Begin
    If DataLen>0 then
    Begin
      Result:=FData.Read(Position,DataLen,data);
      Advance(Result);
    end else
    result:=0;
  end;


  //##########################################################################
  // TBRReader
  //##########################################################################

  Procedure TBRReader.Reset;
  Begin
    FOffset:=0;
  end;

  Procedure TBRReader.Advance(Const Value:Integer);
  Begin
    If Value>0 then
    FOffset:=FOffset + Value;
  end;

  Function TBRReader.ReadByte:Byte;
  Begin
    If Read(Result,SizeOf(Result))<SizeOf(Result) then
    Raise EBRReader.Create(ERR_BR_READER_FAILEDREAD);
  end;

  Function TBRReader.ReadBool:Boolean;
  Begin
    If Read(Result,SizeOf(Result))<SizeOf(Result) then
    Raise EBRReader.Create(ERR_BR_READER_FAILEDREAD);
  end;

  Function TBRReader.ReadWord:Word;
  Begin
    If Read(Result,SizeOf(Result))<SizeOf(Result) then
    Raise EBRReader.Create(ERR_BR_READER_FAILEDREAD);
  end;

  Function TBRReader.ReadSmall:SmallInt;
  Begin
    If Read(Result,SizeOf(Result))<SizeOf(Result) then
    Raise EBRReader.Create(ERR_BR_READER_FAILEDREAD);
  end;

  Function TBRReader.ReadPointer:Pointer;
  Begin
    If Read(Result,SizeOf(Result))<SizeOf(Result) then
    Raise EBRReader.Create(ERR_BR_READER_FAILEDREAD);
  end;

  Function TBRReader.ReadInt:Integer;
  Begin
    If Read(Result,SizeOf(Result))<SizeOf(Result) then
    Raise EBRReader.Create(ERR_BR_READER_FAILEDREAD);
  end;

  Function TBRReader.ReadLong:longword;
  Begin
    If Read(Result,SizeOf(Result))<SizeOf(Result) then
    Raise EBRReader.Create(ERR_BR_READER_FAILEDREAD);
  end;

  Function TBRReader.ReadInt64:Int64;
  Begin
    If Read(Result,SizeOf(Result))<SizeOf(Result) then
    Raise EBRReader.Create(ERR_BR_READER_FAILEDREAD);
  end;

  Function TBRReader.ReadCurrency:Currency;
  Begin
    If Read(Result,SizeOf(Result))<SizeOf(Result) then
    Raise EBRReader.Create(ERR_BR_READER_FAILEDREAD);
  end;

  Function TBRReader.ReadGUID:TGUID;
  Begin
    If Read(Result,SizeOf(Result))<SizeOf(Result) then
    Raise EBRReader.Create(ERR_BR_READER_FAILEDREAD);
  end;

  Function TBRReader.ReadShort:Shortint;
  Begin
    If Read(Result,SizeOf(Result))<SizeOf(Result) then
    Raise EBRReader.Create(ERR_BR_READER_FAILEDREAD);
  end;

  Function TBRReader.ReadSingle:Single;
  Begin
    If Read(Result,SizeOf(Result))<SizeOf(Result) then
    Raise EBRReader.Create(ERR_BR_READER_FAILEDREAD);
  end;

  Function TBRReader.ReadDouble:Double;
  Begin
    If Read(Result,SizeOf(Result))<SizeOf(Result) then
    Raise EBRReader.Create(ERR_BR_READER_FAILEDREAD);
  end;

  Function TBRReader.ReadDateTime:TDateTime;
  Begin
    If Read(Result,SizeOf(Result))<SizeOf(Result) then
    Raise EBRReader.Create(ERR_BR_READER_FAILEDREAD);
  end;

  Function  TBRReader.CopyTo(Const Writer:TBRWriter;
            CopyLen:Integer):Integer;
  var
    FRead:        Integer;
    FBytesToRead: Integer;
    FWritten:     Integer;
    FCache:       TBRIOCache;
  Begin
    If Writer<>NIL then
    Begin
      Result:=0;
      While CopyLen>0 do
      Begin
        FBytesToRead:=EnsureRange(SizeOf(FCache),0,CopyLen);
        FRead:=Read(FCache,FBytesToRead);
        If FRead>0 then
        Begin
          FWritten:=Writer.Write(FCache,FRead);
          dec(CopyLen,FWritten);
          inc(Result,FWritten);
        end else
        Break;
      end;
    end else
    Raise EBRReader.Create(ERR_BR_READER_INVALIDOBJECT);
  end;

  Function TBRReader.CopyTo(Const Binary:TBRBuffer;
           Const CopyLen:Integer):Integer;
  var
    FWriter: TBRWriter;
  Begin
    If Binary<>NIL then
    Begin
      FWriter:=TBRWriterBuffer.Create(Binary);
      try
        Result:=CopyTo(FWriter,CopyLen);
      finally
        FWriter.free;
      end;
    end else
    Raise EBRReader.Create(ERR_BR_READER_INVALIDOBJECT);
  end;

  Function  TBRReader.CopyTo(Const Stream:TStream;
            Const CopyLen:Integer):Integer;
  var
    FWriter: TBRWriterStream;
  Begin
    If Stream<>NIL then
    Begin
      FWriter:=TBRWriterStream.Create(Stream);
      try
        Result:=CopyTo(FWriter,CopyLen);
      finally
        FWriter.free;
      end;
    end else
    Raise EBRReader.Create(ERR_BR_READER_INVALIDOBJECT);
  end;

  Function TBRReader.ContentToStream:TStream;
  var
    FRead:  Integer;
    FCache: TBRIOCache;
  Begin
    Result:=TMemoryStream.Create;
    try
      While True do
      Begin
        FRead:=Read(FCache,SizeOf(FCache));
        If FRead>0 then
        Result.WriteBuffer(FCache,FRead) else
        Break;
      end;
    except
      on e: exception do
      Begin
        FreeAndNil(Result);
        Raise EBRReader.Create(e.message);
      end;
    end;
  end;

  Function TBRReader.ReadNone(Length:Integer):Integer;
  var
    FToRead:  Integer;
    FRead:    Integer;
    FCache:   TBRIOCache;
  Begin
    result:=0;
    If Length>0 then
    Begin
      try
        While Length>0 do
        Begin
          FToRead:=EnsureRange(SizeOf(FCache),0,Length);
          FRead:=Read(FCache,FToRead);
          If FRead>0 then
          Begin
            Length:=Length - FRead;
            Result:=Result + FRead;
          end else
          Break;
        end;
      except
        on e: exception do
        Raise EBRReader.Create(e.message);
      end;
    end;
  end;

  Function TBRReader.ContentToData:TBRBuffer;
  var
    FRead:  Integer;
    FCache: TBRIOCache;
  Begin
    Result:=TBRBufferMemory.Create;
    try
      While True do
      Begin
        FRead:=Read(FCache,SizeOf(FCache));
        If FRead>0 then
        Result.Append(FCache,FRead) else
        Break;
      end;
    except
      on e: exception do
      Begin
        FreeAndNil(Result);
        Raise EBRReader.Create(e.message);
      end;
    end;
  end;

  Function TBRReader.ReadAsc(Const Length:Integer):AnsiString;
  var
    FRead: Integer;
  Begin
    If Length>0 then
    Begin
      SetLength(Result,Length);
      FRead:=Read(Result[1],Length);
      If FRead<Length then
      SetLength(Result,FRead);
    end else
    Result:='';
  end;

  Function  TBRReader.ReadAsc(Const Terminator:AnsiString
            =CNT_BR_CRLF):AnsiString;

    Function JL_RightStr(Const Value:AnsiString;Count:Integer):AnsiString;
    var
      FLen: Integer;
    Begin
      FLen:=Length(Value);
      If (FLen>0) and (Count>0) then
      Begin
        If Count>FLen then
        Count:=FLen;
        Result:=Copy(Value,(FLen-Count)+1,Count);
      end else
      result:='';
    end;

  var
    FCurrent: AnsiChar;
    FLen:     Integer;
  Begin
    result:='';
    FLen:=Length(Terminator);
    If FLen>0 then
    Begin
      While Read(FCurrent,SizeOf(FCurrent))=SizeOf(FCurrent) do
      Begin
        Result:=Result + FCurrent;
        If JL_RightStr(Result,FLen)=Terminator then
        Begin
          SetLength(Result,Length(Result) - FLen);
          Break;
        end;
      end;
    end else
    Begin
      While Read(FCurrent,SizeOf(FCurrent))=SizeOf(FCurrent) do
      Result:=Result + FCurrent;
    end;
  end;

  Function TBRReader.ReadToEOL:AnsiString;
  Begin
    Result:=ReadAsc;
  end;

  Function TBRReader.DoReadAnsiString:AnsiString;
  var
    FBytes: Integer;
    FLen:   Integer;
  Begin
    (* normal ansi string *)
    FBytes:=ReadInt;
    If FBytes>0 then
    Begin
      SetLength(Result,FBytes);
      FLen:=Read(Result[1],FBytes);
      If FLen<FBytes then
      Raise EBRReader.Create(ERR_BR_READER_FAILEDREAD);
    end else
    Result:='';
  end;

  Function TBRReader.DoReadWideString:WideString;
  var
    FRead:  Integer;
    FBytes: Integer;
    FLen:   Integer;
  Begin
    FLen:=ReadWord;
    If FLen>0 then
    Begin
      FBytes:=FLen * SizeOf(WideChar);
      SetLength(Result,FLen);
      FRead:=Read(Result[1],FBytes);
      If FRead<FBytes then
      Raise EBRReader.Create(ERR_BR_READER_FAILEDREAD);
    end else
    result:='';
  end;

  Function TBRReader.DoReadString:String;
  var
    FRead:  Integer;
    FBytes: Integer;
    FLen:   Integer;
  Begin
    FLen:=ReadInt;
    If FLen>0 then
    Begin
      FBytes:=FLen * SizeOf(Char);
      SetLength(Result,FLen);
      FRead:=Read(Result[1],FBytes);
      If FRead<FBytes then
      Raise EBRReader.Create(ERR_BR_READER_FAILEDREAD);
    end else
    result:='';
  end;

  Function TBRReader.ReadString:String;
  var
    FHead: Word;
  Begin
    (* Get String identifier *)
    FHead:=ReadWord;

    (* Support both ansi and widestring *)
    Case FHead of
    CNT_BR_ASTRING_HEADER: Result:=String(doReadAnsiString);
    CNT_BR_USTRING_HEADER: Result:=DoReadString;
    CNT_BR_WSTRING_HEADER: Result:=DoReadWideString;
    else
      Raise EBRReader.CreateFmt
      (ERR_BR_READER_INVALIDHEADER,[CNT_BR_ASTRING_HEADER,FHead]);
    End;
  end;

  Function TBRReader.ReadWideString:WideString;
  var
    FHead: Word;
  Begin
    (* Get String identifier *)
    FHead:=ReadWord;

    (* Support both ansi and widestring *)
    Case FHead of
    CNT_BR_ASTRING_HEADER: Result:=string(DoReadAnsiString);
    CNT_BR_WSTRING_HEADER: Result:=DoReadWideString;
    else
      Raise EBRReader.CreateFmt
      (ERR_BR_READER_INVALIDHEADER,[CNT_BR_ASTRING_HEADER,FHead]);
    End;
  end;

  {$IFDEF BR_SUPPORT_VARIANTS}
  Function TBRReader.ReadVariant:Variant;
  var
    FTemp:    Word;
    FKind:    TVarType;
    FCount,x: Integer;
    FIsArray: Boolean;

    Function ReadVariantData:Variant;
    var
      FTyp: TVarType;
    Begin
      FTyp:=ReadWord;
      Case FTyp of
      varError:     Result:=VarAsError(ReadLong);
      varVariant:   Result:=ReadVariant;
      varByte:      Result:=ReadByte;
      varBoolean:   Result:=ReadBool;
      varShortInt:  Result:=ReadShort;
      varWord:      Result:=ReadWord;
      varSmallint:  Result:=ReadSmall;
      varInteger:   Result:=ReadInt;
      varLongWord:  Result:=ReadLong;
      varInt64:     Result:=ReadInt64;
      varSingle:    Result:=ReadSingle;
      varDouble:    Result:=ReadDouble;
      varCurrency:  Result:=ReadCurrency;
      varDate:      Result:=ReadDateTime;
      varString:    Result:=ReadString;
      varOleStr:    Result:=ReadWideString;
      end;
    end;

  Begin
    FTemp:=ReadWord;
    If FTemp=CNT_BR_VARIANT_HEADER then
    Begin
      (* read datatype *)
      FKind:=TVarType(ReadWord);

      If not (FKind in [varEmpty,varNull]) then
      Begin
        (* read array declaration *)
        FIsArray:=ReadBool;

        If FIsArray then
        Begin
          FCount:=ReadInt;
          Result:=VarArrayCreate([0,FCount-1],FKind);
          for x:=1 to FCount do
          VarArrayPut(Result,ReadVariantData,[0,x-1]);
        end else
        Result:=ReadVariantData;
      end else
      result:=NULL;

    end else
    Raise EBRReader.CreateFmt
    (ERR_BR_READER_INVALIDHEADER,
    [CNT_BR_VARIANT_HEADER,FTemp]);
  end;
  {$ENDIF}

  Function TBRReader.ReadData:TBRBuffer;
  var
    FTotal:   Int64;
    FToRead:  Integer;
    FRead:    Integer;
    FCache:   TBRIOCache;
  Begin
    Result:=TBRBufferMemory.Create;
    try
      FTotal:=ReadInt64;
      While FTotal>0 do
      Begin
        FToRead:=EnsureRange(SizeOf(FCache),0,FTotal);
        FRead:=Read(FCache,FToRead);
        If FRead>0 then
        Begin
          Result.Append(FCache,FRead);
          FTotal:=FTotal - FRead;
        end else
        Break;
      end;
    except
      on e: exception do
      Begin
        FreeAndNil(Result);
        raise EBRReader.Create(e.message);
      end;
    end;
  end;

  Function TBRReader.ReadStream:TStream;
  var
    FTotal:   Int64;
    FToRead:  Integer;
    FRead:    Integer;
    FCache:   TBRIOCache;
  Begin
    Result:=TMemoryStream.Create;
    try
      FTotal:=ReadInt64;
      While FTotal>0 do
      Begin
        FToRead:=EnsureRange(SizeOf(FCache),0,FTotal);
        FRead:=Read(FCache,FToRead);
        If FRead>0 then
        Begin
          Result.WriteBuffer(FCache,FRead);
          FTotal:=FTotal - FRead;
        end else
        Break;
      end;
      Result.Position:=0;
    except
      on e: exception do
      Begin
        FreeAndNil(Result);
        raise EBRReader.Create(e.message);
      end;
    end;
  end;

  //##########################################################################
  // TBRWriter
  //##########################################################################

  Procedure TBRWriter.Reset;
  Begin
    FOffset:=0;
  end;

  Procedure TBRWriter.Advance(Const Value:Integer);
  Begin
    If Value>0 then
    FOffset:=FOffset + Value;
  end;

  {$IFDEF BR_SUPPORT_VARIANTS}
  Procedure TBRWriter.WriteVariant(Const Value:Variant);
  var
    FKind:    TVarType;
    FCount,x: Integer;
    FIsArray: Boolean;
    //FData:    PByte;

    Procedure WriteVariantData(Const VarValue:Variant);
    var
      FAddr:  PVarData;
    Begin
      FAddr:=FindVarData(VarValue);
      If FAddr<>NIL then
      Begin
        (* write datatype *)
        WriteWord(FAddr^.VType);

        (* write variant content *)
        Case FAddr^.VType of
        varVariant:   WriteVariantData(VarValue);
        varError:     WriteLong(TVarData(varValue).VError);
        varByte:      WriteByte(FAddr^.VByte);
        varBoolean:   WriteBool(FAddr^.VBoolean);
        varShortInt:  WriteShort(FAddr^.VShortInt);
        varWord:      WriteWord(FAddr^.VWord);
        varSmallint:  WriteSmall(FAddr^.VSmallInt);
        varInteger:   WriteInt(FAddr^.VInteger);
        varLongWord:  WriteLong(FAddr^.VLongWord);
        varInt64:     WriteInt64(FAddr^.VInt64);
        varSingle:    WriteSingle(FAddr^.VSingle);
        varDouble:    WriteDouble(FAddr^.VDouble);
        varCurrency:  WriteCurrency(FAddr^.VCurrency);
        varDate:      WriteDateTime(FAddr^.VDate);
        varString:    WriteString(String(FAddr^.VString));
        varOleStr:    WriteWideString(WideString(FAddr^.VString));
        end;
      end;
    end;
  Begin
    (* Extract datatype & exclude array info *)
    FKind:=VarType(Value) and varTypeMask;

    (* Write variant header *)
    WriteWord(CNT_BR_VARIANT_HEADER);

    (* write datatype *)
    WriteWord(FKind);

    (* Content is array? *)
    FIsArray:=VarIsArray(Value);

    (* write array declaration *)
    If not (FKind in [varEmpty,varNull]) then
    Begin
      (* write TRUE if variant is an array *)
      WriteBool(FIsArray);

      (* write each item if array, or just the single one.. *)
      If FIsArray then
      Begin
        (* write # of items *)
        FCount:=VarArrayHighBound(Value,1) - VarArrayLowBound(Value,1) + 1;
        WriteInt(FCount);

        (* write each element in array *)
        for x:=VarArrayLowBound(Value,1) to VarArrayHighBound(Value,1) do
        WriteVariantData(VarArrayGet(Value,[1,x-1]));

      end else
      WriteVariantData(Value);
    end;
  end;
  {$ENDIF}

  Procedure TBRWriter.WriteByte(Const Value:Byte);
  Begin
    If Write(Value,SizeOf(Value))<SizeOf(Value) then
    Raise EBRWriter.Create(ERR_BR_WRITER_FAILEDWRITE);
  end;

  Procedure TBRWriter.WriteShort(Const Value:ShortInt);
  Begin
    If Write(Value,SizeOf(Value))<SizeOf(Value) then
    Raise EBRWriter.Create(ERR_BR_WRITER_FAILEDWRITE);
  end;

  Procedure TBRWriter.WriteBool(Const Value:Boolean);
  Begin
    If Write(Value,SizeOf(Value))<SizeOf(Value) then
    Raise EBRWriter.Create(ERR_BR_WRITER_FAILEDWRITE);
  end;

  Procedure TBRWriter.WriteWord(Const Value:Word);
  Begin
    If Write(Value,SizeOf(Value))<SizeOf(Value) then
    Raise EBRWriter.Create(ERR_BR_WRITER_FAILEDWRITE);
  end;

  Procedure TBRWriter.WriteSmall(Const Value:SmallInt);
  Begin
    If Write(Value,SizeOf(Value))<SizeOf(Value) then
    Raise EBRWriter.Create(ERR_BR_WRITER_FAILEDWRITE);
  end;

  Procedure TBRWriter.WritePointer(Const Value:Pointer);
  Begin
    If Write(Value,SizeOf(Value))<SizeOf(Value) then
    Raise EBRWriter.Create(ERR_BR_WRITER_FAILEDWRITE);
  end;

  procedure TBRWriter.WriteInt(Const Value:Integer);
  Begin
    If Write(Value,SizeOf(Value))<SizeOf(Value) then
    Raise EBRWriter.Create(ERR_BR_WRITER_FAILEDWRITE);
  end;

  Procedure TBRWriter.WriteLong(Const Value:Longword);
  Begin
    If Write(Value,SizeOf(Value))<SizeOf(Value) then
    Raise EBRWriter.Create(ERR_BR_WRITER_FAILEDWRITE);
  end;

  Procedure TBRWriter.WriteGUID(Const Value:TGUID);
  Begin
    If Write(Value,SizeOf(Value))<SizeOf(Value) then
    Raise EBRWriter.Create(ERR_BR_WRITER_FAILEDWRITE);
  end;

  procedure TBRWriter.WriteInt64(Const Value:Int64);
  Begin
    If Write(Value,SizeOf(Value))<SizeOf(Value) then
    Raise EBRWriter.Create(ERR_BR_WRITER_FAILEDWRITE);
  end;

  Procedure TBRWriter.WriteCurrency(Const Value:Currency);
  Begin
    If Write(Value,SizeOf(Value))<SizeOf(Value) then
    Raise EBRWriter.Create(ERR_BR_WRITER_FAILEDWRITE);
  end;

  Procedure TBRWriter.WriteSingle(Const Value:Single);
  Begin
    If Write(Value,SizeOf(Value))<SizeOf(Value) then
    Raise EBRWriter.Create(ERR_BR_WRITER_FAILEDWRITE);
  end;

  procedure TBRWriter.WriteDouble(Const Value:Double);
  Begin
    If Write(Value,SizeOf(Value))<SizeOf(Value) then
    Raise EBRWriter.Create(ERR_BR_WRITER_FAILEDWRITE);
  end;

  procedure TBRWriter.WriteDateTime(Const Value:TDateTime);
  Begin
    If Write(Value,SizeOf(Value))<SizeOf(Value) then
    Raise EBRWriter.Create(ERR_BR_WRITER_FAILEDWRITE);
  end;

  Procedure TBRWriter.WriteFile(Const Filename:String);
  var
    FFile: TFileStream;
  Begin
    FFile:=TFileStream.Create(Filename,fmOpenRead or fmShareDenyNone);
    try
      WriteStreamContent(FFile,False);
    finally
      FFile.free;
    end;
  end;

  Procedure TBRWriter.WriteStreamContent(Const Value:TStream;
            Const Disposable:Boolean=False);
  var
    FTotal:     Int64;
    FRead:      Integer;
    FWritten:   Integer;
    FCache:     TBRIOCache;
  Begin
    If Value<>NIl then
    Begin
      try
        FTotal:=Value.Size;
        If FTotal>0 then
        Begin
          Value.Position:=0;
          Repeat
            FRead:=Value.Read(FCache,SizeOf(FCache));
            If FRead>0 then
            Begin
              FWritten:=Write(FCache,FRead);
              FTotal:=FTotal - FWritten;
            end;
          Until (FRead<1) or (FTotal<1);
        end;
      finally
        If Disposable then
        Value.free;
      end;
    end else
    Raise EBRWriter.Create(ERR_BR_WRITER_INVALIDDATASOURCE);
  end;

  Procedure TBRWriter.WriteDataContent(Const Value:TBRBuffer;
            Const Disposable:Boolean=False);
  var
    FBytes:   Integer;
    FRead:    Integer;
    FWritten: Integer;
    FOffset:  Integer;
    FCache:   TBRIOCache;
  Begin
    If (Value<>NIl) then
    Begin

      try
        FOffset:=0;
        FBytes:=Value.Size;

        Repeat
          FRead:=Value.Read(FOffset,SizeOf(FCache),FCache);
          If FRead>0 then
          Begin
            FWritten:=Write(FCache,FRead);
            FBytes:=FBytes-FWritten;
            FOffset:=FOffset + FWritten;
          end;
        Until (FRead<1) or (FBytes<1);

      finally
        If Disposable then
        Value.free;
      end;

    end else
    Raise EBRWriter.Create(ERR_BR_WRITER_INVALIDDATASOURCE);
  end;

  procedure TBRWriter.WriteData(Const Value:TBRBuffer;
            Const Disposable:Boolean);
  var
    FTemp:  Int64;
  Begin
    If Value<>NIl then
    Begin
      try
        FTemp:=Value.Size;
        WriteInt64(FTemp);
        If FTemp>0 then
        WriteDataContent(Value);
      finally
        If Disposable then
        Value.free;
      end;
    end else
    Raise EBRWriter.Create(ERR_BR_WRITER_INVALIDDATASOURCE);
  end;

  procedure TBRWriter.WriteStream(Const Value:TStream;
            Const Disposable:Boolean);
  Begin
    If Value<>NIl then
    Begin
      try
        WriteInt64(Value.Size);
        If Value.Size>0 then
        WriteStreamContent(Value);
      finally
        If Disposable then
        Value.free;
      end;
    end else
    Raise EBRWriter.Create(ERR_BR_WRITER_INVALIDDATASOURCE);
  end;

  Function TBRWriter.CopyFrom(Const Reader:TBRReader;
            DataLen:Int64):Int64;
  var
    FRead:        Integer;
    FBytesToRead: Integer;
    FCache:       TBRIOCache;
  Begin
    If Reader<>NIL then
    Begin
      result:=0;
      While DataLen>0 do
      Begin
        {FBytesToRead:=Sizeof(FCache);
        If FBytesToRead>DataLen then
        FBytesToRead:=DataLen; }
        FBytesToRead:=EnsureRange(SizeOf(FCache),0,DataLen);

        FRead:=Reader.Read(FCache,FBytesToRead);
        If FRead>0 then
        Begin
          Write(FCache,FRead);
          DataLen:=DataLen - FRead;
          Result:=Result + FRead;
        end else
        Break;
      end;
    end else
    Raise EBRWriter.Create(ERR_BR_WRITER_INVALIDDATASOURCE);
  end;

  Function TBRWriter.CopyFrom(Const Stream:TStream;
           Const DataLen:Int64):Int64;
  var
    FReader: TBRReaderStream;
  Begin
    If Stream<>NIL then
    Begin
      FReader:=TBRReaderStream.Create(Stream);
      try
        Result:=CopyFrom(FReader,DataLen);
      finally
        FReader.free;
      end;
    end else
    Raise EBRWriter.Create(ERR_BR_WRITER_INVALIDDATASOURCE);
  end;

  Function TBRWriter.CopyFrom(Const Data:TBRBuffer;
           Const DataLen:Int64):Int64;
  var
    FReader: TBRReaderBuffer;
  Begin
    If Data<>NIL then
    Begin
      FReader:=TBRReaderBuffer.Create(Data);
      try
        Result:=CopyFrom(FReader,DataLen);
      finally
        FReader.free;
      end;
    end else
    Raise EBRWriter.Create(ERR_BR_WRITER_INVALIDDATASOURCE);
  end;

  Procedure TBRWriter.WriteCRLF(Const Times:Integer=1);
  var
    FLen:   Integer;
    FWord:  Word;
    FData:  Pointer;
  Begin
    If Times>0 then
    Begin
      FWord:=2573; // [#13,#10]

      If Times=1 then
      Write(FWord,SizeOf(FWord)) else

      if Times=2 then
      Begin
        Write(FWord,SizeOf(FWord));
        Write(FWord,SizeOf(FWord));
      end else

      if Times>2 then
      Begin
        FLen:=SizeOf(FWord) * Times;
        FData:=Allocmem(FLen);
        try
          JL_FillWord(FData,Times,FWord);
          Write(FData^,FLen);
        finally
          FreeMem(FData);
        end;
      end;
    end;
  end;

  Procedure TBRWriter.WriteAsc(Value:AnsiString;
            Const Delimiter:AnsiString=CNT_BR_CRLF);
  var
    FLen: Integer;
  Begin
    Value:=Value + Delimiter;
    FLen:=Length(Value);
    If FLen>0 then
    Write(Value[1],FLen);
  end;

  Procedure TBRWriter.WriteString(Const Value:String);
  var
    FLen: Integer;
  Begin
    (* write string header *)
    WriteWord(CNT_BR_USTRING_HEADER);

    (* get length of data *)
    FLen:=Length(Value);

    (* write length of data *)
    WriteInt(FLen);

    (* Any the actual data *)
    {$WARNINGS OFF}
    If FLen>0 then
    If Write(Value[1],FLen * SizeOf(Char))<FLen then
    Raise EBRWriter.Create(ERR_BR_WRITER_FAILEDWRITE);
    {$WARNINGS ON}
  end;

  Procedure TBRWriter.WriteWideString(Const Value:WideString);
  var
    FLen: Integer;
  Begin
    (* write string header *)
    WriteWord(CNT_BR_WSTRING_HEADER);

    (* get length of data *)
    FLen:=Length(Value);

    (* write length of data *)
    WriteInt(FLen);

    (* Any the actual data *)
    {$WARNINGS OFF}
    If FLen>0 then
    Begin
      FLen:=FLen * 2;
      If Write(Value[1],FLen)<FLen then
      Raise EBRWriter.Create(ERR_BR_WRITER_FAILEDWRITE);
    end;
    {$WARNINGS ON}
  end;

//############################################################################
// TBRBitAccess
//############################################################################

constructor TBRBitAccess.Create(const aBuffer: TBRBuffer);
begin
  inherited Create;
  FBuffer:=aBuffer;
end;

function TBRBitAccess.asString(const aLineLen:Integer=32):String;
var
  x:  NativeUInt;
  mBreak: Integer;
begin
  SetLength(Result,0);
  mBreak:=0;
  for x:=0 to getCount-1 do
  Begin
    Inc(mBreak);
    if readBit(x) then
    result:=Result + '1' else
    result:=result + '0';
    if mBreak>=aLineLen then
    Begin
      result:=Result + #13#10;
      mBreak:=0;
    end;
  end;
end;

function TBRBitAccess.FindIdleBit(const aFromIndex:NativeUInt;
         out aIdleBitIndex:NativeUInt):Boolean;
var
  x:      NativeUInt;
  mCount: NativeUInt;
Begin
  aIdleBitIndex:=0;
  Result:=False;
  mCount:=getCount;
  if aFromIndex<mCount-1 then
  Begin
    for x:=aFromIndex to mCount-1 do
    Begin
      if readBit(x)=False then
      Begin
        aIdleBitIndex:=x;
        Result:=true;
        break;
      end;
    end;
  end;
end;

function TBRBitAccess.readBit(const aIndex: NativeInt): Boolean;
var
  mOffset:  NativeUInt;
  mBitOff:  0..255;
  mByte:    Byte;
begin
  mOffset:=aIndex shr 3;
  mBitOff:=aIndex mod 8;
  if FBuffer.Read(mOffset,SizeOf(Byte),mByte)=SizeOf(byte) then
  Result:=(mByte and (1 shl (mBitOff mod 8)))<>0 else
  result:=False;
end;

procedure TBRBitAccess.writeBit(const aIndex: NativeInt;
  const aValue: Boolean);
var
  mOffset:  NativeUInt;
  mBitOff:  0..255;
  mByte:    Byte;
  mSet: Boolean;
begin
  mOffset:=aIndex shr 3;
  mBitOff:=aIndex mod 8;
  if FBuffer.Read(mOffset,SizeOf(Byte),mByte)=SizeOf(Byte) then
  Begin
    mSet:=(mByte and (1 shl (mBitOff mod 8)))<>0;
    if mSet<>aValue then
    Begin
      case aValue of
      True:   mByte:=(mByte or (1 shl (mBitOff mod 8)));
      false:  mByte:=(mByte and not (1 shl (mBitOff mod 8)));
      end;
      FBuffer.Write(mOffset,1,mByte);
    end;
  end;
end;

function TBRBitAccess.getCount:NativeUInt;
Begin
  Result:=FBuffer.Size shl 3;
end;

function TBRBitAccess.getEmpty:Boolean;
begin
  result:=FBuffer.Size=0;
end;

  Procedure TBRBitAccess.SetBitRange(First,Last:NativeUInt;
            Const Bitvalue:Boolean);
  var
    x:        NativeUInt;
    //FLongs:   NativeInt;
    //FSingles: NativeInt;
    //FCount:   NativeUInt;
  Begin
    //If FData<>NIL then
    //Begin
      If  First<Count then
      Begin
        If Last<Count then
        Begin
          (* Conditional swap *)
          If First>Last then
          Begin
            x:=last;
            last:=First;
            First:=x;
            //SRLSwap(First,Last);
          end;

          x:=First;
          While x<=Last do
          Begin
            writeBit(x,BitValue);
          end;

          {
          (* get totals, take ZERO into account *)
          FCount:=SRLDiff(First,Last,True);

          (* use refactoring & loop reduction *)
          FLongs:=Integer(FCount shr 3);

          x:=First;

          while FLongs>0 do
          Begin
            SetBit(x,Bitvalue);inc(x);
            SetBit(x,Bitvalue);inc(x);
            SetBit(x,Bitvalue);inc(x);
            SetBit(x,Bitvalue);inc(x);
            SetBit(x,Bitvalue);inc(x);
            SetBit(x,Bitvalue);inc(x);
            SetBit(x,Bitvalue);inc(x);
            SetBit(x,Bitvalue);inc(x);
            dec(FLongs);
          end;

          (* process singles *)
          FSingles:=TSRLInt(FCount mod 8);
          while FSingles>0 do
          Begin
            SetBit(x,Bitvalue);inc(x);
            dec(FSingles);
          end; }

        end else
        Begin
          If First=Last then
          WriteBit(First,True) else
          Raise Exception.Create('Invalid bit index');
          //(ERR_SRLBitBuffer_InvalidBitIndex,[FBitsMax,Last]);
        end;
      end else
      Raise Exception.Create('Invalid bit index');
      //Raise GetException.CreateFmt(ERR_SRLBitBuffer_InvalidBitIndex,
      //[FBitsMax,First]);
    //end else
    //Raise GetException.Create(ERR_SRLBitBuffer_BitBufferEmpty);
  end;


//############################################################################
// TBRPartsAccess
//############################################################################

constructor TBRPartsAccess.Create(const aBuffer: TBRBuffer;
  const aReservedHeaderSize, aPartSize: Integer);
begin
  inherited Create;
  if aBuffer<>nil then
  FBuffer:=aBuffer else
  raise Exception.Create(CNT_PARTACCESS_BUFFERISNIL);

  FheadSize:=EnsureRange(aReservedHeaderSize,0,MaxInt);

  if aPartSize>0 then
  FPartSize:=aPartSize else
  Raise Exception.Create(CNT_PARTACCESS_PARTSIZEINVALID);
end;


function TBRPartsAccess.CalcPartsForData(const aDataSize: Int64): NativeInt;
begin
  Result:=0;
  if (aDataSize>0) and (FPartSize>0) then
  Begin
    result:=aDatasize div FPartSize;
    if (result * FPartSize) < FBuffer.Size then
    inc(Result);
  end;
end;

function TBRPartsAccess.getOffsetForPart(const aPartIndex: Integer): Int64;
begin
  Result:=FheadSize + (aPartIndex * FPartSize);
end;

function TBRPartsAccess.getPartCount:Integer;
var
  mTotal: Int64;
begin
  Result:=0;
  if FBuffer.Size>0 then
  Begin
    if FPartSize>0 then
    Begin
      mTotal:=FBuffer.Size - FheadSize;
      Result:=mTotal div FPartSize;
      if (result * FPartSize)<mTotal then
      Inc(result);
    end;
  end;
end;

procedure TBRPartsAccess.ReadPart(const aIndex: Integer; var aData);
var
  mOffset:  Int64;
begin
  if (mcRead in FBuffer.Capabilities) then
  Begin
    mOffset:=getOffsetForPart(aIndex);
    FBuffer.Read(mOffset,FPartSize,aData);
  end else
  Raise Exception.Create(CNT_ERR_BTRG_READNOTSUPPORTED);
end;

procedure TBRPartsAccess.ReadPart(const aIndex: Integer;
  const aBuffer: TBRBuffer);
var
  mOffset:  Int64;
  mData:  Pointer;
begin
  if (mcRead in FBuffer.Capabilities) then
  Begin
    if aBuffer<>nil then
    Begin
      aBuffer.Release;
      mOffset:=getOffsetForPart(aIndex);
      mData:=AllocMem(FPartSize);
      try
        FBuffer.Read(mOffset,FPartSize,mData^);
        aBuffer.Append(mData^,FPartSize);
      finally
        FreeMem(mData);
      end;
    end else
    Raise Exception.Create(CNT_PARTACCESS_TARGETBUFFERINVALID);
  end else
  Raise Exception.Create(CNT_ERR_BTRG_READNOTSUPPORTED);
end;

procedure TBRPartsAccess.AppendPart(const aData;aLength:Integer);
var
  mData:  Pointer;
Begin
  (* Check that buffer can scale and that we have write access *)
  if (mcWrite in FBuffer.Capabilities)
  and (mcScale in FBuffer.Capabilities) then
  Begin
    aLength:=EnsureRange(aLength,0,FPartSize);
    if aLength>0 then
    Begin
      mData:=AllocMem(FPartSize);
      try
        TBRBuffer.FillByte(PByte(mData),FPartSize,0);
        Move(aData,mData^,aLength);
        FBuffer.Append(mData^,FPartSize);
      finally
        FreeMem(mData);
      end;
    end;
  end;
end;

procedure TBRPartsAccess.AppendPart(const aData:TBRBuffer);
var
  mData:  Pointer;
  mLength:  Integer;
Begin
  (* Check that buffer can scale and that we have write access *)
  if (mcWrite in FBuffer.Capabilities)
  and (mcScale in FBuffer.Capabilities) then
  Begin
    mLength:=EnsureRange(aData.Size,0,FPartSize);
    if mLength>0 then
    Begin
      mData:=AllocMem(FPartSize);
      try
        aData.Read(0,mLength,mData^);
        FBuffer.Append(mData^,FPartSize);
      finally
        FreeMem(mData);
      end;
    end;
  end;
end;

procedure TBRPartsAccess.WritePart(const aIndex: Integer; const aData;
  const aLength: Integer);
var
  mOffset:  Int64;
begin
  (* Check that buffer have write access *)
  if (mcWrite in FBuffer.Capabilities) then
  Begin
    if aLength>0 then
    Begin
      if FBuffer.Size>0 then
      Begin
        mOffset:=getOffsetForPart(aIndex);
        FBuffer.Write(mOffset,aLength,aData);
      end;
    end;
  end;
end;

procedure TBRPartsAccess.WritePart(const aIndex: Integer;
          const aData: TBRBuffer);
var
  mOffset:  Int64;
  mData:  Pointer;
  mToRead:  Integer;
begin
  (* Check that buffer have write access *)
  if (mcWrite in FBuffer.Capabilities) then
  Begin
    if (aData<>NIL) and (aData.Size>0) then
    Begin
      if FBuffer.Size>0 then
      Begin
        mOffset:=getOffsetForPart(aIndex);

        mToRead:=EnsureRange(aData.Size,1,FPartSize);

        mData:=AllocMem(mToRead);
        try
          aData.Read(0,mToRead,mData^);
          FBuffer.Write(mOffset,mToRead,mData^);
        finally
          FreeMem(mData);
        end;
      end;
    end;
  end;
end;


//##########################################################################
// TBRCustomRecord
//##########################################################################

Constructor TBRCustomRecord.Create;
Begin
  inherited Create;
  FObjects:=TObjectList.Create(True);
end;

Destructor TBRCustomRecord.Destroy;
Begin
  FObjects.free;
  inherited;
end;

(* procedure TBRCustomRecord.Assign(source:TPersistent);
var
  mStream:  TStream;
begin
  if (source<>nil)
  and (source is TBRCustomRecord) then
  begin
    mStream:=TBRCustomRecord(source).toStream;
    try
      LoadFromStream(mStream);
    finally
      mStream.Free;
    end;
  end else
  inherited;
end;  *)

Procedure TBRCustomRecord.Clear;
Begin
  FObjects.Clear;
end;

(* function TBRCustomRecord.toStream:TStream;
Begin
  result:=TMemoryStream.Create;
  try
    SaveToStream(result);
    result.Position:=0;
  except
    on exception do
    Begin
      FreeAndNIL(result);
      Raise;
    end;
  end;
end; *)

(* function TBRCustomRecord.toBuffer:TBRBuffer;
var
  mAdapter: TBRStreamAdapter;
Begin
  result:=TBRBufferMemory.Create;
  try
    mAdapter:=TBRStreamAdapter.Create(result);
    try
      SaveToStream(mAdapter);
    finally
      mAdapter.Free;
    end;
  except
    on exception do
    Begin
      FreeAndNIL(result);
      Raise;
    end;
  end;
end; *)

Procedure TBRCustomRecord.BeforeReadObject;
begin
  inherited;
  FObjects.Clear;
end;

Procedure TBRCustomRecord.WriteObject(Const Writer:TBRWriter);
var
  x:  Integer;
Begin
  inherited;
  Writer.WriteInt(FObjects.Count);
  for x:=0 to FObjects.Count-1 do
  begin
    Writer.WriteString(Items[x].ClassName);
    IBRPersistent(items[x]).ObjectTo(Writer);
  end;
end;

Procedure TBRCustomRecord.ReadObject(Const Reader:TBRReader);
var
  x, mCount:Integer;
  mId:  String;
  mField: TBRRecordField;
Begin
  inherited;
  mCount:=Reader.ReadInt;
  if mCount>0 then
  begin
    for x:=0 to mCount-1 do
    begin
      mId:=Reader.ReadString;
      if BRRecordInstanceFromName(mId,mField) then
      Begin
        FObjects.Add(mField);
        IBRPersistent(mField).ObjectFrom(Reader);
      end;
    end;
  end;
end;

(* Procedure TBRCustomRecord.SaveToStream(Const stream:TStream);
var
  x:  Integer;
  mWriter:  TBRWriter;
Begin
  mWriter:=TBRWriterStream.Create(stream);
  try
    mWriter.WriteLong(CNT_RECORD_HEADER);
    mWriter.WriteInt(FObjects.Count);
    for x:=0 to FObjects.Count-1 do
    Begin
      mWriter.WriteString(items[x].ClassName);
      IBRPersistent(items[x]).ObjectTo(mWriter);
      //items[x].WriteObject(mWriter);
    end;
  finally
    mWriter.Free;
  end;
end;

procedure TBRCustomRecord.LoadFromStream(Const stream:TStream);
var
  x:  Integer;
  mReader:  TBRReader;
  mHead:  Longword;
  mCount: Integer;
  mName:  String;
  mField: TBRRecordField;
Begin
  Clear;
  mReader:=TBRReaderStream.Create(stream);
  try
    mHead:=mReader.ReadLong;
    if mHead=CNT_RECORD_HEADER then
    begin
      mCount:=mReader.ReadInt;
      for x:=0 to mCount-1 do
      Begin
        mName:=mReader.ReadString;
        if BRRecordInstanceFromName(mName,mField) then
        Begin
          self.FObjects.Add(mField);
          IBRPersistent(mField).ObjectFrom(mReader);
          //mField.ReadObject(mReader);
        end else
        Raise EBRRecordError.CreateFmt
        ('Unknown field class [%s] error',[mName]);
      end;
    end else
    Raise EBRRecordError.Create('Invalid record header error');
  finally
    mReader.Free;
  end;
end;  *)

Procedure TBRCustomRecord.WriteInt(const aName:String;const Value:Integer);
var
  mRef: TBRRecordField;
Begin
  mRef:=ObjectOf(aName);
  if mRef=NIL then
  mRef:=Add(aName,TBRFieldInteger);
  TBRFieldInteger(mRef).Value:=Value;
end;

procedure TBRCustomRecord.WriteStr(const aName:String;const Value:String);
var
  mRef: TBRRecordField;
Begin
  mRef:=ObjectOf(aName);
  if mRef=NIL then
  mRef:=Add(aName,TBRFieldString);
  TBRFieldString(mRef).Value:=Value;
end;

Procedure TBRCustomRecord.WriteByte(const aName:String;const Value:Byte);
var
  mRef: TBRRecordField;
Begin
  mRef:=ObjectOf(aName);
  if mRef=NIL then
  mRef:=Add(aName,TBRFieldByte);
  TBRFieldByte(mRef).Value:=Value;
end;

procedure TBRCustomRecord.WriteBool(const aName:String;const Value:Boolean);
var
  mRef: TBRRecordField;
Begin
  mRef:=ObjectOf(aName);
  if mRef=NIL then
  mRef:=Add(aName,TBRFieldBoolean);
  TBRFieldBoolean(mRef).Value:=Value;
end;

procedure TBRCustomRecord.WriteCurrency(const aName:String;
          const Value:Currency);
var
  mRef: TBRRecordField;
Begin
  mRef:=ObjectOf(aName);
  if mRef=NIL then
  mRef:=Add(aName,TBRFieldCurrency);
  TBRFieldCurrency(mRef).Value:=Value;
end;

procedure TBRCustomRecord.WriteData(const aName:String;const Value:TStream);
var
  mRef: TBRRecordField;
Begin
  mRef:=ObjectOf(aName);
  if mRef=NIL then
  mRef:=Add(aName,TBRFieldData);
  if value<>NIL then
  TBRFieldData(mRef).LoadFromStream(value);
end;

procedure TBRCustomRecord.WriteDateTime(const aName:String;
          const Value:TDateTime);
var
  mRef: TBRRecordField;
Begin
  mRef:=ObjectOf(aName);
  if mRef=NIL then
  mRef:=Add(aName,TBRFieldDateTime);
  TBRFieldDateTime(mRef).Value:=Value;
end;

procedure TBRCustomRecord.WriteDouble(const aName:String;
          const Value:Double);
var
  mRef: TBRRecordField;
Begin
  mRef:=ObjectOf(aName);
  if mRef=NIL then
  mRef:=Add(aName,TBRFieldDouble);
  TBRFieldDouble(mRef).Value:=Value;
end;

Procedure TBRCustomRecord.WriteGUID(const aName:String;
          const Value:TGUID);
var
  mRef: TBRRecordField;
Begin
  mRef:=ObjectOf(aName);
  if mRef=NIL then
  mRef:=Add(aName,TBRFieldGUID);
  TBRFieldGUID(mRef).Value:=Value;
end;

Procedure TBRCustomRecord.WriteInt64(const aName:String;
          const Value:Int64);
var
  mRef: TBRRecordField;
Begin
  mRef:=ObjectOf(aName);
  if mRef=NIL then
  mRef:=Add(aName,TBRFieldInt64);
  TBRFieldInt64(mRef).Value:=Value;
end;

Procedure TBRCustomRecord.WriteLong(const aName:String;
          const Value:Longword);
var
  mRef: TBRRecordField;
Begin
  mRef:=ObjectOf(aName);
  if mRef=NIL then
  mRef:=Add(aName,TBRFieldLong);
  TBRFieldLong(mRef).Value:=Value;
end;

Function TBRCustomRecord.AddInteger(const aName:String):TBRFieldInteger;
Begin
  result:=TBRFieldInteger(Add(aName,TBRFieldInteger));
end;

Function TBRCustomRecord.AddStr(const aName:String):TBRFieldString;
Begin
  result:=TBRFieldString(Add(aName,TBRFieldString));
end;

Function TBRCustomRecord.AddByte(const aName:String):TBRFieldByte;
Begin
  result:=TBRFieldByte(Add(aName,TBRFieldByte));
end;

Function TBRCustomRecord.AddBool(const aName:String):TBRFieldBoolean;
Begin
  result:=TBRFieldBoolean(Add(aName,TBRFieldBoolean));
end;

Function TBRCustomRecord.AddCurrency(const aName:String):TBRFieldCurrency;
Begin
  result:=TBRFieldCurrency(Add(aName,TBRFieldCurrency));
end;

Function TBRCustomRecord.AddData(const aName:String):TBRFieldData;
Begin
  result:=TBRFieldData(Add(aName,TBRFieldData));
end;

Function TBRCustomRecord.AddDateTime(const aName:String):TBRFieldDateTime;
Begin
  result:=TBRFieldDateTime(Add(aName,TBRFieldDateTime));
end;

Function TBRCustomRecord.AddDouble(const aName:String):TBRFieldDouble;
Begin
  result:=TBRFieldDouble(Add(aName,TBRFieldDouble));
end;

Function TBRCustomRecord.AddGUID(const aName:String):TBRFieldGUID;
Begin
  result:=TBRFieldGUID(Add(aName,TBRFieldGUID));
end;

Function TBRCustomRecord.AddInt64(const aName:String):TBRFieldInt64;
Begin
  result:=TBRFieldInt64(Add(aName,TBRFieldInt64));
end;

Function TBRCustomRecord.AddLong(const aName:String):TBRFieldLong;
Begin
  result:=TBRFieldLong(Add(aName,TBRFieldLong));
end;

Function TBRCustomRecord.Add(const aName:String;
         Const aFieldClass:TBRRecordFieldClass):TBRRecordField;
Begin
  result:=ObjectOf(aName);
  if result=NIL then
  Begin
    if aFieldClass<>NIL then
    Begin
      Result:=aFieldClass.Create;
      Result.FieldName:=aName;
      FObjects.Add(result);
    end else
    result:=NIL;
  end;
end;

Function TBRCustomRecord.GetCount:Integer;
Begin
  result:=FObjects.Count;
end;

Function TBRCustomRecord.GetItem(const Index:Integer):TBRRecordField;
Begin
  result:=TBRRecordField(FObjects[index]);
end;

Procedure TBRCustomRecord.SetItem(const Index:Integer;
          const value:TBRRecordField);
Begin
  TBRRecordField(FObjects[index]).Assign(Value);
end;

Function TBRCustomRecord.GetField(const AName:String):TBRRecordField;
Begin
  result:=ObjectOf(aName);
end;

Procedure TBRCustomRecord.SetField(const AName:String;
          const Value:TBRRecordField);
var
  FItem: TBRRecordField;
Begin
  FItem:=ObjectOf(aName);
  If FItem<>NIL then
  FItem.assign(Value);
end;

Function TBRCustomRecord.IndexOf(const aName:String):Integer;
var
  x:  integer;
Begin
  result:=-1;
  if length(aName)>0 then
  Begin
    for x:=0 to FObjects.Count-1 do
    Begin
      if sameText(GetItem(x).FieldName,aName) then
      Begin
        result:=x;
        Break;
      end;
    end;
  end;
end;

Function TBRCustomRecord.ObjectOf(const aName:String):TBRRecordField;
var
  x:      integer;
  FItem:  TBRRecordField;
Begin
  result:=NIL;
  if length(aName)>0 then
  Begin
    for x:=0 to FObjects.Count-1 do
    Begin
      FItem:=GetItem(x);
      if sameText(FItem.FieldName,aName) then
      Begin
        result:=FItem;
        Break;
      end;
    end;
  end;
end;

//##########################################################################
// TBRFieldLong
//##########################################################################

Function TBRFieldLong.asString:String;
Begin
  Result:=IntToStr(Value);
end;

Function TBRFieldLong.GetDisplayName:String;
Begin
  Result:='Longword';
end;

Function TBRFieldLong.GetValue:Longword;
Begin
  If not Empty then
  Begin
    If Read(0,SizeOf(Result),Result)<SizeOf(Result) then
    Raise EBRRecordFieldError.CreateFmt
    (ERR_RECORDFIELD_FailedGet,[FieldName]) else
    SignalRead;
  end else
  Raise EBRRecordFieldError.CreateFmt
  (ERR_RECORDFIELD_FieldIsEmpty,[FieldName]);
end;

Procedure TBRFieldLong.SetValue(Const NewValue:Longword);
Begin
  If Write(0,SizeOf(NewValue),NewValue)<SizeOf(NewValue) then
  Raise EBRRecordFieldError.CreateFmt
  (ERR_RECORDFIELD_FailedSet,[FieldName]) else
  SignalWrite;
end;

//##########################################################################
// TSRLFieldInt64
//##########################################################################

Function TBRFieldInt64.asString:String;
Begin
  Result:=IntToStr(Value);
end;

Function TBRFieldInt64.GetDisplayName:String;
Begin
  Result:='Int64';
end;

Function TBRFieldInt64.GetValue:Int64;
Begin
  If not Empty then
  Begin
    If Read(0,SizeOf(Result),Result)<SizeOf(Result) then
    Raise EBRRecordFieldError.CreateFmt
    (ERR_RECORDFIELD_FailedGet,[FieldName]) else
    SignalRead;
  end else
  Raise EBRRecordFieldError.CreateFmt
  (ERR_RECORDFIELD_FieldIsEmpty,[FieldName]);
end;

Procedure TBRFieldInt64.SetValue(Const NewValue:Int64);
Begin
  If Write(0,SizeOf(NewValue),NewValue)<SizeOf(NewValue) then
  Raise EBRRecordFieldError.CreateFmt
  (ERR_RECORDFIELD_FailedSet,[FieldName]) else
  SignalWrite;
end;

//##########################################################################
// TBRFieldInteger
//##########################################################################

Function TBRFieldInteger.asString:String;
Begin
  Result:=IntToStr(Value);
end;

Function TBRFieldInteger.GetDisplayName:String;
Begin
  Result:='Integer';
end;

Function TBRFieldInteger.GetValue:Integer;
Begin
  If not Empty then
  Begin
    If Read(0,SizeOf(Result),Result)<SizeOf(Result) then
    Raise EBRRecordFieldError.CreateFmt
    (ERR_RECORDFIELD_FailedGet,[FieldName]) else
    SignalRead;
  end else
  Raise EBRRecordFieldError.CreateFmt
  (ERR_RECORDFIELD_FieldIsEmpty,[FieldName]);
end;

Procedure TBRFieldInteger.SetValue(Const NewValue:Integer);
Begin
  If Write(0,SizeOf(NewValue),NewValue)<SizeOf(NewValue) then
  Raise EBRRecordFieldError.CreateFmt
  (ERR_RECORDFIELD_FailedSet,[FieldName]) else
  SignalWrite;
end;

//##########################################################################
// TBRFieldGUID
//##########################################################################

Function TBRFieldGUID.asString:String;
Begin
  Result:=String(BRGUIDToStr(Value));
end;

Function TBRFieldGUID.GetDisplayName:String;
Begin
  Result:='GUID';
end;

Function TBRFieldGUID.GetValue:TGUID;
Begin
  If not Empty then
  Begin
    If Read(0,SizeOf(Result),Result)<SizeOf(Result) then
    Raise EBRRecordFieldError.CreateFmt
    (ERR_RECORDFIELD_FailedGet,[FieldName]) else
    SignalRead;
  end else
  Raise EBRRecordFieldError.CreateFmt
  (ERR_RECORDFIELD_FieldIsEmpty,[FieldName]);
end;

Procedure TBRFieldGUID.SetValue(Const NewValue:TGUID);
Begin
  If Write(0,SizeOf(NewValue),NewValue)<SizeOf(NewValue) then
  Raise EBRRecordFieldError.CreateFmt
  (ERR_RECORDFIELD_FailedSet,[FieldName]) else
  SignalWrite;
end;

//##########################################################################
// TBRFieldDateTime
//##########################################################################

Function TBRFieldDateTime.asString:String;
Begin
  Result:=DateTimeToStr(Value);
end;

Function TBRFieldDateTime.GetDisplayName:String;
Begin
  Result:='DateTime';
end;

Function TBRFieldDateTime.GetValue:TDateTime;
Begin
  If not Empty then
  Begin
    If Read(0,SizeOf(Result),Result)<SizeOf(Result) then
    Raise EBRRecordFieldError.CreateFmt
    (ERR_RECORDFIELD_FailedGet,[FieldName]) else
    SignalRead;
  end else
  Raise EBRRecordFieldError.CreateFmt
  (ERR_RECORDFIELD_FieldIsEmpty,[FieldName]);
end;

Procedure TBRFieldDateTime.SetValue(Const NewValue:TDateTime);
Begin
  If Write(0,SizeOf(NewValue),NewValue)<SizeOf(NewValue) then
  Raise EBRRecordFieldError.CreateFmt
  (ERR_RECORDFIELD_FailedSet,[FieldName]) else
  SignalWrite;
end;

//##########################################################################
// TBRFieldDouble
//##########################################################################

Function TBRFieldDouble.asString:String;
Begin
  Result:=FloatToStr(Value);
end;

Function TBRFieldDouble.GetDisplayName:String;
Begin
  Result:='Double';
end;

Function TBRFieldDouble.GetValue:Double;
Begin
  If not Empty then
  Begin
    If Read(0,SizeOf(Result),Result)<SizeOf(Result) then
    Raise EBRRecordFieldError.CreateFmt
    (ERR_RecordField_FailedGet,[FieldName]) else
    SignalRead;
  end else
  Raise EBRRecordFieldError.CreateFmt
  (ERR_RECORDFIELD_FieldIsEmpty,[FieldName]);
end;

Procedure TBRFieldDouble.SetValue(Const NewValue:Double);
Begin
  If Write(0,SizeOf(NewValue),NewValue)<SizeOf(NewValue) then
  Raise EBRRecordFieldError.CreateFmt
  (ERR_RECORDFIELD_FailedSet,[FieldName]) else
  SignalWrite;
end;

//##########################################################################
// TBRFieldData
//##########################################################################

Function TBRFieldData.asString:String;
Begin
  Result:='[Binary]';
end;

Function TBRFieldData.GetDisplayName:String;
Begin
  Result:='Binary';
end;

//##########################################################################
// TBRFieldCurrency
//##########################################################################

Function TBRFieldCurrency.asString:String;
Begin
  Result:=CurrToStr(Value);
end;

Function TBRFieldCurrency.GetDisplayName:String;
Begin
  Result:='Currency';
end;

Function TBRFieldCurrency.GetValue:Currency;
Begin
  If not Empty then
  Begin
    If Read(0,SizeOf(Result),Result)<SizeOf(Result) then
    Raise EBRRecordFieldError.CreateFmt
    (ERR_RECORDFIELD_FailedGet,[FieldName]) else
    SignalRead;
  end else
  Raise EBRRecordFieldError.CreateFmt
  (ERR_RECORDFIELD_FieldIsEmpty,[FieldName]);
end;

Procedure TBRFieldCurrency.SetValue(Const NewValue:Currency);
Begin
  If Write(0,SizeOf(NewValue),NewValue)<SizeOf(NewValue) then
  Raise EBRRecordFieldError.CreateFmt
  (ERR_RECORDFIELD_FailedSet,[FieldName]) else
  SignalWrite;
end;

//##########################################################################
// TBRFieldByte
//##########################################################################

Function TBRFieldByte.asString:String;
Begin
  Result:=IntToStr(Value);
end;

Function TBRFieldByte.GetDisplayName:String;
Begin
  Result:='Byte';
end;

Function TBRFieldByte.GetValue:Byte;
Begin
  If not Empty then
  Begin
    If Read(0,SizeOf(Result),Result)<SizeOf(Result) then
    Raise EBRRecordFieldError.CreateFmt
    (ERR_RECORDFIELD_FailedGet,[FieldName]) else
    SignalRead;
  end else
  Raise EBRRecordFieldError.CreateFmt
  (ERR_RECORDFIELD_FieldIsEmpty,[FieldName]);
end;

Procedure TBRFieldByte.SetValue(Const NewValue:Byte);
Begin
  If Write(0,SizeOf(NewValue),NewValue)<SizeOf(NewValue) then
  Raise EBRRecordFieldError.CreateFmt
  (ERR_RECORDFIELD_FailedSet,[FieldName]) else
  SignalWrite;
end;

//##########################################################################
// TBRFieldBoolean
//##########################################################################

Function TBRFieldBoolean.asString:String;
Begin
  Result:=BoolToStr(Value,True);
end;

Function TBRFieldBoolean.GetDisplayName:String;
Begin
  Result:='Boolean';
end;

Function TBRFieldBoolean.GetValue:Boolean;
Begin
  If not Empty then
  Begin
    If Read(0,SizeOf(Result),Result)<SizeOf(Result) then
    Raise EBRRecordFieldError.CreateFmt
    (ERR_RECORDFIELD_FailedGet,[FieldName]) else
    SignalRead;
  end else
  Raise EBRRecordFieldError.CreateFmt
  (ERR_RECORDFIELD_FieldIsEmpty,[FieldName]);
end;

Procedure TBRFieldBoolean.SetValue(Const NewValue:Boolean);
Begin
  If Write(0,SizeOf(NewValue),NewValue)<SizeOf(NewValue) then
  Raise EBRRecordFieldError.CreateFmt
  (ERR_RECORDFIELD_FailedSet,[FieldName]) else
  SignalWrite;
end;

//##########################################################################
// TBRFieldString
//##########################################################################

Constructor TBRFieldString.Create;
Begin
  inherited;
  FLength:=0;
  FExplicit:=False;
end;

Function TBRFieldString.asString:String;
Begin
  Result:=Value;
end;

Function TBRFieldString.GetDisplayName:String;
Begin
  Result:='String';
end;

Procedure TBRFieldString.SetFieldLength(Value:Integer);
Begin
  If  FExplicit
  and (Value<>FLength) then
  Begin
    Value:=EnsureRange(Value,0,MAXINT-1);
    If Value>0 then
    Begin
      FLength:=Value;
      If FLength<>Size then
      Size:=FLength;
    end else
    Begin
      FLength:=0;
      Release;
    end;
  end;
end;

Function TBRFieldString.GetValue:String;
Begin
  If not Empty then
  Begin
    SetLength(Result,Size);
    If Read(0,Size,pointer(@Result[1])^)<Size then
    Raise EBRRecordFieldError.CreateFmt
    (ERR_RECORDFIELD_FailedGet,[FieldName]) else
    SignalRead;
  end else
  Result:='';
end;

Procedure TBRFieldString.SetValue(NewValue:String);
var
  FLen: Integer;
Begin
  FLen:=system.Length(NewValue);
  If FLen>0 then
  Begin
    (* cut string to length if explicit *)
    If FExplicit then
    Begin
      If FLen>FLength then
      FLen:=FLength;
    end else
    Size:=FLen * SizeOf(char);

    If FLen>0 then
    Begin
      If Write(0,Size,NewValue[1])<Size then
      Raise EBRRecordFieldError.CreateFmt
      (ERR_RECORDFIELD_FailedSet,[FieldName]) else
      SignalWrite;
    end else
    Release;

  end else
  Release;
end;

//##########################################################################
// TBRRecordField
//##########################################################################

Function TBRRecordField.GetDisplayName:String;
Begin
  Result:='Unknown';
end;

Procedure TBRRecordField.SignalWrite;
Begin
end;

Procedure TBRRecordField.SignalRead;
Begin
end;

Procedure TBRRecordField.SignalRelease;
Begin
end;

Procedure TBRRecordField.SetName(Value:String);
Begin
  If Value<>FName then
  Begin
    Value:=trim(Value);
    If system.Length(Value)>0 then
    Begin
      FName:=Value;
      FNameHash:=ElfHash(LowerCase(Value))
    end else
    Raise EBRRecordFieldError.CreateFmt
    (ERR_RECORDFIELD_INVALIDNAME,[Value]);
  end;
end;

Procedure TBRRecordField.BeforeReadObject;
Begin
  inherited;
  FName:='';
  FNameHash:=0;
end;

Procedure TBRRecordField.ReadObject(const Reader:TBRReader);
Begin
  inherited;
  FNameHash:=Reader.ReadInt64;
  FName:=Reader.ReadString;
end;

Procedure TBRRecordField.WriteObject(const Writer:TBRWriter);
Begin
  inherited;
  Writer.WriteInt64(FNameHash);
  Writer.WriteString(FName);
end;

Procedure TBRRecordField.DoReleaseData;
Begin
  inherited;
  SignalRelease;
end;


  //##########################################################################
  // TBRPersistent
  //##########################################################################

  Constructor TBRPersistent.Create;
  Begin
    inherited;
    FObjId:=ClassIdentifier;
  end;

  function TBRPersistent.ObjectHasData:Boolean;
  Begin
    result:=False;
  end;

  procedure TBRPersistent.DefineProperties(Filer: TFiler);
  begin
    inherited;
    filer.DefineBinaryProperty('$RES',ReadObjBin,WriteObjBin,ObjectHasData);
  end;

  procedure TBRPersistent.ReadObjBin(Stream: TStream);
  var
    mReader:  TBRReaderStream;
  Begin
    mReader:=TBRReaderStream.Create(Stream);
    try
      ObjectFrom(mReader);
    finally
      mReader.Free;
    end;
  end;

  procedure TBRPersistent.WriteObjBin(Stream: TStream);
  var
    mWriter:  TBRWriterStream;
  begin
    mWriter:=TBRWriterStream.Create(Stream);
    try
      ObjectTo(mWriter);
    finally
      mWriter.Free;
    end;
  end;

  Function TBRPersistent.ObjectIdentifier:Longword;
  Begin
    result:=FObjId;
  end;

  Class Function TBRPersistent.ClassIdentifier:Longword;
  var
    mData:  String;
  Begin
    mData:=ObjectPath;
    result:=BobJenkinsHash(mData[1],length(mData) * SizeOf(Char),$BABE);
  end;

  (* ISRLPersistent: ObjectToStream *)
  Procedure TBRPersistent.ObjectToStream(Const Stream:TStream);
  var
    FWriter: TBRWriterStream;
  Begin
    FWriter:=TBRWriterStream.Create(Stream);
    try
      ObjectTo(FWriter);
    finally
      FWriter.free;
    end;
  end;

  (* ISRLPersistent: ObjectToBinary *)
  Procedure TBRPersistent.ObjectToData(Const Binary:TBRBuffer);
  var
    FWriter: TBRWriterBuffer;
  Begin
    FWriter:=TBRWriterBuffer.Create(Binary);
    try
      ObjectTo(FWriter);
    finally
      FWriter.free;
    end;
  end;

  (* ISRLPersistent: ObjectToBinary *)
  Function TBRPersistent.ObjectToData:TBRBuffer;
  Begin
    Result:=TBRBufferMemory.Create;
    try
      ObjectToData(Result);
    except
      on e: exception do
      Begin
        FreeAndNil(Result);
        Raise EBRPersistent.Create(e.Message);
      end;
    end;
  end;

  (* ISRLPersistent: ObjectToStream *)
  Function TBRPersistent.ObjectToStream:TStream;
  Begin
    Result:=TMemoryStream.Create;
    try
      ObjectToStream(Result);
      Result.Position:=0;
    except
      on e: exception do
      Begin
        FreeAndNil(Result);
        Raise EBRPersistent.Create(e.message);
      end;
    end;
  end;

  Procedure TBRPersistent.ObjectFrom(Const Reader:TBRReader);
  Begin
    If Reader<>NIL then
    Begin
      If BeginUpdate then
      Begin
        BeforeReadObject;
        ReadObject(Reader);
        AfterReadObject;
        EndUpdate;
      end;
    end else
    Raise EBRPersistent.Create(ERR_BR_PERSISTENCY_INVALIDREADER);
  end;

  Procedure TBRPersistent.ObjectTo(Const Writer:TBRWriter);
  Begin
    If Writer<>NIl then
    Begin
      BeforeWriteObject;
      WriteObject(Writer);
      AfterWriteObject;
    end else
    Raise EBRPersistent.Create(ERR_BR_PERSISTENCY_INVALIDWRITER);
  end;

  (* ISRLPersistent: ObjectFromBinary *)
  Procedure TBRPersistent.ObjectFromData
            (Const Binary:TBRBuffer;Const Disposable:Boolean);
  var
    FReader: TBRReaderBuffer;
  Begin
    FReader:=TBRReaderBuffer.Create(Binary);
    try
      ObjectFrom(FReader);
    finally
      FReader.free;
      If Disposable then
      Binary.free;
    end;
  end;

  procedure TBRPersistent.Assign(Source:TPersistent);
  Begin
    If Source<>NIL then
    Begin
      if (source is TBRPersistent) then
      Begin
        (* Always supports object of same class *)
        if IBRObject(TBRPersistent(Source)).GetObjectClass=GetObjectClass then
        Begin
          If Source<>Self then
          ObjectFromData(TBRPersistent(Source).ObjectToData,True);
        end else
        (* no support, raise exception *)
        Raise EBRPersistent.CreateFmt
        (ERR_BR_PERSISTENCY_ASSIGNCONFLICT,
        [TBRPersistent(Source).ObjectPath,ObjectPath]);
      end;
    end;
  end;

  Procedure TBRPersistent.BeforeWriteObject;
  Begin
    AddObjectState([osReadWrite]);
  end;

  Procedure TBRPersistent.BeforeReadObject;
  Begin
    AddObjectState([osReadWrite]);
  end;

  Procedure TBRPersistent.AfterReadObject;
  Begin
    RemoveObjectState([osReadWrite]);
  end;

  procedure TBRPersistent.AfterWriteObject;
  Begin
    RemoveObjectState([osReadWrite]);
  end;

  Procedure TBRPersistent.WriteObject(Const Writer:TBRWriter);
  Begin
    (* write identifier to stream *)
    Writer.WriteLong(FObjId);
  end;

  Procedure TBRPersistent.ReadObject(Const Reader:TBRReader);
  var
    FReadId:  Longword;
  Begin
    (* read identifier from stream *)
    FReadId:=Reader.ReadLong;

    If FReadId<>FObjId then
    Raise EBRPersistent.CreateFmt
    (ERR_BR_PERSISTENCY_INVALIDSIGNATURE,
    [IntToHex(FReadId,8),IntToHex(FObjId,8)]);
  end;

  (* ISRLPersistent: ObjectFromStream *)
  Procedure TBRPersistent.ObjectFromStream
            (Const Stream:TStream;Const Disposable:Boolean);
  var
    FReader:  TBRReaderStream;
  Begin
    FReader:=TBRReaderStream.Create(Stream);
    try
      ObjectFrom(FReader);
    finally
      FReader.free;
      If Disposable then
      Stream.free;
    end;
  end;

  (* ISRLPersistent: ObjectfromFile *)
  Procedure TBRPersistent.ObjectfromFile(Const Filename:String);
  Begin
    ObjectFromStream(TFileStream.Create(filename,
    fmOpenRead or fmShareDenyWrite),True);
  end;

  (* ISRLPersistent: ObjectToFile *)
  Procedure TBRPersistent.ObjectToFile(Const Filename:String);
  var
    FFile:  TFileStream;
  Begin
    FFile:=TFileStream.Create(filename,fmCreate);
    try
      ObjectToStream(FFile);
    finally
      FFile.free;
    end;
  end;

  Procedure TBRPersistent.BeforeUpdate;
  Begin
  end;

  procedure TBRPersistent.AfterUpdate;
  Begin
  end;

  Function TBRPersistent.BeginUpdate:Boolean;
  Begin
    result:=QueryObjectState([osDestroying])=False;
    if result then
    Begin
      inc(FUpdCount);
      If FUpdCount=1 then
      Begin
        AddObjectState([osUpdating]);
        BeforeUpdate;
      end;
    end;
  end;

  procedure TBRPersistent.EndUpdate;
  Begin
    If QueryObjectState([osUpdating]) then
    Begin
      dec(FUpdCount);
      If FUpdCount<1 then
      Begin
        RemoveObjectState([osUpdating]);
        AfterUpdate;
      end;
    end;
  end;

  //##########################################################################
  // TBRObject
  //##########################################################################

  Constructor TBRObject.Create;
  Begin
    inherited;
    FState:=[osCreating];
  end;

  Procedure TBRObject.AfterConstruction;
  Begin
    inherited;
    FState:=FState - [osCreating];
  end;

  Procedure TBRObject.BeforeDestruction;
  Begin
    FState:=FState + [osDestroying];
    inherited;
  end;

  Function TBRObject.GetObjectClass:TBRObjectClass;
  Begin
    Result:=TBRObjectClass(ClassType);
  end;

  Class Function TBRObject.ObjectPath:String;
  var
    FAncestor:  TClass;
  Begin
    SetLength(result,0);
    FAncestor:=ClassParent;
    While FAncestor<>NIL do
    Begin
      If Length(Result)>0 then
      Result:=(FAncestor.ClassName + '.' + Result) else
      Result:=FAncestor.ClassName;
      FAncestor:=FAncestor.ClassParent;
    end;
    If Length(Result)>0 then
    result:=result + '.' + ClassName else
    result:=ClassName;
  end;

  Function TBRObject.GetParent:TObject;
  Begin
    result:=FParent;
  end;

  Procedure TBRObject.SetParent(Const Value:TObject);
  Begin
    FParent:=Value;
  end;

  Function TBRObject.GetObjectState:TBRObjectState;
  Begin
    result:=FState;
  end;

  Procedure TBRObject.AddObjectState(Const Value:TBRObjectState);
  Begin
    FState:=FState + Value;
  end;

  Procedure TBRObject.RemoveObjectState(Const Value:TBRObjectState);
  Begin
    FState:=FState - Value;
  end;

  Procedure TBRObject.SetObjectState(Const Value:TBRObjectState);
  Begin
    FState:=Value;
  end;

  Function TBRObject.QueryObjectState(Const Value:TBRObjectState):Boolean;
  Begin
    If (osCreating in Value) then
    Result:=(osCreating in FState) else
    Result:=False;

    If (Result=False) and (osDestroying in Value) then
    Begin
      If (osDestroying in FState) then
      Result:=True;
    end;

    If (Result=False) and (osUpdating in Value) then
    Begin
      if (osUpdating in FState) then
      Result:=True;
    end;

    If (Result=False) and (osReadWrite in Value) then
    Begin
      if (osReadWrite in FState) then
      Result:=True;
    end;

    If (Result=False) and (osSilent in Value) then
    Result:=(osSilent in FState);
  end;

  function TBRObject.QueryInterface(const IID:TGUID;out Obj):HResult;stdcall;
  Begin
    If GetInterface(IID,Obj) then
    Result:=S_OK else
    Result:=E_NOINTERFACE;
  end;

  function TBRObject._AddRef:Integer;stdcall;
  Begin
    (* Inform COM that no reference counting is required *)
    Result:=-1;
  end;

  function TBRObject._Release:Integer;stdcall;
  Begin
    (* Inform COM that no reference counting is required *)
    Result:=-1;
  end;

//############################################################################
// TBRWriterMemory
//############################################################################

constructor TBRWriterMemory.Create(const aMemory: PByte; aDataLen: Integer);
begin
  Inherited Create;
  FMemory:=aMemory;
  FSize:=aDataLen;
  if FMemory=NIL then
  Raise Exception.Create('Invalid memory pointer [NIL] error');
end;

function TBRWriterMemory.Write(const Data; DataLen: Integer): Integer;
var
  mTarget:  PByte;
  mToMove:  Integer;
begin
  mTarget:=FMemory;
  inc(mTarget,FOffset);
  mToMove:=EnsureRange(DataLen,0, FSize - FOffset);
  result:=mToMove;
  if mToMove>0 then
  Begin
    move(Data,mTarget^,mToMove);
    inc(FOffset,mToMove);
  end else
  if (DataLen>0) then
  Raise Exception.Create('Failed to write to memory, size exceeds allowed range error');
end;

Initialization
Begin
  BRRegisterRecordField(TBRFieldBoolean);
  BRRegisterRecordField(TBRFieldByte);
  BRRegisterRecordField(TBRFieldCurrency);
  BRRegisterRecordField(TBRFieldData);
  BRRegisterRecordField(TBRFieldDateTime);
  BRRegisterRecordField(TBRFieldDouble);
  BRRegisterRecordField(TBRFieldGUID);
  BRRegisterRecordField(TBRFieldInt64);
  BRRegisterRecordField(TBRFieldInteger);
  BRRegisterRecordField(TBRFieldLong);
  BRRegisterRecordField(TBRFieldString);
end;

Finalization
SetLength(_FieldClasses,0);

end.
