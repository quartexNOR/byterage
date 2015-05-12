Byterage is an object pascal class library designed to remove some of the limitations of streams. The framework is very simple to use, with only one common ancestor class (TBRBuffer) which defines a set of storage agnostic mechanisms for allocating, scaling, inserting, deleting and otherwise manipulating a segment of raw binary data.

In other words, it abstracts you from:

  1. where data is stored
  1. how data is stored
  1. storage medium IO rules (ftp vs. fat32)
  1. shrink mechanism
  1. growth mechanism
  1. search mechanism

Inheriting from this ancestor are classes that target spesific storage mediums, primarily memory (TBRBufferMemory) and the filesytem (TBRBufferFile). The benefit of this approach is naturally, that if your program doesnt have to worry about where the data is stored or how the data is read or writtem. So once you have written code to manage your raw data - working with a memory segment, a file or even remote files is just a matter of creating the correct buffer. If it is a memory segment, a local file, a remote file via ftp, a virtual filesystem for a hardware device -- the same API will work on all of these if there is a buffer implemented for it.

Besides the buffer classes, byterage also comes with record and field classes.
These can in some way resemble "normal records" in Delphi or FPC, but they have the benefit of being objective, dynamic, streamable and persistent. Also (and this is what they were designed for) they are written to a buffer without padding - meaning that they will only occupy their original size. By default the datasize and field-names are also written to the target, but that can be switched off (explicit mode) so that the data is written verbatim.

Example of a pseudo datastructure:

```
Type
PSomWinApiDataStruct = ^TSomWinAPIDataStruct;
TSomWinAPIDataStruct = Packed Record
  wnId: Longword;
  wnProvider: AnsiChar[128];
  wnDevId: TAPIByteArray_4;
  wnEntry: pointer;
End;
```

With byterage you can now target such "native" datastructures using OOP. While this approach is (naturally) slower than a clean pointer operation, it opens up for editors and binary datatype signatures (which is an essenstial part of hex-editors). There is still a lot of commercial software that have binary, custom fileformats as opposed to XML.

To bridge the final gap between the VCL/FCL streams, I have also provided a stream adapter class. This will allow you to consume a byterage buffer as a stream, and consequently copy, read and write to or from the buffer via the TStream interface.

```
var mAdapter: TBRStreamAdapter;
mAdapter:=TBRStreamAdapter.Create(myBuffer);
try
  mPDFView.LoadFromStream(mAdapter);
finally
  mAdapter.free;
end;
```

**Universal and unbound by memory mapping**

Byterage does not use assembler or OS "file mapping" functions for moving large segments of data, rather it is object pascal native. It makes full use of loop expansion to gain very high data transfer rates. It also use int64 for data length to avoid problems with large files. Since it's written in vanilla object pascal (Delphi 7, pre generics dialect) it should also be compatible with freepascal / Lazarus and modern Delphi compilers (tested on Delphi XE2 without problems).

**Practical use**

Byterage is a low-level library so naturally it wont be suitable for everyone. Having said that, it represents a huge advantage over ordinary streams. It is also pure, native object pascal. Portable. Interfaces directly with object pascal streams. There really are no drawbacks. It was designed to be "future proof" and follows the object pascal guidelines to the letter.

Here is an example of how to inject a piece of data into a buffer. The buffer could be a file or a memory object, and the data could be 10 bytes or 10 gigabytes. Byterage makes it very simple to work with binary, raw data:

```
procedure TForm1.Button2Click(Sender: TObject);
var
  mBuffer:  TBRBufferMemory;
  mText:  String;
  x:  Integer;
  mLen: Integer;
begin
  mBuffer:=TBRBufferMemory.Create;
  try
    // Build our initial buffer content
    mText:='This is a row of data' + #10;
    mLen:=Length(mText);

    // Add X # of rows with same data 
    for x:=0 to 10 do
    mBuffer.Append(mText[1],mLen * SizeOf(char));

    // Insert a row of data in the middle, we now have X+1 rows
    mText:='Some text i want inserted in the middle' + #13;
    mBuffer.Insert( (mLen * SizeOf(char) ) * 5,
    mText[1],length(mText) * SizeOf(Char));

    // Read the data into a string and display the results
    setlength(mText,mBuffer.Size div SizeOf(Char));
    mBuffer.Read(0,mBuffer.Size,mText[1]);

    showmessage(mText);
  finally
    mBuffer.Free;
  end;
end;
```


**Latest additions**

Two new classes has been introduced: TBRObject and TBRPersistent. These represent a completely Delphi/FPC compatible persistence framework, but with the added bonus of taking advantage of byterage's easy to use reader/writer mechanisms.
This mechanism makes it even more easy to add ORM functionality to a DB engine written using byterage.