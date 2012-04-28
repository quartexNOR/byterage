unit mainform;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, brage;

type

TTestData = packed Record
  tdText:   ShortString;
  tdFloat:  Double;
  tdSingle: Single;
End;


  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    log: TListBox;
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    Procedure Write(aText:String);
    Procedure WriteFmt(aText:String;Const aValue:Array of Const);
    Procedure WriteSeperator;
    Procedure TestDataToLog(Const aValue:TTestData);
  private
    Procedure ReadWriteAdapterTest;
    Procedure AppendTest;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button3Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  log.Items.BeginUpdate;
  try
    log.Items.Clear;
  finally
    log.Items.EndUpdate;
  end;
end;

Procedure TForm1.Write(aText:String);
Begin
  log.items.BeginUpdate;
  try
    log.Items.Add(aText);
    log.ItemIndex:=log.items.Count-1;
  finally
    log.items.EndUpdate;
  end;
end;

Procedure TForm1.WriteSeperator;
Begin
  log.items.BeginUpdate;
  try
    log.Items.Add('------------------------------------------');
  finally
    log.items.EndUpdate;
  end;
end;

Procedure TForm1.WriteFmt(aText:String;Const aValue:Array of const);
Begin
  Write(Format(aText,aValue));
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ReadWriteAdapterTest;
  AppendTest;
end;

Procedure TForm1.TestDataToLog(Const aValue:TTestData);
Begin
  WriteFmt('Text = %s',[aValue.tdText]);
  WriteFmt('Float = %f',[aValue.tdFloat]);
  WriteFmt('Single = %f',[aValue.tdSingle]);
end;

Procedure TForm1.AppendTest;
var
  mBuffer:  TBRBufferMemory;
  mItem:    TTestData;
  x:  Integer;
  mOffset:  Integer;
Begin
  mBuffer:=TBRBufferMemory.Create;
  try
    Write('');
    Write('Append testing');
    WriteSeperator;

    Write('Appending 100 records');
    for x:=1 to 100 do
    Begin
      mItem.tdText:='Insert text #' + IntToStr(x);
      mItem.tdFloat:=132.32;
      mItem.tdSingle:=x;
      mBuffer.Append(mItem,SizeOf(TTestData));
    end;

    Write('Proof reading them back');
    for x:=1 to 100 do
    Begin
      mOffset:=(x-1) * SizeOf(TTestData);
      if mBuffer.Read(mOffset,SizeOf(mItem),mItem)=SizeOf(mItem) then
      Begin
        if  (mItem.tdText<>'Insert text #' + IntToStr(x) )
        or  (round(mItem.tdSingle)<>x) then
        Begin
          Write('Error in record #' + IntToStr(x));
          self.TestDataToLog(mItem);
          exit;
        end;
      end;
    end;

    Write('Result: Success');

  finally
    mBuffer.free;
  end;
end;


Procedure TForm1.ReadWriteAdapterTest;
var
  mBuffer:  TBRBufferMemory;
  mAdapter: TBRStreamAdapter;
  mWriter:  TWriter;
  mReader:  TReader;
  mSrcData: TTestData;
  mDstData: TTestData;
Begin
  Write('');
  Write('Read/Write test with stream-adapter');
  WriteSeperator;


  try

    mBuffer:=TBRBufferMemory.Create;
    try

      mAdapter:=TBRStreamAdapter.Create(mBuffer);
      try

        mSrcData.tdText:='This is a string test';
        mSrcData.tdFloat:=12.93;
        mSrcData.tdSingle:=1.0;

        mWriter:=TWriter.Create(mAdapter,1024);
        try
          (* write data *)
          mWriter.WriteString(mSrcData.tdText);
          mWriter.WriteFloat(mSrcData.tdFloat);
          mWriter.WriteSingle(mSrcData.tdSingle);
          Write('Data written:');
          TestDataToLog(mSrcData);
        finally
          mWriter.FlushBuffer;
          mWriter.free;
        end;

        mAdapter.Position:=0;
        Write('');

        mReader:=TReader.Create(mAdapter,1024);
        try
          (* read data back *)
          mDstData.tdText:=mReader.ReadString;
          mDstData.tdFloat:=mReader.ReadFloat;
          mDstData.tdSingle:=mReader.ReadSingle;
          Write('Data read:');
          TestDataToLog(mSrcData);
        finally
          mReader.free;
        end;

        (* compare data *)
        if (mDstData.tdText<>mSrcData.tdText)
        or (mDstData.tdFloat<>mSrcData.tdFloat)
        or (mDstData.tdSingle<>mSrcData.tdSingle) then
        Begin
          Write('Result: Failure!');

          exit;
        end else
        Write('Result: Success');
        
      finally
        mAdapter.free;
      end;



    finally
      mBuffer.free;
    end;


  except
    on e: exception do
    WriteFmt('**ERROR:%s',[e.message]);
  end;

end;

end.
