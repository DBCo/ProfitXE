unit deClpBrdStorage;

interface

uses {SysUtils, }
     {G2D, } DeSchema2D;

const
  DE_SCHEMA_CLIPBOARD_FORMAT : pChar = 'Profit Map';

var
  CF_PROFITSCHEMA : integer =-1;

type
  TdeClipboardStorage = class(TdeSchemaStorage)
  public
    //constructor Create;
    //destructor  Destroy;override;
    procedure   Save(aSchema:TdeSchema);override;
    procedure   Load(aSchema:TdeSchema);override;
  end;

implementation

uses Windows, Classes, ClipBrd, Graphics,
     DeLog, DeEMFStorage, deStrmStorage;

{
constructor TdeClipboardStorage.Create;
begin
  inherited Create;
end;

destructor  TdeClipboardStorage.Destroy;
begin
  inherited Destroy;
end;
}

procedure   TdeClipboardStorage.Save(aSchema:TdeSchema);
var
  Clp      : TClipboard;
  aStrm    : TdeMemoryStream;
  EMF      : TMetaFile;
  aFormat  : Word;
  aData    : THandle;
  aPalette : HPALETTE;
  aSize    : int64;
  DataPtr  : pointer;
begin
  if (aSchema = nil) or (aSchema.RootCount = 0) then exit;
  Clp := Clipboard;
  Clp.Open;
  Clp.Clear;
  try
    //"отрисовываем" временную схему в EMF и сохраняем в буфере обмена
    EMF := TMetaFile.Create;
    try
      EMF.Enhanced := true;
      with TdeEMFStorage.Create(EMF) do try
        Save(aSchema);
      finally
        Free;
      end;
      EMF.SaveToClipboardFormat(aFormat,aData,aPalette);
      Clp.SetAsHandle(aFormat,aData);
    finally
      EMF.Free;
    end;
    //сохраняем временную схему во внутреннем формате
    aStrm    := TdeMemoryStream.Create;
    try
      with TdeStreamStorage.Create(aStrm) do try
        Save(aSchema);
      finally
        Free;
      end;
      aSize := aStrm.Size;
      aData := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, aSize+1);
      DataPtr := GlobalLock(aData);
      system.Move(aStrm.Memory^,DataPtr^,aSize);
      GlobalUnlock(aData);
      Clp.SetAsHandle(CF_PROFITSCHEMA,aData);
    finally
      aStrm.Free;
    end;
  finally
    Clp.Close;
  end;
end;

procedure   TdeClipboardStorage.Load(aSchema:TdeSchema);
var
  Clp      : TClipboard;
  EMF      : TMetaFile;
  Strm     : TdeMemoryStream;
  aData    : THandle;
  aPalette : HPALETTE;
  DataPtr  : pointer;
begin
  if (aSchema = nil) then exit;
  Clp := Clipboard;
  Clp.Open;
  try
    aSchema.Lock;
    try
      if Clp.HasFormat(CF_PROFITSCHEMA) then begin
        Strm := TdeMemoryStream.Create;
        try
          aData := Clp.GetAsHandle(CF_PROFITSCHEMA);
          DataPtr := GlobalLock(aData);
          Strm.Write(DataPtr^,GlobalSize(aData));
          GlobalUnlock(aData);
          Strm.Seek(0,soFromBeginning);
          with TdeStreamStorage.Create(Strm) do try
            Load(aSchema);
          finally
            Free;
          end
        finally
          Strm.Free;
        end;
      end
      else if Clp.HasFormat(CF_ENHMETAFILE) then begin
        EMF := TMetaFile.Create;
        try
          aData    := 0;
          aPalette := 0;
          EMF.LoadFromClipboardFormat(CF_ENHMETAFILE,aData,aPalette);
          with TdeEMFStorage.Create(EMF) do try
            Load(aSchema);
          finally
            Free;
          end
        finally
          EMF.Free;
        end;
      end;
    finally
      aSchema.unLock;
    end;
  finally
    Clp.Close;
  end;
end;

{$REGION 'Startup & Shutdown Unit Runtime'}
procedure Startup;
begin
  {$IFDEF DEBUG}
  DebugLog('deClpBrdStorage unit initialization ...');
  {$ENDIF}
  CF_PROFITSCHEMA := RegisterClipboardFormat(DE_SCHEMA_CLIPBOARD_FORMAT);
end;

procedure Shutdown;
begin
  {$IFDEF DEBUG}
  DebugLog('deClpBrdStorage unit finalization ...');
  {$ENDIF}
end;
{$ENDREGION}

initialization
  Startup;

finalization
  Shutdown;

end.

