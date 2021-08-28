unit PospoLiteHTML.Cache;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes, PospoLiteHTML.CSS.Basics;

type

  { TPLCSSImageCacheItem }

  TPLCSSImageCacheItem = packed class(TObject)
  private
    FName: string;
    FBitmap: TBGRABitmap;
  public
    constructor Create(AName: string; ABitmap: TBGRABitmap);
    destructor Destroy; override;

    property Name: string read FName;
    property Bitmap: TBGRABitmap read FBitmap;
  end;

  TPLCSSImageCacheItems = class(specialize TPLObjectList<TPLCSSImageCacheItem>);

  { TPLCSSImagesCache }

  TPLCSSImagesCache = class(TObject)
  private
    FList: TPLCSSImageCacheItems;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(AName: string; ABitmap: TBGRABitmap);
    procedure Clear;
    function Exists(AName: string; out ABitmap: TBGRABitmap): boolean;

    property List: TPLCSSImageCacheItems read FList write FList;
  end;

var
  PLCSSImagesCache: TPLCSSImagesCache;

implementation

{ TPLCSSImageCacheItem }

constructor TPLCSSImageCacheItem.Create(AName: string; ABitmap: TBGRABitmap);
begin
  inherited Create;

  FName := AName;
  FBitmap := ABitmap;
end;

destructor TPLCSSImageCacheItem.Destroy;
begin
  FreeAndNil(FBitmap);

  inherited Destroy;
end;

{ TPLCSSImagesCache }

constructor TPLCSSImagesCache.Create;
begin
  inherited Create;

  FList := TPLCSSImageCacheItems.Create(true);
end;

destructor TPLCSSImagesCache.Destroy;
begin
  FList.Free;

  inherited Destroy;
end;

procedure TPLCSSImagesCache.Add(AName: string; ABitmap: TBGRABitmap);
var
  dummy: TBGRABitmap;
begin
  if FList.Count = MaxListSize then Clear;

  if not Exists(AName, dummy) then
    FList.Add(TPLCSSImageCacheItem.Create(AName, ABitmap));
end;

procedure TPLCSSImagesCache.Clear;
begin
  FList.Clear;
end;

function TPLCSSImagesCache.Exists(AName: string; out ABitmap: TBGRABitmap
  ): boolean;
var
  i: SizeInt;
begin
  Result := false;
  ABitmap := nil;

  for i := 0 to FList.Count-1 do
    if FList[i].Name = AName then begin
      ABitmap := FList[i].Bitmap;
      exit(true);
    end;
end;

initialization
  PLCSSImagesCache := TPLCSSImagesCache.Create;

finalization
  PLCSSImagesCache.Clear;
  PLCSSImagesCache.Free;

end.

