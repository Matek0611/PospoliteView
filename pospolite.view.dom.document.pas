unit Pospolite.View.DOM.Document;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Pospolite.View.Basics;

type

  { TPLHTMLDocumentQueries }

  TPLHTMLDocumentQueries = class sealed
  private
    class function querySelectorX(const AQuery: TPLString; AObject: TPLHTMLObject;
      const AFirstOnly: TPLBool): TPLHTMLObjects;
  public
    class function querySelector(const AQuery: TPLString; AObject: TPLHTMLObject
      ): TPLHTMLObject;
    class function querySelectorAll(const AQuery: TPLString; AObject: TPLHTMLObject
      ): TPLHTMLObjects;
  end;

implementation

{ TPLHTMLDocumentQueries }

class function TPLHTMLDocumentQueries.querySelectorX(const AQuery: TPLString;
  AObject: TPLHTMLObject; const AFirstOnly: TPLBool): TPLHTMLObjects;
begin
  Result := TPLHTMLObjects.Create(false);

  if not Assigned(AObject) then exit;


end;

class function TPLHTMLDocumentQueries.querySelector(const AQuery: TPLString;
  AObject: TPLHTMLObject): TPLHTMLObject;
var
  objs: IPLHTMLObjects;
begin
  objs := querySelectorX(AQuery, AObject, true);

  if not objs.Empty then
    Result := objs.First
  else
    Result := nil;
end;

class function TPLHTMLDocumentQueries.querySelectorAll(const AQuery: TPLString;
  AObject: TPLHTMLObject): TPLHTMLObjects;
begin
  Result := querySelectorX(AQuery, AObject, false);
end;

end.

