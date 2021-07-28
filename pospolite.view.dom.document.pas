unit Pospolite.View.DOM.Document;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Pospolite.View.Basics, Pospolite.View.CSS.Selector;

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
      ): TPLHTMLObjects; inline;
  end;

implementation

{ TPLHTMLDocumentQueries }

class function TPLHTMLDocumentQueries.querySelectorX(const AQuery: TPLString;
  AObject: TPLHTMLObject; const AFirstOnly: TPLBool): TPLHTMLObjects;
var
  list: TPLHTMLObjects absolute Result;
  sel: IPLCSSSelectors;
  s: TPLCSSSelector;
  ssp: TPLCSSSimpleSelectorPattern;
  tmp: array of IPLHTMLObjects;
  x, i: TPLInt;
  c: TPLCSSSelectorCombinatorItem;

  procedure AddObject(a: TPLHTMLObject);
  begin
    if list.Find(a) < 0 then list.Add(a);
  end;

  procedure TrySelect(a: TPLHTMLObject);
  var
    obj: TPLHTMLObject;
  begin
    if not Assigned(a) then exit;
    if ssp.AppliesTo(a) and (tmp[i].Find(a) < 0) then tmp[i].Add(a);

    for obj in a.Children do TrySelect(obj);
  end;

begin
  Result := TPLHTMLObjects.Create(false);

  if not Assigned(AObject) then exit;
  sel := TPLCSSSelectorParser.ParseSelector(AQuery);

  for s in sel do begin
    if s.SimpleSelectors.Empty then continue;
    if s.Combinators.Empty then x := 1 else x := 2 * s.SimpleSelectors.Count;
    SetLength(tmp, x);
    for i := 0 to x-1 do
      tmp[i] := TPLHTMLObjects.Create(false);
    i := 0;

    for ssp in s.SimpleSelectors do begin
      TrySelect(AObject);
      Inc(i);
    end;

    for c in s.Combinators do begin // na końcu po obróbce są te obiekty co trzeba
      case c.Value of
        scDescendant: begin
          //...
        end;
      end;
    end;

    if (x > 0) then begin
      Dec(x);

      for i := 0 to tmp[x].Count-1 do begin
        if AFirstOnly and (Result.Count > 0) then break;
        AddObject(tmp[x][i]);
      end;

      if AFirstOnly and (Result.Count > 0) then break;
    end;
  end;
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

