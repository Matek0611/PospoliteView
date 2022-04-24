unit Pospolite.View.DOM.Document;

{
  +-------------------------+
  | Package: Pospolite View |
  | Author: Matek0611       |
  | Email: matiowo@wp.pl    |
  | Version: 1.0p           |
  +-------------------------+

  Comments:
  ...
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Pospolite.View.Basics, Pospolite.View.CSS.Selector;

type

  { TPLHTMLDocumentQueries }

  TPLHTMLDocumentQueries = class sealed
  public
    class function querySelectorFast(const AQuery: TPLString; AObject: TPLHTMLObject;
      const AFirstOnly: TPLBool): TPLHTMLObjects;
    class function querySelector(const AQuery: TPLString; AObject: TPLHTMLObject
      ): TPLHTMLObject;
    class function querySelectorAll(const AQuery: TPLString; AObject: TPLHTMLObject
      ): TPLHTMLObjects; inline;
  end;

implementation

{ TPLHTMLDocumentQueries }

class function TPLHTMLDocumentQueries.querySelectorFast(
  const AQuery: TPLString; AObject: TPLHTMLObject; const AFirstOnly: TPLBool
  ): TPLHTMLObjects;
var
  sel: IPLCSSSelectors;
  s: TPLCSSSelector;
  i, j, id: SizeInt;
  ssp: TPLCSSSimpleSelectorPattern;
  tmp: IPLHTMLObjects;
  res: TPLBool;
  objp, obja: TPLHTMLObject;

  procedure TrySelect(a: TPLHTMLObject);
  var
    obj, applied: TPLHTMLObject;
  begin
    if not Assigned(a) then exit;
    if ssp.AppliesTo(a, applied) then tmp.Add(applied);

    for obj in a.Children do TrySelect(obj);
  end;
begin
  Result := TPLHTMLObjects.Create(false);

  if not Assigned(AObject) then exit;

  tmp := TPLHTMLObjects.Create(false);
  sel := TPLCSSSelectorParser.ParseSelector(AQuery);

  for s in sel do begin
    // parsowanie od tyłu, jak to robi przeglądarka

    tmp.Clear;
    ssp := s.SimpleSelectors.Last;
    TrySelect(AObject);

    for i := 0 to tmp.Count-1 do begin
      res := true;
      objp := tmp[i];

      for j := s.Combinators.Count-1 downto 0 do begin
        ssp := s.SimpleSelectors[j];

        case s.Combinators[j].Value of
          scDescendant: begin
            objp := objp.Parent;
            res := false;

            while Assigned(objp) do begin
              if ssp.AppliesTo(objp, obja) then begin
                res := true;
                objp := obja;
                break;
              end else objp := objp.Parent;
            end;
          end;
          scChild: begin
            objp := objp.Parent;

            if ssp.AppliesTo(objp, obja) then begin
              objp := obja;
              break;
            end else res := false;
          end;
          scAdjascentSibling: begin
            obja := objp.Parent;
            id := objp.GetIDFromParent;

            if id > 0 then begin
              id -= 1;                                  // omijanie zwykłego tekstu
              while (id >= 0) and (obja.Children[id].Name = 'internal_text_object') do id -= 1;
              res := (id >= 0) and ssp.AppliesTo(obja.Children[id], objp);
            end else res := false;
          end;
          scGeneralSibling: begin
            obja := objp.Parent;
            id := objp.GetIDFromParent;

            if id > 0 then begin
              id -= 1;
              while (id >= 0) and not ssp.AppliesTo(objp.Parent.Children[id], obja) do id -= 1;
              res := id >= 0;
              if res then objp := obja;
            end else res := false;
          end;
          scUndefined: res := false;
        end;

        if not res then break;
      end;

      if res and (Result.Find(tmp[i]) < 0) then
        Result.Add(tmp[i]);
    end;
  end;

  if AFirstOnly then
    while Result.Count > 1 do Result.Pop;
end;

class function TPLHTMLDocumentQueries.querySelector(const AQuery: TPLString;
  AObject: TPLHTMLObject): TPLHTMLObject;
var
  objs: IPLHTMLObjects;
begin                                        // false cuz we can omit while by objs.First
  objs := querySelectorFast(AQuery, AObject, false);

  if not objs.Empty then
    Result := objs.First
  else
    Result := nil;
end;

class function TPLHTMLDocumentQueries.querySelectorAll(const AQuery: TPLString;
  AObject: TPLHTMLObject): TPLHTMLObjects;
begin
  Result := querySelectorFast(AQuery, AObject, false);
end;

end.

