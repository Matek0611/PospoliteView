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
  private
    class function querySelectorX(const AQuery: TPLString; AObject: TPLHTMLObject;
      const AFirstOnly: TPLBool): TPLHTMLObjects;
  public
    class function querySelector(const AQuery: TPLString; AObject: TPLHTMLObject
      ): TPLHTMLObject;
    class function querySelectorAll(const AQuery: TPLString; AObject: TPLHTMLObject
      ): TPLHTMLObjects; inline;
    class function isQuerySelectorFor(const AObject: TPLHTMLObject; const AQuery: TPLString): TPLBool;
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
  tmpe: TPLHTMLObject;
  x, i, j, a, id1, id2: TPLInt;
  c: TPLCSSSelectorCombinatorItem;
  r, w: TPLBool;

  procedure AddObject(a: TPLHTMLObject);
  begin
    if list.Find(a) < 0 then list.Add(a);
  end;

  procedure TrySelect(a: TPLHTMLObject);
  var
    obj, applied: TPLHTMLObject;
  begin
    if not Assigned(a) then exit;
    if ssp.AppliesTo(a, applied) and (tmp[i].Find(a) < 0) then tmp[i].Add(applied);

    for obj in a.Children do TrySelect(obj);
  end;

  function FindInside(what, where: TPLHTMLObject): TPLBool;
  var
    b: SizeInt;
  begin
    if what.Parent = where then exit(true);

    Result := false;
    for b := 0 to where.Children.Count-1 do begin
      if FindInside(what, where.Children[b]) then exit(true);
    end;
  end;

begin
  Result := TPLHTMLObjects.Create(false);

  if not Assigned(AObject) then exit;
  sel := TPLCSSSelectorParser.ParseSelector(AQuery);

  for s in sel do begin
    if s.SimpleSelectors.Empty then continue;
    if s.Combinators.Empty then x := 1 else x := s.SimpleSelectors.Count;
    SetLength(tmp, x);
    for i := 0 to x-1 do
      tmp[i] := TPLHTMLObjects.Create(false);
    i := 0;

    for ssp in s.SimpleSelectors do begin
      TrySelect(AObject);
      Inc(i);
    end;

    w := true;
    i := 0;
    for c in s.Combinators do begin // na końcu po obróbce są te obiekty co trzeba
      if i+1 >= x then break;
      if tmp[i].Empty then begin
        w := false;
        break;
      end;

      for j := tmp[i+1].Count-1 downto 0 do begin
        case c.Value of
          scDescendant: begin
            if not tmp[i].Empty then begin
              r := false;

              for a := 0 to tmp[i].Count-1 do begin
                if FindInside(tmp[i+1][j], tmp[i][a]) then begin
                  r := true;
                  break;
                end;
              end;

              if not r then tmp[i+1].Remove(tmp[i+1][j]);
            end;
          end;
          scChild: begin
            if tmp[i].Find(tmp[i+1][j].Parent) < 0 then tmp[i+1].Remove(tmp[i+1][j]);
          end;
          scGeneralSibling, scAdjascentSibling: begin
            if not tmp[i].Empty then begin
              r := false;

              for a := 0 to tmp[i].Count-1 do begin
                if tmp[i][a].Parent = tmp[i+1][j].Parent then begin
                  id1 := tmp[i][a].Parent.Children.Find(tmp[i][a]);
                  id2 := tmp[i+1][j].Parent.Children.Find(tmp[i+1][j]);

                  if ((c.Value = scGeneralSibling) and (id1 < id2)) or
                  ((c.Value = scAdjascentSibling) and (id1+1 = id2)) then begin
                    r := true;
                    break;
                  end;
                end;
              end;

              if not r then tmp[i+1].Remove(tmp[i+1][j]);
            end;
          end;
        end;
      end;

      Inc(i);
    end;

    if (x > 0) and w then begin
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

class function TPLHTMLDocumentQueries.isQuerySelectorFor(
  const AObject: TPLHTMLObject; const AQuery: TPLString): TPLBool;
var
  sel: IPLCSSSelectors;
  s: TPLCSSSelector;
  id, idc: SizeInt;
  ssp: TPLCSSSimpleSelectorPattern;
  obj: TPLHTMLObject;
  cb: TPLCSSSelectorCombinatorItem;

  function FindOutside(where: TPLHTMLObject): TPLBool;
  var
    b: SizeInt;
  begin
    //if (where.Parent) then exit(true);
             //?
  end;

begin
  if TPLString.IsNullOrEmpty(AQuery) then exit(true);
  sel := TPLCSSSelectorParser.ParseSelector(AQuery);
  Result := false;

  for s in sel do begin
    if s.SimpleSelectors.Empty or (s.Combinators.Count < s.SimpleSelectors.Count div 2) then continue;
    obj := AObject;
    idc := s.Combinators.Count-1;

    for id := s.SimpleSelectors.Count-1 downto 0 do begin // ?
      ssp := s.SimpleSelectors[id];
      cb := s.Combinators[idc];

      if not ssp.AppliesTo(obj, obj) then break;

      case cb.Value of
        scDescendant: begin
          if not FindOutside(obj) then break;
        end;
      end;
    end;

    if Result then break;
  end;
end;

end.

