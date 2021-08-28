unit PospoLiteHTML.HTML.Document;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, URIParser, Dialogs, DOM, DOM_HTML, SAX_HTML,
  PospoLiteHTML.Version, PospoLiteHTML.Internet, PospoLiteHTML.CSS.Basics,
  PospoLiteHTML.CSS.Stylesheet, PospoLiteHTML.Cache;

type

  { TPLHTMLDocumentFactory }

  TPLHTMLDocumentFactory = class sealed
  public
    class function GlobalStyles: TPLCSSString;
    class function EmptyFormat(AHead, ABody: TPLCSSString): TPLCSSString;

    // pospolite://version
    class function Version: TPLCSSString;
    // pospolite://error?ACode
    class function Error(ACode: PtrInt): TPLCSSString;
  end;

  TPLHTMLDocumentType = (dtUnknown, dtEmpty, dtHTML, dtXML, dtImage, dtText, dtPDF, dtAudio, dtVideo);

  { TPLHTMLDocument }

  TPLHTMLDocument = class(TPersistent)
  private
    FDoc: THTMLDocument;
    FBody: TDOMNode;
    FHead: TDOMNode;
    FScript: TStringList;
    FStyles: TPLCSSStyleSheet;
    FType: TPLHTMLDocumentType;
    FURL: TPLCSSString;
    procedure SetURL(AValue: TPLCSSString);
  protected
    procedure LoadImage(AStream: TStream; AName: TPLCSSString);
    procedure LoadText(AStream: TStream; AName: TPLCSSString);
    procedure LoadXML(AStream: TStream; AName: TPLCSSString);
    procedure LoadPDF(AStream: TStream; AName: TPLCSSString);
    procedure DownloadFile(AStream: TStream; AName: TPLCSSString);
    procedure LoadAudio(AStream: TStream; AName: TPLCSSString);
    procedure LoadVideo(AStream: TStream; AName: TPLCSSString);

    procedure LoadAllStyles(AParent: TDOMNode);
    procedure LoadAllScripts(AParent: TDOMNode);
    procedure CollectImages;

    procedure LoadFromURL(AURL: TPLCSSString);
    procedure LoadFromString(AString: TPLCSSString; AType: TPLHTMLDocumentType = dtHTML; AParams: TPLCSSString = '');
    procedure LoadFromStream(AStream: TStream; AType: TPLHTMLDocumentType = dtHTML; AParams: TPLCSSString = '');
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadHtmlFromString(AString: TPLCSSString);

    function GetNode(AFrom: TDOMNode; AWhat: TPLCSSString): TDOMNode;

    property URL: TPLCSSString read FURL write SetURL;

    property &Type: TPLHTMLDocumentType read FType;
    property Styles: TPLCSSStyleSheet read FStyles;
    property Script: TStringList read FScript;
    property Head: TDOMNode read FHead;
    property Body: TDOMNode read FBody;
    property AsRaw: THTMLDocument read FDoc;
  end;

  { TPLHTMLDocumentErrors }

  TPLHTMLDocumentErrors = class sealed
  public const
    // general system error
    PLHTML_ERROR_GENERAL = 0;
    // file opening error
    PLHTML_ERROR_FILE_OPEN = 1;
    // file not found
    PLHTML_ERROR_FILE_NOT_FOUND = 2;
    // invalid protocol
    PLHTML_ERROR_INVALID_PROTOCOL = 3;
    // not supported file format
    PLHTML_ERROR_UNSUPPORTED_FILE_FORMAT = 4;
    // stream reading error
    PLHTML_ERROR_STREAM_READ = 5;
  public
    class function {%H-}ToString(AId: PtrInt): TPLCSSString;
  end;

implementation

uses BGRABitmap, SynHighlighterXML, SynExportHTML, PospoLiteHTML.CSS.Parser,
  PospoLiteHTML.Localization;

procedure RemoveEmptyLines(AList: TStringList);
var
  i: integer;
begin
  i := 0;
  while i < AList.Count do begin
    if Trim(AList[i]).Replace(#13, '').Replace(#10, '') = '' then AList.Delete(i) else Inc(i);
  end;
end;

{ TPLHTMLDocumentErrors }

class function TPLHTMLDocumentErrors.ToString(AId: PtrInt): TPLCSSString;
begin
  Result := 'PLHTML_ERROR_UNKNOWN';

  case AId of
    PLHTML_ERROR_GENERAL: Result := 'PLHTML_ERROR_GENERAL';
    PLHTML_ERROR_FILE_OPEN: Result := 'PLHTML_ERROR_FILE_OPEN';
    PLHTML_ERROR_FILE_NOT_FOUND: Result := 'PLHTML_ERROR_FILE_NOT_FOUND';
    PLHTML_ERROR_INVALID_PROTOCOL : Result := 'PLHTML_ERROR_INVALID_PROTOCOL';
    PLHTML_ERROR_UNSUPPORTED_FILE_FORMAT: Result := 'PLHTML_ERROR_UNSUPPORTED_FILE_FORMAT';
    PLHTML_ERROR_STREAM_READ: Result := 'PLHTML_ERROR_STREAM_READ';
    else if (AId > 99) and (AId < 900) then Result := 'PLHTML_ERROR_ONLINECLIENT_' + IntToStr(AId);
  end;
end;

{ TPLHTMLDocumentFactory }

class function TPLHTMLDocumentFactory.GlobalStyles: TPLCSSString;
begin
  Result := '<styles>' + LineEnding +

    '</styles>';
end;

class function TPLHTMLDocumentFactory.EmptyFormat(AHead, ABody: TPLCSSString
  ): TPLCSSString;
begin
  Result := Format('<html>' + LineEnding +
    '<head>' + LineEnding +
      '%s' +
    '</head>' + LineEnding +
    '<body>' + LineEnding +
      '%s' +
    '</body>' + LineEnding +
    '</html>', [AHead, ABody]);
end;

class function TPLHTMLDocumentFactory.Version: TPLCSSString;
begin

end;

class function TPLHTMLDocumentFactory.Error(ACode: PtrInt): TPLCSSString;
begin

end;

{ TPLHTMLDocument }

procedure TPLHTMLDocument.SetURL(AValue: TPLCSSString);
begin
  if FURL = AValue then exit;

  LoadFromURL(AValue);
end;

procedure TPLHTMLDocument.LoadImage(AStream: TStream; AName: TPLCSSString);
var
  dummy: TBGRABitmap;
begin
  if not PLCSSImagesCache.Exists(AName, dummy) then
    PLCSSImagesCache.Add(AName, TBGRABitmap.Create(AStream));

  LoadFromString(TPLHTMLDocumentFactory.EmptyFormat('<title>' + AName + '</title>' + '<style> .imgprev {width:100%; height: 100%;} </style>', '<img class="imgprev" src="' + AName + '">'));
end;

procedure TPLHTMLDocument.LoadText(AStream: TStream; AName: TPLCSSString);
begin
  LoadFromString(TPLHTMLDocumentFactory.EmptyFormat('', '<pre>' + LineEnding + TStringStream(AStream).DataString + LineEnding + '</pre>'));
end;

procedure TPLHTMLDocument.LoadXML(AStream: TStream; AName: TPLCSSString);
var
  h: TSynXMLSyn;
  e: TSynExporterHTML;
  s: TStringList;
  ss: TStringStream;
begin
  e := TSynExporterHTML.Create(nil);
  h := TSynXMLSyn.Create(nil);
  s := TStringList.Create;
  ss := TStringStream.Create('');
  try
    e.Highlighter := h;
    e.Title := AName;
    s.Text := TStringStream(AStream).DataString;
    e.ExportAll(s);
    e.SaveToStream(ss);
    LoadFromString(ss.DataString);
  finally
    ss.Free;
    s.Free;
    h.Free;
    e.Free;
  end;
end;

procedure TPLHTMLDocument.LoadPDF(AStream: TStream; AName: TPLCSSString);
begin
  // not implemented yet
  LoadFromString(TPLHTMLDocumentFactory.Error(TPLHTMLDocumentErrors.PLHTML_ERROR_UNSUPPORTED_FILE_FORMAT));
end;

procedure TPLHTMLDocument.DownloadFile(AStream: TStream; AName: TPLCSSString);
var
  sd: TSaveDialog;
  s: TFileStream;
begin
  sd := TSaveDialog.Create(nil);
  try
    sd.Title := PLLocMng['SAVEDIALOG_TITLE'];
    sd.Filter := PLLocMng['SAVEDIALOG_FILTER'];

    if sd.Execute then begin
      s := TFileStream.Create(sd.FileName, fmCreate or fmOpenWrite or fmShareDenyWrite);
      try
        AStream.Position := 0;
        s.CopyFrom(AStream, AStream.Size);
        AStream.Position := 0;
      finally
        s.Free;
      end;
    end;
  finally
    sd.Free;
  end;
end;

procedure TPLHTMLDocument.LoadAudio(AStream: TStream; AName: TPLCSSString);
begin
  // not implemented yet
  LoadFromString(TPLHTMLDocumentFactory.Error(TPLHTMLDocumentErrors.PLHTML_ERROR_UNSUPPORTED_FILE_FORMAT));
end;

procedure TPLHTMLDocument.LoadVideo(AStream: TStream; AName: TPLCSSString);
begin
  // not implemented yet
  LoadFromString(TPLHTMLDocumentFactory.Error(TPLHTMLDocumentErrors.PLHTML_ERROR_UNSUPPORTED_FILE_FORMAT));
end;

procedure TPLHTMLDocument.LoadAllStyles(AParent: TDOMNode);
var
  n, m: TDOMNode;
  s: TPLCSSString;
begin
  if (AParent = nil) or (AParent.ChildNodes.Count = 0) then exit;

  n := AParent.FirstChild;
  while Assigned(n) do begin
    if LowerCase(n.NodeName) = 'style' then begin
      m := n.FirstChild;
      s := '';
      while Assigned(m) do begin
        if m.NodeType = TEXT_NODE then
          s += m.NodeValue;

        if m = n.LastChild then break;
        m := m.NextSibling;
      end;
      FStyles.Add(s);
    end else if LowerCase(n.NodeValue) = 'link' then begin
      m := n.Attributes.GetNamedItem('rel');
      if Assigned(m) then begin
        if LowerCase(m.NodeValue) = 'stylesheet' then begin
          m := n.Attributes.GetNamedItem('href');
          if Assigned(m) then begin
            s := m.NodeValue;
            CheckAndCorrectURL(s, FURL);
            FStyles.Add(GetStringFromURL(s));
          end;
        end;
      end;
    end;

    if n = AParent.LastChild then break;
    n := n.NextSibling;
  end;
end;

procedure TPLHTMLDocument.LoadAllScripts(AParent: TDOMNode);
var
  n, m: TDOMNode;
  s: TPLCSSString;
begin
  if (AParent = nil) or (AParent.ChildNodes.Count = 0) then exit;

  n := AParent.FirstChild;
  while Assigned(n) do begin
    if LowerCase(n.NodeName) = 'script' then begin
      m := n.Attributes.GetNamedItem('src');
      if Assigned(m) then begin
        s := m.NodeValue;
        CheckAndCorrectURL(s, FURL);
        FScript.Add(GetStringFromURL(s));
      end else begin
        m := n.FirstChild;
        s := '';
        while Assigned(m) do begin
          if m.NodeType = TEXT_NODE then
            s += m.NodeValue;

          if m = n.LastChild then break;
          m := m.NextSibling;
        end;
        FScript.Add(s);
      end;
    end;

    if n = AParent.LastChild then break;
    n := n.NextSibling;
  end;
end;

procedure TPLHTMLDocument.CollectImages;
var
  n, m: TDOMNode;
  s: TPLCSSString;
  bmp: TBGRABitmap;
begin
  if not Assigned(FBody) or (FBody.ChildNodes.Count = 0)then exit;

  n := FBody.FirstChild;
  while Assigned(n) do begin
    if LowerCase(n.NodeName) = 'img' then begin
      m := n.Attributes.GetNamedItem('src');
      if Assigned(m) then begin
        s := m.NodeValue;
        CheckAndCorrectURL(s, FURL);

        if not PLCSSImagesCache.Exists(s, bmp) then begin
          if FileExists(s) then begin
            bmp := TBGRABitmap.Create(s);
            PLCSSImagesCache.Add(s, bmp);
          end else if LoadOnlineImage(s, bmp) then begin
            PLCSSImagesCache.Add(s, bmp);
          end else begin
            PLCSSImagesCache.Add(s, GetBlankImage);
          end;
        end;
      end;
    end;

    if n = FBody.LastChild then break;
    n := n.NextSibling;
  end;
end;

constructor TPLHTMLDocument.Create;
begin
  inherited Create;

  FURL := '';
  FDoc := nil;
  FType := dtEmpty;
  FStyles := TPLCSSStyleSheet.Create;
  FScript := TStringList.Create;
end;

destructor TPLHTMLDocument.Destroy;
begin
  if Assigned(FDoc) then FreeAndNil(FDoc);
  FStyles.Free;
  FScript.Free;

  inherited Destroy;
end;

procedure TPLHTMLDocument.LoadHtmlFromString(AString: TPLCSSString);
begin
  FURL := '';

  LoadFromString(AString);
end;

function TPLHTMLDocument.GetNode(AFrom: TDOMNode; AWhat: TPLCSSString): TDOMNode;
begin
  Result := nil;

  if not Assigned(AFrom) then exit(nil);

  Result := AFrom.FindNode(AWhat);
  if not Assigned(Result) then Result := AFrom.FindNode(AWhat.ToLower);
  if not Assigned(Result) then Result := AFrom.FindNode(AWhat.ToUpper);
  if not Assigned(Result) then Result := AFrom.FindNode(AWhat.ToLowerInvariant);
  if not Assigned(Result) then Result := AFrom.FindNode(AWhat.ToUpperInvariant);
end;

procedure TPLHTMLDocument.LoadFromURL(AURL: TPLCSSString);
var
  uri: TURI;
  fs: TFileStream;
  ms: TMemoryStream;
  tp: TPLHTMLDocumentType;
  tmp: TPLCSSString;
begin
  AURL := AURL.Trim;
  uri := ParseURI(AURL);
  FURL := AURL;

  try
    case uri.Protocol.ToLower of
      '': begin
        LoadFromString(TPLHTMLDocumentFactory.EmptyFormat('', ''));
      end;
      'pospolite': begin
        case uri.Host.ToLower of
          'error': LoadFromString(TPLHTMLDocumentFactory.Error(StrToInt(uri.Params)));
          'version': LoadFromString(TPLHTMLDocumentFactory.Version);
          else LoadFromURL('pospolite://error?' + IntToStr(TPLHTMLDocumentErrors.PLHTML_ERROR_FILE_NOT_FOUND));
        end;
      end;
      'file': begin
        AURL := Copy(AURL, Length('file://'), Length(AURL)).Replace('/', PathDelim);
        if FileExists(AURL) then begin
          try
            fs := TFileStream.Create(AURL, fmOpenRead or fmShareDenyRead);
            try
              tmp := TPLCSSString(ExtractFileExt(AURL)).Replace('.', '').ToLower;

              case tmp of
                'bmp', 'ico', 'jpeg', 'jpg', 'png', 'svg', 'tif', 'tiff', 'webp': tp := dtImage;
                'html', 'htm': tp := dtHTML;
                'pdf': tp := dtPDF;
                'xml', 'xhtml': tp := dtXML;
                'css', 'csv', 'ics', 'js', 'mjs', 'php', 'txt': tp := dtText;
                'aac', 'mid', 'midi', 'mp3', 'oga', 'opus', 'wav', 'weba': tp := dtAudio;
                'avi', 'mpeg', 'ogv', 'webm', '3gp', '3g2': tp := dtVideo;
                else tp := dtUnknown;
              end;

              LoadFromStream(fs, tp, AURL);
            finally
              fs.Free;
            end;
          except
            on e: Exception do LoadFromURL('pospolite://error?' + IntToStr(TPLHTMLDocumentErrors.PLHTML_ERROR_FILE_OPEN));
          end;
        end else LoadFromURL('pospolite://error?' + IntToStr(TPLHTMLDocumentErrors.PLHTML_ERROR_FILE_NOT_FOUND));
      end;
      'http', 'https': begin
        try
          ms := TMemoryStream.Create;
          try
            OnlineClient.Get(AURL, ms);

            if OnlineClient.ResponseStatusCode = 200 then begin
              // https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/MIME_types/Common_types
              if Pos('image/', OnlineClient.ResponseHeaders.Values['Content-Type']) > 0 then tp := dtImage else
              if Pos('text/html', OnlineClient.ResponseHeaders.Values['Content-Type']) > 0 then tp := dtHTML else
              if Pos('application/pdf', OnlineClient.ResponseHeaders.Values['Content-Type']) > 0 then tp := dtPDF else
              if (Pos('/xml', OnlineClient.ResponseHeaders.Values['Content-Type']) > 0) or (Pos('/xhtml+xml', OnlineClient.ResponseHeaders.Values['Content-Type']) > 0) then tp := dtXML else
              if (Pos('text/', OnlineClient.ResponseHeaders.Values['Content-Type']) > 0) or (Pos('/x-httpd-php', OnlineClient.ResponseHeaders.Values['Content-Type']) > 0) then tp := dtText else
              if Pos('audio/', OnlineClient.ResponseHeaders.Values['Content-Type']) > 0 then tp := dtAudio else
              if Pos('video/', OnlineClient.ResponseHeaders.Values['Content-Type']) > 0 then tp := dtVideo else
                tp := dtUnknown;

              LoadFromStream(ms, tp, AURL);
            end else raise Exception.Create('OnlineClient.ResponseStatusCode <> 200');
          finally
            ms.Free;
          end;
        except
          on e: Exception do LoadFromURL('pospolite://error?' + IntToStr(OnlineClient.ResponseStatusCode));
        end;
      end;
      else LoadFromURL('pospolite://error?' + IntToStr(TPLHTMLDocumentErrors.PLHTML_ERROR_INVALID_PROTOCOL));
    end;
  except
    on e: Exception do LoadFromURL('pospolite://error?' + IntToStr(TPLHTMLDocumentErrors.PLHTML_ERROR_GENERAL));
  end;
end;

procedure TPLHTMLDocument.LoadFromString(AString: TPLCSSString;
  AType: TPLHTMLDocumentType; AParams: TPLCSSString);
var
  ss: TStringStream;
begin
  ss := TStringStream.Create(AString);
  try
    LoadFromStream(ss, AType, AParams);
  finally
    ss.Free;
  end;
end;

procedure TPLHTMLDocument.LoadFromStream(AStream: TStream;
  AType: TPLHTMLDocumentType; AParams: TPLCSSString);
var
  Node: TDOMNode;
begin
  if Assigned(FDoc) then FreeAndNil(FDoc);
  FBody := nil;
  FHead := nil;
  FStyles.Clear;
  FScript.Clear;

  try
    case AType of
      dtEmpty: begin
        LoadFromString(TPLHTMLDocumentFactory.EmptyFormat('',''));
        exit;
      end;
      dtText: begin
        LoadText(AStream, AParams);
        exit;
      end;
      dtImage: begin
        LoadImage(AStream, AParams);
        exit;
      end;
      dtXML: begin
        LoadXML(AStream, AParams);
        exit;
      end;
      dtPDF: begin
        LoadPDF(AStream, AParams);
        exit;
      end;
      dtAudio: begin
        LoadAudio(AStream, AParams);
        exit;
      end;
      dtVideo: begin
        LoadVideo(AStream, AParams);
        exit;
      end;
      dtUnknown: begin
        DownloadFile(AStream, AParams);
        exit;
      end;
    end;

    // dtHTML:

    ReadHTMLFile(FDoc, AStream);

    // img, css ...

    Node := FDoc.DocumentElement.FirstChild;
    while Assigned(Node) and (Node.NodeName <> 'head') do
      Node := Node.NextSibling;
    FHead := Node;

    Node := FDoc.DocumentElement.FirstChild;
    while Assigned(Node) and (Node.NodeName <> 'body') do
      Node := Node.NextSibling;
    FBody := Node;

    if Assigned(FHead) then begin
      LoadAllStyles(FHead);
      LoadAllScripts(FHead);
    end;

    if Assigned(FBody) then begin
      LoadAllStyles(FBody);
      LoadAllScripts(FBody);
    end;

    RemoveEmptyLines(FScript);

    CollectImages;
  except
    on e: Exception do LoadFromURL('pospolite://error?' + IntToStr(TPLHTMLDocumentErrors.PLHTML_ERROR_STREAM_READ));
  end;
end;

end.

