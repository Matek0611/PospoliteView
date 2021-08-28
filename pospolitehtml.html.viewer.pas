unit PospoLiteHTML.HTML.Viewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, LMessages, math, Forms, DOM_HTML, DOM,
  LCLType, LCLProc, LCLIntf, strutils, BGRABitmap, BGRABitmapTypes, Dialogs,
  PospoLiteHTML.CSS.Basics, PospoLiteHTML.CSS.Classes,
  PospoLiteHTML.HTML.Document, PospoLiteHTML.Localization,
  PospoLiteHTML.HTML.Elements, PospoLiteHTML.Version;

type

  { TPLHTMLViewerOptionsAbout }

  TPLHTMLViewerOptionsAbout = class(TPersistent)
  private
    function GetAuthor: string;
    function GetExVersion: string;
    function GetJavaScript: string;
    function GetModules: string;
    function GetName: string;
    function GetOS: string;
    function GetUserAgent: string;
    function GetVersion: string;
  public
    property Name: string read GetName;
    property Version: string read GetVersion;
    property ExVersion: string read GetExVersion;
    property OS: string read GetOS;
    property JavaScript: string read GetJavaScript;
    property UserAgent: string read GetUserAgent;
    property Modules: string read GetModules;
    property Author: string read GetAuthor;
  end;

  { TPLHTMLViewerOptions }

  TPLHTMLViewerOptions = class(TPersistent)
  private
    FAbout: TPLHTMLViewerOptionsAbout;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    property About: TPLHTMLViewerOptionsAbout read FAbout;
  end;

  TPLHTMLViewer = class(TPLHTMLElementHtml)
  private
    FOptions: TPLHTMLViewerOptions;
    FDoc: TPLHTMLDocument;
    function GetTitle: string;
    procedure SetOptions(AValue: TPLHTMLViewerOptions);
  protected
    procedure Rebuilt;
    procedure UpdateElements;
    procedure Paint; override;

    function CreateBody(AParent: TPLHTMLBasicObject; ANode: TDOMNode): TPLHTMLElementBody;
    function CreateDiv(AParent: TPLHTMLBasicObject; ANode: TDOMNode): TPLHTMLElementDiv;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadURL(AURL: string);
    procedure LoadText(AText: string);

    procedure SetUserAgentStyles;

    property Document: TPLHTMLDocument read FDoc;
  published
    property Options: TPLHTMLViewerOptions read FOptions write SetOptions;
    property Title: string read GetTitle;
    property Zoom;
  end;

implementation

{ TPLHTMLViewerOptionsAbout }

function TPLHTMLViewerOptionsAbout.GetAuthor: string;
begin
  Result := TPLHTMLBrowserVersion.Author;
end;

function TPLHTMLViewerOptionsAbout.GetExVersion: string;
begin
  Result := TPLHTMLBrowserVersion.ExVersion;
end;

function TPLHTMLViewerOptionsAbout.GetJavaScript: string;
begin
  Result := TPLHTMLBrowserVersion.JavaScript;
end;

function TPLHTMLViewerOptionsAbout.GetModules: string;
begin
  Result := TPLHTMLBrowserVersion.Modules;
end;

function TPLHTMLViewerOptionsAbout.GetName: string;
begin
  Result := TPLHTMLBrowserVersion.Name;
end;

function TPLHTMLViewerOptionsAbout.GetOS: string;
begin
  Result := TPLHTMLBrowserVersion.OS;
end;

function TPLHTMLViewerOptionsAbout.GetUserAgent: string;
begin
  Result := TPLHTMLBrowserVersion.UserAgent;
end;

function TPLHTMLViewerOptionsAbout.GetVersion: string;
begin
  Result := TPLHTMLBrowserVersion.Version;
end;

{ TPLHTMLViewerOptions }

constructor TPLHTMLViewerOptions.Create;
begin
  inherited Create;

  FAbout := TPLHTMLViewerOptionsAbout.Create;
end;

destructor TPLHTMLViewerOptions.Destroy;
begin
  FAbout.Free;

  inherited Destroy;
end;

procedure TPLHTMLViewerOptions.Assign(Source: TPersistent);
var
  opts: TPLHTMLViewerOptions;
begin
  if Source is TPLHTMLViewerOptions then begin
    opts := Source as TPLHTMLViewerOptions;


  end;
end;

{ TPLHTMLViewer }

procedure TPLHTMLViewer.SetOptions(AValue: TPLHTMLViewerOptions);
begin
  FOptions.Assign(AValue);
end;

procedure TPLHTMLViewer.Rebuilt;

  procedure BuildElement(AParent: TPLHTMLBasicObject; ANode: TDOMNode);
  var
    n: TDOMNode;
    p: TPLHTMLBasicObject;
  begin
    n := ANode;
    while Assigned(n) do begin
      p := nil;
      case LowerCase(n.NodeName) of
        'body': p := CreateBody(AParent, n);
        'div': p := CreateDiv(AParent, n);
      end;

      if Assigned(p) then BuildElement(p, n.FirstChild);

      n := n.NextSibling;
    end;
  end;

var
  i: integer;
begin
  for i := ControlCount-1 downto 0 do begin
    if Controls[i] is TPLHTMLBasicObject then Controls[i].Free;
  end;

  BuildElement(Self, FDoc.AsRaw.DocumentElement.FirstChild);

  UpdateElements;
end;

procedure TPLHTMLViewer.UpdateElements;
var
  i: integer;
begin
  for i := 0 to ControlCount-1 do begin
    if Controls[i] is TPLHTMLElementWithLayout then
      TPLHTMLElementWithLayout(Controls[i]).UpdateLayout;
  end;
end;

procedure TPLHTMLViewer.Paint;
begin
  inherited Paint;
end;

function TPLHTMLViewer.CreateBody(AParent: TPLHTMLBasicObject; ANode: TDOMNode
  ): TPLHTMLElementBody;
begin
  Result := TPLHTMLElementBody.Create(Self);
  Result.Parent := AParent;
  Result.DestParent := AParent;
  Result.Browser := Self;

  Result.Node := ANode;
end;

function TPLHTMLViewer.CreateDiv(AParent: TPLHTMLBasicObject; ANode: TDOMNode
  ): TPLHTMLElementDiv;
begin
  Result := TPLHTMLElementDiv.Create(Self);
  Result.Parent := AParent;
  Result.DestParent := AParent;
  Result.Browser := Self;

  Result.Node := ANode;
end;

function TPLHTMLViewer.GetTitle: string;
begin
  Result := FDoc.AsRaw.Title;
end;

constructor TPLHTMLViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FOptions := TPLHTMLViewerOptions.Create;
  FDoc := TPLHTMLDocument.Create;

  SetUserAgentStyles;
end;

destructor TPLHTMLViewer.Destroy;
begin
  FDoc.Free;
  FOptions.Free;

  inherited Destroy;
end;

procedure TPLHTMLViewer.LoadURL(AURL: string);
begin
  FDoc.URL := AURL;

  Rebuilt;
end;

procedure TPLHTMLViewer.LoadText(AText: string);
begin
  FDoc.LoadHtmlFromString(AText);

  Rebuilt;
end;

procedure TPLHTMLViewer.SetUserAgentStyles;
//var
  //p: TPLCSSProperties;
begin
  //p := Styles['html'].Properties;
  //p['display'] := 'block';
  //p['background-color'] := 'white';
  //
  //p := Styles['head'].Properties;
  //p['display'] := 'none !important';
  //
  //p := Styles['body'].Properties;
  //p['display'] := 'block';
  //p['margin'] := '8px';
  //p['width'] := '100%';
  //
  //p := Styles['div'].Properties;
  //p['display'] := 'block';
  //
  //p := Styles['p'].Properties;
  //p['display'] := 'block';
end;

end.

