unit Pospolite.View.DOM.Document;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Pospolite.View.Basics, Pospolite.View.HTML.Document;

type

  { IPLPlainTextDocument }

  IPLPlainTextDocument = interface(IPLHTMLDocument)
    ['{1E767884-D463-4322-9A3C-2366308F4985}']
  end;

  { IPLScriptDocument }

  IPLScriptDocument = interface(IPLHTMLDocument)
    ['{0791AC80-6028-4221-8184-70DD2FFE3994}']
    procedure CompileAndRun;
  end;

  { IPLImageDocument }

  IPLImageDocument = interface(IPLHTMLDocument)
    ['{36C085F0-02A7-417B-B275-BFBEBD829E44}']
  end;

  { IPLAudioDocument }

  IPLAudioDocument = interface(IPLHTMLDocument)
    ['{608F3741-5A22-4B55-847D-2B1E6BAA9BBE}']
  end;

  { IPLVideoDocument }

  IPLVideoDocument = interface(IPLHTMLDocument)
    ['{C4D73C29-2AF6-4A4D-BBE8-A8A0FC0C1472}']
  end;

  { IPLPDFDocument }

  IPLPDFDocument = interface(IPLHTMLDocument)
    ['{83116245-1FB3-44A8-97E9-CC201BB59096}']
  end;

  { IPLOtherDocument }

  IPLOtherDocument = interface(IPLHTMLDocument)
    ['{34C13B33-E4A0-40FC-B536-49F4BDFC954C}']
  end;

implementation

end.

