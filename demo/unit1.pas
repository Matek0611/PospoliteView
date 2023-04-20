unit Unit1;

{$mode objfpc}{$H+}
{$macro on}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, dateutils,
  math, Pospolite.View.Basics, Pospolite.View.Threads, Clipbrd, ExtCtrls,
  windows;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Edit1: TEdit;
    Memo1: TMemo;
    Panel1: TPanel;
    Shape1: TShape;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Edit1EditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure test1(const AArguments: array of const);
    procedure test2(const AArguments: array of const);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses Pospolite.View.CSS.Declaration, Pospolite.View.Drawing.Basics,
  Pospolite.View.Drawing.Drawer, Pospolite.View.Drawing.DrawerD2D1,
  Pospolite.View.Drawing.Renderer, Pospolite.View.Internet, Pospolite.View.Version,
  Pospolite.View.HTML.Document, Pospolite.View.Controls,
  Pospolite.View.CSS.Selector, Pospolite.View.CSS.MediaQuery, Pospolite.View.CSS.Binder,
  Pospolite.View.RegisterAll, Pospolite.View.HTML.Basics, Pospolite.View.DOM.Document;

threadvar
  ax, bx: sizeint;

type
  tt = specialize TPLObjectList<TPLCSSDeclarations>;

var
  tab: tt;
  d: TPLD2D1Drawer;
  sj: IPLHTTPClient = nil;
  task1, task2: IPLAsyncTask;
  browser: TPLHTMLViewer;
  tabs: TPLUITabs;

const
  LONG_VAL = '--val1: 3.45px inset /*rgba(23 13 44 / 35%)*/ ;--xxx: linear-gradient(to left, #333, #333 50%, #eee 75%, #333 75%), linear-gradient(217deg, rgba(255,0,0,.8), rgba(255,0,0,0) 70.71%);-webkit-appearance: media-enter-fullscreen-button;font-family: "Segoe UI", Verdana, sans-serif;width: calc(var(--variable-width) / 20px);;'#13#10'background-color: rgba(233 23 13 / 56%), #00f0;transform: rotate(45deg) translateX(180px) !important;transform: rotate3d(1, 1, 1, 30deg) matrix3d(1,0,0,0,0,1,6,0,0,0,1,0,50,100,0,1.1);';

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  i: integer;
  dt: TDateTime;
  p: TPLCSSProperty;
begin
  //tab := tt.Create(true);

  //for i := 0 to 100 do
  //  tab.Add(TPLCSSDeclarations.Create(LONG_VAL));

  //dt := now;
  //if tab[0].Exists('--val1', p) then ShowMessage('Found "' + p.AsString + '" in ' + MilliSecondsBetween(now, dt).ToString + 'ms');
  //ShowMessage(tab[0].AsString);

  //for p in tab[0] do ShowMessage(p.AsString);

  //ShowMessage(TPLColor('linen').ToString(ctHSLWithAlpha, true));
  //ShowMessage(AngleDeg(-100, 'grad'));

  //ShowMessage(BoolToStr(TPLPointF.Create(7, 8) = TPLPointI.Create(7, 8)));

  //ShowMessage(TPLCSSProperty.InstanceSize);

  browser := TPLHTMLViewer.Create(self);
  browser.Align := alClient;
  browser.BorderSpacing.Top := 50;
  browser.Debug := true;

  tabs := TPLUITabs.Create(self);
  tabs.Parent := Self;
  //tabs.Theme.ToDark;
  //tabs.Manager.Animate := true;
end;

procedure TForm1.Edit1EditingDone(Sender: TObject);
begin
  Invalidate;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  oc: IPLHTTPClient;
  doc: IPLHTMLDocument;
  s: TPLCSSSelectors;
  i, j, k: SizeInt;
  ss: TPLString = '';
  obj: IPLHTMLObjects;
  env: TPLCSSMediaQueriesEnvironment;
  mq: TPLCSSMediaQueries;
  arrb: array of TPLHTMLNormalObject;
begin
  //oc := OnlineClient;
  //Memo1.Lines.Text := oc.Download(InputBox('Strona', 'Podaj adres URL', 'https://developer.mozilla.org/en-US/docs/Web/HTML'));
  //ShowMessage(oc.MimeType);

  //doc := TPLHTMLDocument.Create;
  //doc.LoadFromLocalFile('D:\lazarus\docs\index.html');
  //doc.LoadFromString('<html><head><title>%s</title></head><body><h1 id="test" class="ma">%s</h1><span></span><p id="abc" class="m1 test xd">%s</p><p>Q</p><a></a><p>xxx</p><div id="sel"><p id="in"><p>w</p></p></div></body></html>');
  //doc.LoadFromURL('https://github.com/Matek0611/PospoliteView');
  //doc.LoadFromURL('');
  //Memo1.Lines.Text := doc.Root.ToHTML;
  //ShowMessage(doc.Title);
  //doc.Title := 'test to jest';
  //Memo1.Lines.Text := doc.Root.ToHTML;
  //obj := doc.querySelectorAll('body > p:empty');
  //for i := 0 to obj.Count-1 do ss += obj[i].ToHTML + LineEnding + '---' + LineEnding;
  //ShowMessage(ss);

  //Memo1.Clear;
  //ax := 0;
  //bx := 0;
  //Async(@test1);
  //Async(@test2);

  //env := TPLCSSMediaQueriesEnvironment.Create(100, 100);
  //env.UsePrinter := true;
  //ShowMessage(env.FeatureHeight);
  //for i := 0 to Memo1.Lines.Count-1 do begin
  //  mq := TPLCSSMediaQueries.Create();
  //  try
  //    TPLCSSMediaQueryParser.ParseMediaQueries(Memo1.Lines[i], mq);
  //    ss += ('Media Query: ' + mq.AsString + LineEnding + 'Result: ' + BoolToStr(mq.Evaluate(env), true)) + LineEnding + LineEnding;
  //  finally
  //    mq.Free;
  //  end;
  //end;
  //Memo1.Lines.Add(ss);
  //ShowMessage(ss);

  //browser.LoadFromString('<html><head><title>Test</title></head><body><h1 id="test" class="ma">%s</h1><span></span><p id="abc" class="m1 test xd">%s</p><p>Q</p><a></a><p>xxx</p><div id="sel"><p id="in"><p>w<p id="in"><p>w<p id="in"><p>w<p id="in"><p>w<p id="in"><p>w<p id="in"><p>w<p id="in"><p>w<p id="in"><p>w<p id="in"><p>w<p id="in"><p>w<p id="in"><p>w<p id="in"><p><p id="in"><p>w<p id="in"><p>w<p id="in"><p>w<p id="in"><p>w<p id="in"><p>w<p id="in"><p>w<p id="in"><p>w<p id="in"><p>w<p id="in"><p><p id="in"><p>w<p id="in"><p>w<p id="in"><p>w<p id="in"><p>w<p id="in"><p>w<p id="in"><p>w<p id="in"><p>w<p id="in"><p>w<p id="in"><p>w<p id="in"><p>w<p id="in"><p>w<p id="in"><p>w<p id="in"><p>w<p id="in"><p>w<p id="in"><p>w<p id="in"><p>w</p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></p></div></body></html>');
  //browser.LoadFromString('<html lang="pl"><head><title>Test</title><link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.2.1/css/bootstrap.min.css" integrity="sha384-GJzZqFGwb1QTTN6wy59ffF1BuGJpLSa9DkKMp0DgiMDm4iYMj70gZWKYbI706tWS" crossorigin="anonymous"><style>.spdiv{width: 200px; height: 100px; background: red;}</style></head><body><div class="spdiv"></div></body></html>');
  //browser.LoadFromString('<html><head><title>Test</title></head><body><h1>Nagłówek 1</h1><p>Opis jakiś tam</p></body></html>');
  //browser.LoadFromString('<ul> <li>Unordered item</li> <li>Unordered item <ol> <li>Item 1</li> <li>Item 2</li> </ol> </li> </ul>');
  //browser.LoadFromString('<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"> <html> <head> <meta content="text/html; charset=ISO-8859-1" http-equiv="content-type"> <title>About Lazarus</title> </head> <body> <h3><br> <a href="MainPage.html">Back to the Main Page</a><br> <a href="SecondPage.html">Forward to the Second Page</a></h3> <h3> History </h3> <p> Lazarus was started in February of 1999. It was primarily founded by three individuals: </p> <ul> <li>Cliff Baeseman</li> <li>Shane Miller</li> <li>Michael A. Hess</li> </ul> All three had attempted to get involved with the Megido project which dissolved. In frustration they started the Lazarus project. It has had a steady growth of supporters and developers during the following years. Of the three founders, only Michael A. Hess is still involved with the project.<br> <br> The next oldest member of the team is Marc Weustink. He got involved with the project in Aug. 1999. Following him is Mattias Gaertner who got involved in Sept. 2000. Both of them have been the major contributors to the core of what makes Lazarus tick. <p></p> <h3> So just what is Lazarus? </h3> <p> Lazarus is the class libraries for Free Pascal that emulate Delphi. Free Pascal is a GPL"ed compiler that runs on Linux, Win32, OS/2, 68K and more. Free Pascal is designed to be able to understand and compile Delphi syntax, which is of course OOP. Lazarus is the part of the missing puzzle that will allow you to develop Delphi like programs in all of the above platforms. Unlike Java which strives to be a write once run anywhere, Lazarus and Free Pascal strives for write once compile anywhere. Since the exact same compiler is available on all of the above platforms it means you don"t need to do any recoding to produce identical products for different platforms. </p> <h3> Yeah, but what about the GUI? What widget set are you using? </h3> <p> That is the neat part. You decide. Lazarus is being developed to be totally and completely API independent. Once you write your code you just link it against the API widget set of your choice. If you want to use GTK+, great! If you want it to be Gnome compliant, great! As long as the interface code for the widget set you want to use is available you can link to it. If it isn"t available, well you can write it. </p> <p> For example. Let"s say you are creating a product on Windows using the standard Windows widgets. Now you want to create a Linux version. First you decide what widget set you want to use. Let"s assume you want to use gtk+. So you copy the code over to your Linux development machine, compile, and link against the gtk+ interface unit. That"s it. You"ve now just created a Linux version of the Windows product without any additional coding. </p> <p> At this point in the development we are using gtk+ as our initial API widget set. Some work is also being done with Qt and the Win32 API. As soon as Lazarus reaches a 1.0 release developers will be able to start to create the interface unit to tie the LCL (Lazarus Component Libraries) to other widget sets. </p> <h3> So is this thing really RAD like Delphi? </h3> <p> It sure is. Is it totally completed? No not yet. The forms design portion is still in need of a great deal of development. The over all IDE is complete and can be used for most programming needs. Several aspects of the project are still in need of help. Hint. Hint. </p> <h3> Can I use my existing Delphi code? </h3> <p> Some of it yes. If the code is standard Delphi pascal and it uses the standard components found in Delphi then the answer is yes. If it uses some specific database, OCX, or DCU then the answer would be no. These items are specific to Windows and would only work on and within Windows. However, if you are only looking to create a Windows product using Free Pascal and Lazarus then the answer would be yes. This hasn"t been added to the LCL yet but it should be possible in the future. </p> <h3> Can I create commercial products with this? </h3> <p> Yes. The code for the Free Pascal compiler is licensed under the GPL. This means that it is open source, free, whatever name you want to stick to it. You can modify the code if you wish but you MUST distribute those changes or make them available to others if they wish to use it. </p> <p> The FCL (Free Pascal Component Libraries) and the LCL (which will eventually become part of the FCL) are licensed under a modified LGPL. In a nut shell this means that you can write your own proprietary software that just links to these libraries. You can sell your application without the need to supply or make available your code. However, as with the compiler if you make modifications to the FCL or LCL you must make those changes available to the general public and the world. </p> </body> </html> ');
  browser.LoadFromString('<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd"> <html><head> <meta http-equiv="content-type" content="text/html; charset=ISO-8859-1"> <title>Lazarus</title><meta name="author" content="Mattias Gaertner"> <meta name="description" content="Start Page"></head> <body style="color: rgb(0, 0, 0); background-color: rgb(255, 255, 255); font-family: helvetica,arial,sans-serif;" alink="#006600" link="#993300" vlink="#660000"><table style="width: 740px; text-align: left; margin-left: auto; margin-right: auto;" border="0" cellpadding="2" cellspacing="2"> <tbody> <tr align="center"> <td style="vertical-align: top; background-color: rgb(255, 255, 255);"><big> <img src="images/laztitle.png" title="" alt="The Lazarus Project" style="width: 740px; height: 100px;"></big><big><span style="font-family: helvetica,arial,sans-serif;"><br> </span></big></td> </tr> <tr> <td style="vertical-align: top; background-color: rgb(255, 255, 255);"><big><br>Welcome to Lazarus</big><br> <br> Lazarus is a Rapid Application Development tool for Free Pascal and currently runs on Linux, Mac OS X, BSD and of course Windows.<br> It is freely available, open source and completely written in Free Pascal.<br> <br>The official Lazarus site is <a href="https://www.lazarus-ide.org/">https://www.lazarus-ide.org/</a>.<br> There is a wiki with a lot of information around Lazarus at <a href="http://wiki.lazarus.freepascal.org/">http://wiki.lazarus.freepascal.org/</a>.<br> Free Pascal can be found at <a href="https://www.freepascal.org/">https://www.freepascal.org/</a>.<br> <br> The Lazarus Component Library is licensed under a modified GNU Lesser General Public License.<br> The Lazarus IDE is licensed under the GNU General Public License.<br> <br> <a href="https://www.freepascal.org/docs.var">Free Pascal online documentation.</a><br> <a href="http://lazarus-ccr.sourceforge.net/docs/rtl">RTL - Free Pascal Run Time Library</a><br> <a href="http://lazarus-ccr.sourceforge.net/docs/fcl">FCL - Free Component Library</a><br> <a href="http://lazarus-ccr.sourceforge.net/docs/lcl">LCL - Lazarus Component Library</a><br> <br> <h3>Offline help</h3> Many help files are available for download in chm and inf format.<br> <ul> <li>Viewer for chm files: see <a href="http://wiki.freepascal.org/Installing_Help_in_the_IDE#Installing_CHM_Help_for_The_RTL.2C_FCL_and_LCL_in_the_Lazarus_IDE">Installing CHM help in the IDE</a></li> <li>Viewer for inf files: see <a href="https://fpgui.sourceforge.net/docview_ide_integration.shtml">DocView IDE Integration</a>.</li> </td> </tr> </tbody> </table><big><br> </big><br></body></html> ');
  Caption := 'Demo - ' + browser.Document.Title;

  //SetLength(arrb, 1000);
  //for i := 0 to 1000-1 do arrb[i] := TPLHTMLNormalObject.Create(nil);
  //ShowMessage(SizeOf(arrb[0]));
  //for i := 0 to 1000-1 do arrb[i].Free;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Clipboard.AsText := OnlineClient.FileGetContents(InputBox('url', '', ''));
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  l: IPLHTMLObjects;
  ob: TPLHTMLObject;
  s: string = '';
  sel: string;
  freq, ta, tb: Int64;
  v1, v2: double;
begin
  QueryPerformanceFrequency(freq);
  //ShowMessage(browser.Document.Root.ToHTML);
  sel := InputBox('Query Selector', 'Selector:', '');

  QueryPerformanceCounter(tb);
  l := TPLHTMLDocumentQueries.querySelectorFast(sel, browser.Document.Root, false);
  QueryPerformanceCounter(ta);
  v1 := (ta-tb)/(10e9/freq);
  s := 'FAST' + LineEnding;
  for ob in l do s += ob.ToHTML + LineEnding;
  ShowMessage(s);   // body table tbody tr[align="center"] > td[style] br

  //QueryPerformanceCounter(tb);
  //l := TPLHTMLDocumentQueries.querySelectorX(sel, browser.Document.Root, false);
  //QueryPerformanceCounter(ta);
  //v2 := (ta-tb)/(10e9/freq);
  //s := 'X' + LineEnding;
  //for ob in l do s += ob.ToHTML + LineEnding;
  //ShowMessage(s);

  ShowMessage('fast: ' + FormatFloat('0.00000 ns', v1) + LineEnding + 'x: ' + FormatFloat('0.00000 ns', v2));
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  //browser.Free;
  //tab.Free;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  //Invalidate;
end;

procedure TForm1.FormPaint(Sender: TObject);
var
  b: array[1..4] of TPLDrawingBorder;
  dr: IPLDrawingRenderer;
  decl: TPLCSSDeclarations;
  gb: IPLDrawingBrushGradientLinear;
  surf: IPLDrawingSurface;
begin
  //d := TPLD2D1Drawer.Create(self.Canvas);
  //try
  //  //with d.Surface do begin
  //  //  Ellipse(TPLRectF.Create(0, 0, 100, 100));
  //  //  Fill(NewDrawingSolidBrushD2D('#d7c2e6'), true);
  //  //  Ellipse(TPLRectF.Create(0, 0, 100, 100));
  //  //  Stroke(NewDrawingPenD2D('#a16ac8'));
  //  //end;
  //  b[1] := TPLDrawingBorder.Create(4, TPLColor.Create(255, 0, 0), dbsOutset);
  //  b[2] := TPLDrawingBorder.Create(4, TPLColor.Create(15, 255, 4), dbsOutset);
  //  b[3] := TPLDrawingBorder.Create(4, TPLColor.Create(0, 0, 255), dbsOutset);
  //  b[4] := TPLDrawingBorder.Create(4, TPLColor.Create(100, 255, 50), dbsOutset);
  //  gb := NewDrawingLinearGradientBrushD2D(TPLPointF.Create(0, 0), TPLPointF.Create(0, 200));
  //  gb.AddStop('red', 0);
  //  gb.AddStop('blue', 1);
  //  d.DrawBox(TPLRectF.Create(10, 10, 200, 100), gb, //NewDrawingSolidBrushD2D('ivory'),
  //    TPLDrawingBorders.Create(b[1], b[2], b[3], b[4], PLDrawingBordersRadiusData(10, 0, 30, 5)));
  //finally
  //  d.Free;
  //end;

  //dr := TPLDrawingRenderer.Create(Canvas);          //linear-gradient(217deg, rgba(255,0,0,0.8), rgba(0,0,0,0) 70.71%), linear-gradient(127deg, rgba(0,255,0,.8), rgba(0,255,0,0) 70.71%), linear-gradient(336deg, rgba(0,0,255,.8), rgba(0,0,255,0) 70.71%)
  //dr.DrawBox(TPLRectF.Create(10, 40, Width-20, Height-50), TPLCSSDeclarations.Create('outline-color:skyblue;outline-style:dotted;outline-offset:5px;border-left-style: dotted;border-bottom-color: red;/*border-image:linear-gradient(red, white) 20%*/;border-top-width:7pt;border-width:3pt;border-radius: 15px 100px 50px;background-image:'+Edit1.Text+';background-color: #aaaa;'), nil, false, true);

  //surf := NewDrawingSurfaceD2D(Canvas);
  //surf.Clear('#abc');
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  //ShowMessage(TPLViewVersion.UserAgent);
  tabs.AddTab;
end;

procedure TForm1.test1(const AArguments: array of const);
begin
  while ax < 1000 do begin
    Memo1.Lines.Add('a = ' + ax.ToString);
    ax += 1;
  end;
end;

procedure TForm1.test2(const AArguments: array of const);
begin
  while bx < 1000 do begin
    Memo1.Lines.Add('b = ' + bx.ToString);
    bx += 1;
  end;
end;

end.

