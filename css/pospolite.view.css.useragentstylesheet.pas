unit Pospolite.View.CSS.UserAgentStyleSheet;

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
  Classes, SysUtils;

const CPLUserAgentStylesSource =
  '@namespace url(http://www.w3.org/1999/xhtml);' + LineEnding +
  'address, article, aside, body, center, details, dd, div, dl, dt, figcaption, footer, form, header, hgroup, html, main, multicol, nav, p, section, summary, blockquote, figure { display: block; }' + LineEnding +
  'body { margin: 10px; }' + LineEnding +
  'p, dl, multicol { margin: 0 1em; }' + LineEnding +
  'dd { margin-top: 40px; }' + LineEnding +
  'blockquote, figure { margin: 40px 1em; }' + LineEnding +
  'blockquote[type=cite] { margin: 0 1em; padding-top: 1em; border-left: thin solid gray; }' + LineEnding +
  'address, i, cite, em, var, dfn { font-style: italic; }' + LineEnding +
  'center { text-align: center; }' + LineEnding +
  'h1, h2, h3, h4, h5, h6 { display: block; font-weight: bold; }' + LineEnding +
  'h1 { font-size: 2em; margin: 0 0.67em; }' + LineEnding +
  'h2 { font-size: 1.5em; margin: 0 0.83em; }' + LineEnding +
  'h3 { font-size: 1.17em; margin: 0 1em; }' + LineEnding +
  'h4 { font-size: 1em; margin: 0 1.33em; }' + LineEnding +
  'h5 { font-size: 0.83em; margin: 0 1.67em; }' + LineEnding +
  'h6 { font-size: 0.67em; margin: 0 2.33em; }' + LineEnding +
  'xmp, pre, plaintext, listing { display: block; font-family: monospace; white-space: pre; margin: 0.25em 1em; }' + LineEnding +
  'listing { font-size: medium; }' + LineEnding +
  'b, strong { font-weight: bolder; }' + LineEnding +
  'u, ins { text-decoration: underline; }' + LineEnding +
  's, strike, del { text-decoration: line-through; }' + LineEnding +
  'big { font-size: larger; }' + LineEnding +
  'small { font-size: smaller; }' + LineEnding +
  'sub { vertical-align: sub; font-size: smaller; }' + LineEnding +
  'sup { vertical-align: super; font-size: smaller; }' + LineEnding +
  'nobr { white-space: nowrap; }' + LineEnding +
  'mark { background: yellow; color: black; }' + LineEnding +
  'abbr[title], acronym[title] { text-decoration: dotted underline; }' + LineEnding +
  'spacer { position: static !important; float: none !important; }' + LineEnding +
  'iframe { border: 2px inset; }' + LineEnding +
  'frame { border-radius: 0 !important; }' + LineEnding +
  'img[usemap], object[usemap] { color: blue; }' + LineEnding +
  'hr { display: block; border: 1px inset; margin auto 0.5em; color: gray; box-sizing: content-box; }' + LineEnding +
  'hr[size="1"] { border-style: solid none none none; }' + LineEnding +
  'ul, menu, dir { display: block; list-style-type: disc; margin: 0 1em; padding-left: 40px; }' + LineEnding +
  'ul, ol, menu { counter-reset: list-item; -pl-list-reversed: false; }' + LineEnding +
  'ol[reversed] { -pl-list-reversed: true; }' + LineEnding +
  'ol { display: block; list-style-type: decimal; margin: 20px 1em; }' + LineEnding +
  'li { display: list-item; }' + LineEnding +
  'ul > li > li, menu > li > li, dir > li > li { list-style-type: circle; }' + LineEnding +
  'ul > li > li > li, menu > li > li > li, dir > li > li > li { list-style-type: square; }' + LineEnding +
  'q:before, q:after { content: "\""; }' + LineEnding +
  '';
  //'' + LineEnding +

var PLUserAgentInstance: Pointer = nil;

implementation

uses Pospolite.View.CSS.StyleSheet;

initialization
  PLUserAgentInstance := TPLCSSStyleSheet.Create;
  TPLCSSStyleSheet(PLUserAgentInstance).Load(CPLUserAgentStylesSource);

finalization
  TPLCSSStyleSheet(PLUserAgentInstance).Free;

end.

