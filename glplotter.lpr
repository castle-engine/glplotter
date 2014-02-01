{
  Copyright 2001-2013 Michalis Kamburelis.

  This file is part of "glplotter".

  "glplotter" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "glplotter" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "glplotter"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  ----------------------------------------------------------------------------
}

program glplotter;

{ TODO:
  - format pliku: kolor= i specyfikacja koloru wykresu
  - podawanie wlasnego kolor schema parametrami (albo plik ini ?)
}

{ Old comments:
  Uwaga : ten program jest dla Windowsa aplikacja GUI, tzn. domyslnie
  nie ma przypisanych plikow stdin, stdout i stderr. Pod windowsem chcac
  zaladowac wykres z stdin musisz wiec zawsze uruchomic program
  tak zeby jego stdin bylo utworzone - na przyklad, umiescic
  glplotter na koncu potoku :
      gen_function 0 100 0.1 "x+1" | glplotter -
  zadziala zgodnie z oczekiwaniami.
}

{$I castleconf.inc}
{$apptype GUI}

uses SysUtils, CastleGL, CastleWindow, CastleUtils, CastleGLUtils, Math, Classes,
  CastleClassUtils, CastleMessages, CastleParameters, CastleVectors,
  CastleStringUtils, CastleFilesUtils, CastleScript, CastleScriptParser,
  CastleWindowRecentFiles, CastleGLImages, CastleColors, FGL, CastleGenericLists,
  CastleConfig, CastleKeysMouse, CastleURIUtils, CastleControls, CastleControlsImages;

{$define read_interface}
{$define read_implementation}

const
  Version = '1.2.5';

{ colors -------------------------------------------------------------------- }

type
  { pomiedzy ciGraph1 a ciGraphMax musza byc kolejne ciGraph* }
  TColorItem = (ciBG, ciMainXYLines, ciCrosshair,
    ciGrid1,      ciNumScale1,      ciNumbers1,
    ciGridPi,     ciNumScalePi,     ciNumbersPi,
    ciGridCustom, ciNumScaleCustom, ciNumbersCustom,
    ciGraph1, ciGraph2, ciGraph3);
  TColorScheme = array [TColorItem] of TCastleColor;
  PColorScheme = ^TColorScheme;

const
  ciGraphMax = ciGraph3;
  IloscGraphKol = Ord(ciGraphMax)-Ord(ciGraph1)+1;

  { zestawy kolorow : }
  ColorSchemeDark: TColorScheme =
  ( (0.0 , 0.0 , 0.0 , 1.0),
    (0.33, 0.33, 1.0 , 1.0),
    (1.0 , 1.0 , 1.0 , 1.0),
    (0.0 , 0.33, 0.0 , 1.0),
    (0.0 , 0.33, 0.0 , 1.0),
    (0.0 , 0.66, 0.0 , 1.0),
    (0.66, 0.16, 0.0 , 1.0),
    (0.66, 0.16, 0.0 , 1.0),
    (0.66, 0.33, 0.0 , 1.0),
    (0   , 0.0 , 0.66, 1.0),
    (0   , 0.0 , 0.66, 1.0),
    (0.33, 0.33, 1.0 , 1.0),
    (1.0 , 1.0 , 0.33, 1.0),
    (1.0 , 0.0 , 0.0 , 1.0),
    (0.66, 0.66, 0.66, 1.0));
  ColorSchemeLight: TColorScheme =
  ( (1.0 , 1.0 , 1.0 , 1.0),
    (0.0 , 0.0 , 0.66, 1.0),
    (0.0 , 0.66, 0.0 , 1.0),
    (1.0 , 1.0 , 0.33, 1.0),
    (1.0 , 1.0 , 0.33, 1.0),
    (0.0 , 0.66, 0.0 , 1.0),
    (1.0 , 0.5 , 0.0 , 1.0),
    (1.0 , 0.5 , 0.0 , 1.0),
    (0.66, 0.33, 0.0 , 1.0),
    (1.0 , 0.5 , 0.0 , 1.0),
    (1.0 , 0.5 , 0.0 , 1.0),
    (1.0 , 0.5 , 0.0 , 1.0),
    (0.0 , 0.0 , 0.0 , 1.0),
    (0.0 , 0.66, 0.0 , 1.0),
    (0.66, 0.66, 0.66, 1.0));

var
  ColorScheme: PColorScheme = @ColorSchemeDark;

{ TGraph --------------------------------------------------------------------- }

type
  TXY = record
    Break: boolean; { jezeli Break to pola x, y sa bez znaczenia }
    x, y: Float;     { these must be float to work with DeFormat %f }
  end;
  PXY = ^TXY;

  TXYList = specialize TGenericStructList<TXY>;

type
  TGraph = class
  private
    FVisible: boolean;
    procedure SetVisible(const Value: boolean);
    procedure CreateCommon(AColorNumber: Integer);
  public
    MenuItem: TMenuItemChecked;
    Points: TXYList;
    Color: TCastleColor;
    Name: string;
    property Visible: boolean read FVisible write SetVisible;

    { Initialize TGraph reading Points from file URL.
      URL = '-' means stdin.

      Graph name will be taken from URL, or (if line "name=..." is
      present in PointsFile) then it will be used.

      @param AColorNumber color number (see TColorItem), counted from 0. }
    constructor CreateFromFile(const URL: string;
      AColorNumber: Integer);

    { Initialize TGraph using function Expression. }
    constructor CreateFromExpression(const Expression: string;
      const X1Str, X2Str, XStepStr: string;
      AColorNumber: Integer);

    destructor Destroy; override;
  end;

procedure TGraph.CreateCommon(AColorNumber: Integer);
begin
  { calculate Color }
  if AColorNumber < IloscGraphKol then
    Color := ColorScheme^[TColorItem(Ord(ciGraph1) + AColorNumber)] else
  repeat
    Color := Vector4Single(Random, Random, Random, 1.0);
    { Don't allow too dark colors, as they are not visible... }
  until GrayscaleValue(Color) >= 0.2;

  { initialize Points }
  Points := TXYList.Create;

  FVisible := true;
end;

constructor TGraph.CreateFromExpression(const Expression: string;
  const X1Str, X2Str, XStepStr: string; AColorNumber: Integer);
var
  I: Integer;
  Expr: TCasScriptExpression;
  X1, X2, XStep: Extended;
  X, Y: TCasScriptFloat;
begin
  inherited Create;
  CreateCommon(AColorNumber);

  X1 := ParseConstantFloatExpression(X1Str);
  X2 := ParseConstantFloatExpression(X2Str);
  XStep := ParseConstantFloatExpression(XStepStr);

  Name := Expression;

  X := TCasScriptFloat.Create(false);
  try
    X.Name := 'x';
    Expr := ParseFloatExpression(Expression, [X]);
    try
      Points.Count := Floor((X2 - X1) / XStep);
      for I := 0 to Points.Count - 1 do
      begin
        { calculate each time X as X1 + I * XStep, this is numerically stable
          (contrary to X += XStep each time. that cummulates errors)  }
        X.Value := X1 + I * XStep;
        Y := Expr.TryExecuteMath as TCasScriptFloat;

        Points.L[I].X := X.Value;
        Points.L[I].Break := Y = nil;
        if Y <> nil then
          Points.L[I].Y := Y.Value;
      end;
    finally FreeAndNil(Expr) end;
  finally FreeAndNil(X) end;
end;

constructor TGraph.CreateFromFile(const URL: string; AColorNumber: Integer);

  procedure CreateFromReader(PointsFile: TTextReader; const AName: string);
  var line: string;
      xy: TXY;
  const SNameLine = 'name=';
        SBreakLine = 'break';
  begin
   Name := AName;

   { load Points from PointsFile }
   while not PointsFile.Eof do
   begin
    line := PointsFile.Readln;
    line := Trim(line);

    if (line = '') or (line[1] = '#') then
     {to jest komentarz; wiec nic nie rob} else
    if IsPrefix(SNameLine, line) then
    begin
     Name := SEnding(line, Length(SNameLine)+1); {to jest name=}
    end else
    begin
     xy.Break := SameText(line, SBreakLine);
     { stworz nowy punkt, chyba ze to jest linia z breakiem i (poprzednia linia
       tez byla z breakiem lub nie bylo poprzednej linii) }
     if not (xy.Break and ((Points.Count = 0) or Points.Last.Break) ) then
     begin
      if not xy.Break then DeFormat(line, '%f %f', [@xy.x, @xy.y]);
      Points.Add(xy);
     end;
    end;
   end;
  end;

var
  Reader: TTextReader;
begin
 inherited Create;
 CreateCommon(AColorNumber);

 if URL = '-' then
  CreateFromReader(StdInReader, 'stdin') else
 begin
  Reader := TTextReader.Create(URL);
  try
   CreateFromReader(Reader, URL);
  finally FreeAndNil(Reader) end;
 end;
end;

destructor TGraph.Destroy;
begin
 FreeAndNil(Points);
 inherited;
end;

procedure TGraph.SetVisible(const Value: boolean);
begin
 FVisible := Value;
 if MenuItem <> nil then MenuItem.Checked := Value;
end;

type
  TGraphList = specialize TFPGObjectList<TGraph>;

var Graphs: TGraphList;

{ BoolOptions -------------------------------------------------------- }

type
  TBoolOption=
  ( boCrosshair, boPointsCoords, boMainXYLines, boMap,
    boGrid1,      boNumScale1,      boNumbers1,
    boGridPi,     boNumScalePi,     boNumbersPi,
    boGridCustom, boNumScaleCustom, boNumbersCustom,
    boOnlyPoints);

var
  { BoolOptions przechowuje aktualny stan kazdej bool opcji.
    Tutaj podajemy tez wartosci startowe tych opcji. }
  BoolOptions: array[TBoolOption]of boolean =
  ( false, false, true, true,
    true, false, false,
    false, false, false,
    false, false, false,
    false);

const
  { BoolOptionsKeys decyduja o przelaczaniu opcji klawiszami }
  BoolOptionsKeys: array[TBoolOption]of char =
  ( 'c', 'p', 'l', 'm',
    'g', 's', 'n',
    'G', 'S', 'N',
    CtrlG, CtrlS, CtrlN,
    'o' );
  BoolOptionsMenuNames: array[TBoolOption]of string =
  ( 'Crosshair', 'Point Coordinates', 'Main XY lines', 'Map',
    'Grid 1',      'NumScale 1',      'Numbers 1',
    'Grid Pi',     'NumScale Pi',     'Numbers Pi',
    'Grid Custom', 'NumScale Custom', 'Numbers Custom',
    'Only Points');

{ inne zmienne globalne -------------------------------------------------- }

var
  Window: TCastleWindowCustom;

  CustomSize: TGLfloat = 2.5;

  { ponizej rzeczy ktore sa bezposrednio wykorzystywane w Render.
    Ich wartosci poczatkowe sa ustalane w HomeState. }
  MoveX, MoveY: TGLfloat;
  ScaleX, ScaleY: TGLfloat;

{ global funcs ---------------------------------------------------------- }

{ Reset view properties to default, such that all Graphs are visible. }
procedure HomeState;
var MinX, MaxX, MinY, MaxY: Float;
    MiddleX, MiddleY, SizeX, SizeY: Float;
    i, j: Integer;
begin
 MinX := MaxFloat;
 MinY := MaxFloat;
 MaxX := -MaxFloat;
 MaxY := -MaxFloat;
 for i := 0 to Graphs.Count-1 do
  for j := 0 to Graphs[i].Points.Count-1 do
  begin
   MinX := CastleUtils.Min(MinX, Graphs[i].Points.L[j].x);
   MinY := CastleUtils.Min(MinY, Graphs[i].Points.L[j].y);
   MaxX := CastleUtils.Max(MaxX, Graphs[i].Points.L[j].x);
   MaxY := CastleUtils.Max(MaxY, Graphs[i].Points.L[j].y);
  end;

 if MinX = MaxFloat then
 begin
  { This means that there was no points (with Break = false)
    in graphs (maybe there was no graphs at all) !
    So we set default MoveXY (look at the middle) and ScaleXY. }
  MoveX := Window.Width/2;
  MoveY := Window.Height/2;
  ScaleX := 1;
  ScaleY := 1;
 end else
 begin
  SizeX := MaxX - MinX;
  SizeY := MaxY - MinY;

  { When ScaleX/Y = 1, we see Window.Width/Height pixels.
    But we want to see SizeX/Y pixels.
    Also we don't want ScaleX/Y to be too little (this makes a lot of trouble,
    and OpenGL rendering also has trouble and crawls surprisingly slowly).

    By default we set ScaleX and ScaleY to be equal, as this is most
    natural for user. }
  ScaleX := CastleUtils.Max(CastleUtils.Min(Window.Width / SizeX, Window.Height / SizeY), 0.01);
  ScaleY := ScaleX;

  MiddleX := (MinX + MaxX) / 2;
  MiddleY := (MinY + MaxY) / 2;

  { MoveX/Y to look at MiddleX/Y.
    I want XWindowToUklad(Window.Width/2) = MiddleX.
    Solve equation (Window.Width/2-MoveX)/ScaleX = MiddleX for MoveX and you got
    what you need. }
  MoveX := Window.Width /2 - MiddleX * ScaleX;
  MoveY := Window.Height/2 - MiddleY * ScaleY;
 end;

 Window.PostRedisplay;
end;

{ funkcje XYWindowToUklad pobieraja pozycje we wspolrzednej okna OpenGL'a
  (czyli 0..Window.Width lub Window.Height)
  i zwracaja jaka jest pozycja punktu na wykresie
  w tym miejscu okna. }
function XWindowToUklad(WindowX: TGLfloat): TGLfloat;
begin result := (WindowX - MoveX) / ScaleX end;

function YWindowToUklad(WindowY: TGLfloat): TGLfloat;
begin result := (WindowY - MoveY) / ScaleY end;

function XUkladToWindow(UkladX: TGLfloat): TGLfloat;
begin result := UkladX * ScaleX + MoveX end;

function YUkladToWindow(UkladY: TGLfloat): TGLfloat;
begin result := UkladY * ScaleY + MoveY end;

function WindowPosUklad(const X, Y: Extended): TVector2Integer;
begin
  Result := Vector2Integer(Round(XUkladToWindow(X)), Round(YUkladToWindow(Y)));
end;

var
  GraphsListMenu: TMenu;

procedure UpdateGraphsMenu;
var
  I: Integer;
  CharKey: char;
const
  GraphsListPrefixNumber = 5;
begin
  while GraphsListMenu.Count > GraphsListPrefixNumber do
    GraphsListMenu.Delete(GraphsListPrefixNumber);

  for I := 0 to Graphs.Count-1 do
  begin
    if I < 10 then
      CharKey := DigitAsChar(i) else
      CharKey := #0;
    Graphs[i].MenuItem := TMenuItemChecked.Create(
      'Graph "' + SQuoteMenuEntryCaption(Graphs[i].Name) + '"',
      I + 1000, CharKey, Graphs[i].Visible,
      false {false, because TGraph.SetVisible will handle this anyway});
    GraphsListMenu.Append(Graphs[i].MenuItem);
  end;
end;

{ This creates new TGraph instance using CreateFromFile
  and adds it to Graphs list.
  If loading graph from file fails, it will display nice MessageOK dialog.

  It also takes care of setting AColorNumer parameter for TGraph.Create
  as it should be. }
procedure GraphsAddFromFile(const URL: string);
var
  G: TGraph;
begin
  try
    G := TGraph.CreateFromFile(URL, Graphs.Count);
  except
    on E: Exception do
    begin
      MessageOK(Window, Format('Error when opening graph from file "%s": %s',
        [URL, E.Message]));
      Exit;
    end;
  end;

  Graphs.Add(G);
end;

{ This creates new TGraph instance using CreateFromExpression
  and adds it to Graphs list.
  If parsing expression fails, it will display nice MessageOK dialog.

  It also takes care of setting AColorNumer parameter for TGraph.Create
  as it should be. }
procedure GraphsAddFromExpression(const Expression: string;
  const X1, X2, XStep: string);
var
  G: TGraph;
begin
  try
    G := TGraph.CreateFromExpression(Expression, X1, X2, XStep, Graphs.Count);
  except
    on E: ECasScriptSyntaxError do
    begin
      MessageOK(Window, Format(
        'Error when parsing function expression at position %d: %s',
        [E.LexerTextPos, E.Message]));
      Exit;
    end;
    on E: ECasScriptError do
    begin
      MessageOK(Window, Format(
        'Error %s in function expression: %s',
        [E.ClassName, E.Message]));
      Exit;
    end;
  end;

  Graphs.Add(G);
end;

{ registered window callbacks -------------------------------------------------- }

procedure Render(Container: TUIContainer);

  procedure ShowGridNumScale(
    const krok: extended; const LiczbowyString: string;
    showGrid, showNumScale, showNumbers: boolean;
    const gridKol, NumScaleKol, NumbersKol: TCastleColor);
  { ShowGridNumScale draws grid, numbers scale and numbers.
    Odpowiednie parametry showXxx okreslaja co dokladnie narysowac -
    dowolna kombinacja trzech powyzszych elementow moze byc wiec narysowana.
    Parametry xxxKol okreslaja kolory odpowiednich elementow.
    Parametr LiczbowyString ma zastosowanie tylko do rysowania podzialki liczbowej.
    Przy kazdej kresce na osi bedzie pisany tekst Format(LiczbowyString, [i, i*krok])
     gdzie i to wielokrotnosc kroku o jaka dana kreska jest wysunieta.
     Przykladowe wartosci LiczbowyTekst to '%d', '%d*Pi', '%1: f'. }
  var i, minx, maxx, miny, maxy: integer;
  const NumScaleLength =10;
  begin
   if not (showGrid or showNumScale or showNumbers) then exit;

   { minx to najmniejsza wartosc taka ze minx*krok miesci sie na ekranie.
     Podobnie maxx, miny, maxy. }
   minx := Ceil(XWindowToUklad(0) / krok);
   miny := Ceil(YWindowToUklad(0) / krok);
   maxx := Floor(XWindowToUklad(Window.Width ) / krok);
   maxy := Floor(YWindowToUklad(Window.Height) / krok);

   if ShowGrid then
   begin
    glColorv(gridKol);
    glBegin(GL_LINES);
    { przesuwajac sie o wartosc integer unikamy przy okazji kumulacji bledow
      zaokraglen zmiennoprzecinkowych gdybysmy przesuwali sie o krok
      zmieniajac jakas wartosc o +krok w kazdym kroku petli. }
    for i := minx to maxx do
    begin
     glVertex2f(i*krok, YWindowToUklad(0));
     glVertex2f(i*krok, YWindowToUklad(Window.Height));
    end;
    for i := miny to maxy do
    begin
     glVertex2f(XWindowToUklad(0), i*krok); glVertex2f(XWindowToUklad(Window.Width), i*krok);
    end;
    glEnd;
   end;

   if ShowNumScale then
   begin
    glColorv(NumScaleKol);
    glBegin(GL_LINES);
    { przesuwajac sie o wartosc integer unikamy przy okazji kumulacji bledow
      zaokraglen zmiennoprzecinkowych gdybysmy przesuwali sie o krok
      zmieniajac jakas wartosc o +krok w kazdym kroku petli. }
    for i := minx to maxx do
    begin
     glVertex2f(i*krok, -NumScaleLength/ScaleY);
     glVertex2f(i*krok,+ NumScaleLength/ScaleY);
    end;
    for i := miny to maxy do
    begin
     glVertex2f(-NumScaleLength/ScaleX, i*krok);
     glVertex2f(+NumScaleLength/ScaleX, i*krok);
    end;
    glEnd;
   end;

   if ShowNumbers then
   begin
     for i := minx to maxx do
       UIFont.Print(WindowPosUklad(i*krok, 0), NumbersKol,
         Format(LiczbowyString, [i, i*krok]));
     for i := miny to maxy do
       UIFont.Print(WindowPosUklad(0, i*krok), NumbersKol,
         Format(LiczbowyString, [i, i*krok]));
   end;
  end;

  procedure ShowGraph(const Graph: TGraph);
  var i: integer;
  begin
   with Graph do
   if Visible then
   begin
    glColorv(Color);
    if BoolOptions[boOnlyPoints] then
    begin
     glPointSize(3);
     glBegin(GL_POINTS);
     for i := 0 to Points.Count-1 do
      if not points.L[i].break then
       glVertex2f(points.L[i].x, points.L[i].y);
     glEnd;
     glPointSize(1);
    end else
    begin
     glBegin(GL_LINE_STRIP);
      for i := 0 to Points.Count-1 do
       if points.L[i].break then
       begin
        glEnd;
        glBegin(GL_LINE_STRIP);
       end else
        glVertex2f(points.L[i].x, points.L[i].y);
     glEnd;
    end;

    if BoolOptions[boPointsCoords] then
    begin
      for i := 0 to Points.Count-1 do
        if not points.L[i].break then
          UIFont.Print(WindowPosUklad(points.L[i].x, points.L[i].y), Color,
            Format('(%f,%f)', [points.L[i].x, points.L[i].y]));
    end;
   end;
  end;

var
  i, j: integer;
  TextY: integer;
  S: string;
begin
 GLClear([cbColor], ColorScheme^[ciBG]);
 glLoadIdentity;

 glTranslatef(MoveX, MoveY, 0);
 glScalef(ScaleX, ScaleY, 0);

 ShowGridNumScale(1, '%d',
   BoolOptions[boGrid1],   BoolOptions[boNumScale1],   BoolOptions[boNumbers1],
   ColorScheme^[ciGrid1],  ColorScheme^[ciNumScale1],  ColorScheme^[ciNumbers1]);
 ShowGridNumScale(Pi, '%d*Pi',
   BoolOptions[boGridPi],  BoolOptions[boNumScalePi],  BoolOptions[boNumbersPi],
   ColorScheme^[ciGridPi], ColorScheme^[ciNumScalePi], ColorScheme^[ciNumbersPi]);
 ShowGridNumScale(CustomSize, '%1:f',
   BoolOptions[boGridCustom],  BoolOptions[boNumScaleCustom],  BoolOptions[boNumbersCustom],
   ColorScheme^[ciGridCustom], ColorScheme^[ciNumScaleCustom], ColorScheme^[ciNumbersCustom]);

 if BoolOptions[boMainXYLines] then
 begin
  glColorv(ColorScheme^[ciMainXYLines]);
  glBegin(GL_LINES);
   glVertex2f(0, YWindowToUklad(0)); glVertex2f(0, YWindowToUklad(Window.Height));
   glVertex2f(XWindowToUklad(0), 0); glVertex2f(XWindowToUklad(Window.Width), 0);
  glEnd;
 end;

 for j := 0 to Graphs.Count-1 do ShowGraph(Graphs[j]);

 if BoolOptions[boCrosshair] then
 begin
  glLoadIdentity;
  glColorv(ColorScheme^[ciCrosshair]);

  glLineStipple(10, $AAAA);
  glEnable(GL_LINE_STIPPLE);
  glBegin(GL_LINES);
   glVertex2f(Window.Width/2, 0); glVertex2f(Window.Width/2, Window.Height);
   glVertex2f(0, Window.Height/2); glVertex2f(Window.Width, Window.Height/2);
  glEnd;
  glDisable(GL_LINE_STIPPLE);

  UIFont.Print(Window.Width div 2, Window.Height div 2,
    ColorScheme^[ciCrosshair],
    Format('%f, %f', [XWindowToUklad(Window.Width/2), YWindowToUklad(Window.Height/2)]));
 end;

 if BoolOptions[boMap] then
 begin
  glLoadIdentity;

  TextY := 10;
  UIFont.Print(10, TextY, ColorScheme^[ciCrosshair],
    Format('Scale %f, %f. Move %f, %f. %d graphs.',
    [ScaleX, ScaleY, MoveX, MoveY, Graphs.Count]));

  for i := 0 to Graphs.Count-1 do
  begin
   glColorv(Graphs[i].Color);
   TextY += UIFont.RowHeight + 2;

   glBegin(GL_LINES);
     glVertex2f(5 , TextY + UIFont.RowHeight div 2);
     glVertex2f(15, TextY + UIFont.RowHeight div 2);
   glEnd;
   S := Format('%d - %s', [i, Graphs[i].Name]);
   if not Graphs[i].Visible then
     S := '{' + S + '}';
   UIFont.Print(20, TextY, Graphs[i].Color, S);
  end;
 end;
end;

var
  UpdateFirst: boolean = true;

procedure Update(Container: TUIContainer);

  function SpeedFactor: TGLfloat;
  begin
   Result := Window.Fps.UpdateSecondsPassed * 50; { to make everything time-based }
   if Window.Pressed[K_Ctrl] then Result *= 10;
  end;

  procedure MultiplyGLScale(Multiplier: TGLfloat);
  begin
    Multiplier := Power(Multiplier, SpeedFactor);
    if (ScaleX * Multiplier < 0.00001) or
       (ScaleY * Multiplier < 0.00001) then
      Exit;

    ScaleX *= Multiplier;
    ScaleY *= Multiplier;

    { przy kazdym skalowaniu robiac tez odpowiednie przesuniecie zapewniamy sobie
      ze po wykonaniu skalowania bedziemy stac w tym samym miejscu Graphu.
      Innymi slowy, "tak naprawde" wykonujemy kiepskie skalowanie wzgledem srodka
      ukladu wspolrzednych (0, 0) ale potem odpowiednio sie przesuwamy tak aby wrocic
      tak gdzie bylismy !. }
    MoveX:=(MoveX-Window.Width/2 )*Multiplier + Window.Width /2;
    MoveY:=(MoveY-Window.Height/2)*Multiplier + Window.Height/2;

    Window.PostRedisplay;
  end;

  procedure MultiplyGLScaleX(Multiplier: TGLfloat);
  begin
    Multiplier := Power(Multiplier, SpeedFactor);
    if ScaleX * Multiplier < 0.00001 then
      Exit;

    ScaleX *= Multiplier;

    MoveX := (MoveX-Window.Width/2) * Multiplier + Window.Width/2;

    Window.PostRedisplay;
  end;

  procedure MultiplyGLScaleY(Multiplier: TGLfloat);
  begin
    Multiplier := Power(Multiplier, SpeedFactor);
    if ScaleY * Multiplier < 0.00001 then
      Exit;

    ScaleY *= Multiplier;

    MoveY := (MoveY-Window.Height/2) * Multiplier + Window.Height/2;

    Window.PostRedisplay;
  end;

  procedure AddGL(var Value: TGLfloat; const Change: TGLfloat);
  begin
   Value += Change * SpeedFactor;
   Window.PostRedisplay;
  end;

  { Interpretuje wszystkie pozostale ParStr(1) .. ParStr(ParCount) jako
    nazwy plikow (lub stdin jesli sa '-').
    Laduje z nich graphs, dodajac je do Graphs list. }
  procedure OpenGraphsFromParameters;
  var
    I: integer;
  begin
    for I := 1 to Parameters.High do
      GraphsAddFromFile(Parameters[I]);
  end;

begin
  if UpdateFirst then
  begin
    OpenGraphsFromParameters;
    UpdateGraphsMenu;
    HomeState;
    UpdateFirst := false;
  end;

 with Window do begin
  if Pressed[K_Up] then AddGL(MoveY, -1);
  if Pressed[K_Down] then AddGL(MoveY, +1);

  if Pressed[K_Right] then AddGL(MoveX, -1);
  if Pressed[K_Left] then AddGL(MoveX, +1);

  if Pressed[K_Numpad_Minus] or Pressed[K_Minus] or Pressed.Characters['-'] then
    MultiplyGLScale(1 / 1.1);
  if Pressed[K_Numpad_Plus ] or Pressed[K_Plus ] or Pressed.Characters['+'] then
    MultiplyGLScale(1.1);

  if Pressed[K_X] then
  begin
    if Pressed[K_Shift] then
      MultiplyGLScaleX(1 / 1.1) else
      MultiplyGLScaleX(1.1);
  end;

  if Pressed[K_Y] then
  begin
    if Pressed[K_Shift] then
      MultiplyGLScaleY(1 / 1.1) else
      MultiplyGLScaleY(1.1);
  end;
 end;
end;

procedure MouseMove(Container: TUIContainer; newX, newY: integer);
begin
 if mbLeft in Window.mousePressed then
 begin
  { zmien MoveX i MoveY o tyle o ile zmienila sie pozycja myszy od
    ostatniego MouseMove/Down }
  MoveX := MoveX + (newX-Window.MouseX);
  MoveY := MoveY - (newY-Window.MouseY); { y jest mierzone w przeciwna strone, stad minus }
  Window.PostRedisplay;
 end;
end;

{ menu-related things -------------------------------------------------------- }

var
  OpenURL: string = '';
  RecentMenu: TWindowRecentFiles;

{ This opens/adds graph from file. }
procedure OpenOrAddGraphFromFileCore(Open: boolean; const URL: string);
begin
  if Open then
  begin
    Graphs.Clear;
    GraphsAddFromFile(URL);
    UpdateGraphsMenu;
    HomeState;
  end else
  begin
    GraphsAddFromFile(URL);
    UpdateGraphsMenu;
    { Calling HomeState is not desirable here, maybe user wants to keep
      previous view settings, to see already existing graphs as they were. }
  end;

  RecentMenu.Add(URL);
  OpenURL := URL;
end;

type
  THelper = class
    class procedure OpenRecent(const URL: string);
  end;

class procedure THelper.OpenRecent(const URL: string);
begin
  OpenOrAddGraphFromFileCore(false, URL);
end;

function GetMainMenu(): TMenu;
var
  M, M2: TMenu;
  bo: TBoolOption;
  NextRecentMenuItem: TMenuEntry;
begin
 Result := TMenu.Create('Main menu');
 M := TMenu.Create('_File');
   M.Append(TMenuItem.Create('_Open Graph from File ...', 101, CtrlO));
   M.Append(TMenuItem.Create('_Add Graph from File ...', 102, CtrlA));
   NextRecentMenuItem := TMenuSeparator.Create;
   M.Append(NextRecentMenuItem);
   RecentMenu.NextMenuItem := NextRecentMenuItem;
   M.Append(TMenuItem.Create('_Exit',      10, CharEscape));
   Result.Append(M);
 M := TMenu.Create('F_unctions');
   M.Append(TMenuItem.Create('_Open Graph with Function ...', 201));
   M.Append(TMenuItem.Create('_Add Graph with Function ...', 202));
   M.Append(TMenuSeparator.Create);
   M2 := TMenu.Create('Add Graph with _Sample Function');
     M2.Append(TMenuItem.Create('_sin(x) (on [-10 * Pi, 10 * Pi], with 0.1 step)', 203));
     M2.Append(TMenuItem.Create('_cos(x) (on [-10 * Pi, 10 * Pi], with 0.1 step)', 204));
     M2.Append(TMenuItem.Create('sin(x) > cos(x) (on [-10 * Pi, 10 * Pi], with 0.1 step)', 205));
   M.Append(M2);
   Result.Append(M);
 M := TMenu.Create('_Graphs');
   M.Append(TMenuItem.Create('_Hide All Graphs',      30));
   M.Append(TMenuItem.Create('_Show All Graphs',      31));
   M.Append(TMenuSeparator.Create);
   M.Append(TMenuItem.Create('_Close All Graphs', 103));
   M.Append(TMenuSeparator.Create);
   GraphsListMenu := M;
   Result.Append(M);
 M := TMenu.Create('_View');
   for bo := Low(bo) to High(bo) do
    M.Append(TMenuItemChecked.Create(
      BoolOptionsMenuNames[bo], 900+Ord(bo), BoolOptionsKeys[bo],
      BoolOptions[bo], true));
   Result.Append(M);
 M := TMenu.Create('_Other');
   M.Append(TMenuItem.Create('_Restore Default View',     21, K_Home));
   M.Append(TMenuItemToggleFullScreen.Create(Window.FullScreen));
   M.Append(TMenuItem.Create('_Save Screen ...',       23, K_F5));
   Result.Append(M);
 M := TMenu.Create('_Help');
   M.Append(TMenuItem.Create('Help About Controls (Keys and Mouse)', 5, K_F1));
   M.Append(TMenuSeparator.Create);
   M.Append(TMenuItem.Create('About glplotter'                     , 6));
   Result.Append(M);
end;

var
  LastInputX1: string = '0';
  LastInputX2: string = '1';
  LastInputXStep: string = '0.1';

procedure MenuClick(Container: TUIContainer; Item: TMenuItem);

  procedure SetVisibleAll(Value: boolean);
  var i: Integer;
  begin
   for i := 0 to Graphs.Count - 1 do
    Graphs[i].Visible := Value;
   Window.PostRedisplay;
  end;

  procedure OpenOrAddGraphFromFile(Open: boolean);
  var
    URL, S: string;
  begin
    URL := OpenURL;
    if Open then
      S := 'Open graph from file' else
      S := 'Add graph from file';
    if Window.FileDialog(S, URL, true) then
      OpenOrAddGraphFromFileCore(Open, URL);
  end;

  function GetExpression(out Expression: string;
    out X1, X2, XStep: string): boolean;
  const
    SYouCan = nl + nl + '(You can use any constant mathematical expression here, ' +
      'e.g. try "10 * Pi")';
  begin
    Expression := '';

    X1 := LastInputX1;
    X2 := LastInputX2;
    XStep := LastInputXStep;

    Result :=
      MessageInputQuery(Window,
        'Function expression :' + nl + nl +
        '(where x is the function argument, e.g. "x * 2")',
        Expression) and
      MessageInputQuery(Window, 'First X value :' + SYouCan, X1) and
      MessageInputQuery(Window, 'Last X value :'  + SYouCan, X2) and
      MessageInputQuery(Window, 'X value step :'  + SYouCan, XStep);

    LastInputX1 := X1;
    LastInputX2 := X2;
    LastInputXStep := XStep;
  end;

  procedure OpenGraphFromExpression;
  var
    Expression: string;
    X1, X2, XStep: string;
  begin
    if GetExpression(Expression, X1, X2, XStep) then
    begin
      Graphs.Clear;
      GraphsAddFromExpression(Expression, X1, X2, XStep);
      UpdateGraphsMenu;
      HomeState;
    end;
  end;

  procedure AddGraphFromExpression;
  var
    Expression: string;
    X1, X2, XStep: string;
  begin
    if GetExpression(Expression, X1, X2, XStep) then
    begin
      GraphsAddFromExpression(Expression, X1, X2, XStep);
      UpdateGraphsMenu;
      { Calling HomeState is not desirable here, maybe user wants to keep
        previous view settings, to see already existing graphs as they were. }
    end;
  end;

  procedure AddSampleGraphFromExpression(const Expression: string;
    const X1, X2, XStep: string);
  begin
    GraphsAddFromExpression(Expression, X1, X2, XStep);
    UpdateGraphsMenu;
    { Calling HomeState is not desirable here, maybe user wants to keep
      previous view settings, to see already existing graphs as they were. }
  end;

  procedure CloseAllGraphs;
  begin
    Graphs.Clear;
    UpdateGraphsMenu;
    HomeState;
  end;

var bo: TBoolOption;
begin
 case Item.IntData of
  5:  MessageOK(Window,
        ['Keys to change view (not available as menu items):',
         '  arrows      : move',
         '  +/-         : scale',
         '  x/X         : scale only horizontally',
         '  y/Y         : scale only vertically',
         'Hold down Ctrl to make these keys work 10x faster. ',
         '',
         'You can also move view by dragging while holding left mouse button.']);
  6:  MessageOK(Window,
        [ 'glplotter: plotting graphs, of functions and others.',
          'Version ' + Version,
          'By Michalis Kamburelis.',
          '',
          '[http://castle-engine.sourceforge.net/glplotter_and_gen_function.php]',
          '',
          'Compiled with ' + SCompilerDescription +'.' ]);
  10: Window.Close;

  21: HomeState;
  23: Window.SaveScreenDialog(FileNameAutoInc('glplotter_screen_%d.png'));

  30: SetVisibleAll(false);
  31: SetVisibleAll(true);

  101: OpenOrAddGraphFromFile(true);
  102: OpenOrAddGraphFromFile(false);
  103: CloseAllGraphs;

  201: OpenGraphFromExpression;
  202: AddGraphFromExpression;
  203: AddSampleGraphFromExpression('sin(x)'         , '-10 * Pi', '10 * Pi', '0.1');
  204: AddSampleGraphFromExpression('cos(x)'         , '-10 * Pi', '10 * Pi', '0.1');
  205: AddSampleGraphFromExpression('sin(x) > cos(x)', '-10 * Pi', '10 * Pi', '0.1');

  900..999:
    begin
     bo := TBoolOption(Item.IntData-900);
     BoolOptions[bo] := not BoolOptions[bo];
    end;
  1000..2000:
    begin
     with Graphs[Item.IntData-1000] do Visible := not Visible;
     Window.PostRedisplay;
    end;
  else Exit;
 end;
 Window.PostRedisplay;
end;

{ params parsing ------------------------------------------------------------ }

const
  DisplayApplicationName = 'glplotter';
  Options: array[0..4] of TOption = (
    (Short: 'h'; Long: 'help'; Argument: oaNone),
    (Short: 'c'; Long: 'custom-size'; Argument: oaRequired),
    (Short:  #0; Long: 'light'; Argument: oaNone),
    (Short:  #0; Long: 'dark'; Argument: oaNone),
    (Short: 'v'; Long: 'version'; Argument: oaNone)
  );

procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
begin
  case OptionNum of
    0:begin
        InfoWrite(
          'glplotter: plot lines.' +nl+
          nl+
          'Call as' +nl+
          '  glplotter [OPTION]... [FILE]...' +nl+
          'Each FILE is filename (actually, an URL) or "-" (meaning stdin).' +nl+
          nl+
          'Available options are:' +nl+
          HelpOptionHelp +nl+
          VersionOptionHelp +nl+
          '  --light               Set color scheme to light' +nl+
          '  --drak                Set color scheme to dark' +nl+
          '  --custom-size / -c SIZE' +nl+
          '                        Set size of custom grid' +nl+
          nl+
          TCastleWindowCustom.ParseParametersHelp(StandardParseOptions, true) +nl+
          nl+
          SCastleEngineProgramHelpSuffix(DisplayApplicationName, Version, true));
        ProgramBreak;
      end;
    1:CustomSize := StrToFloat(Argument);
    2:ColorScheme := @ColorSchemeLight;
    3:ColorScheme := @ColorSchemeDark;
    4:begin
        WritelnStr(Version);
        ProgramBreak;
      end;
    else raise EInternalError.Create('OptionProc');
  end;
end;

{ main ------------------------------------------------------------ }

function MyGetApplicationName: string;
begin
  Result := 'glplotter';
end;

begin
  OnGetApplicationName := @MyGetApplicationName;

  Window := TCastleWindowCustom.Create(Application);

  { initialize RecentMenu }
  RecentMenu := TWindowRecentFiles.Create(nil);
  try
    RecentMenu.OnOpenRecent := @THelper(nil).OpenRecent;

    Config.Load;

    Graphs := TGraphList.Create(true);
    try
      { parse parameters }
      Window.ParseParameters;
      Parameters.Parse(Options, @OptionProc, nil);

      { basic glw callbacks }
      Window.OnUpdate := @Update;
      Window.OnResize := @Resize2D;
      Window.OnMouseMove := @MouseMove;
      Window.OnRender := @Render;

      { setup menu }
      Window.MainMenu := GetMainMenu;
      Window.OnMenuClick := @MenuClick;

      { other glw options }
      Window.Caption := 'glplotter';

      Theme.MessageInputTextColor := Vector4Single(0, 0.4, 0, 1.0);
      Theme.MessageTextColor := Black;
      Theme.Images[tiWindow] := WindowGray;

      Window.Open;

      Application.Run;
    finally FreeAndNil(Graphs) end;

    Config.Save;
  finally
    FreeAndNil(RecentMenu);
  end;
end.
