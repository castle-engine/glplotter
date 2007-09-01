{
  Copyright 2001-2007 Michalis Kamburelis.

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
      genfunkcja 0 100 0.1 "x+1" | glplotter -
  zadziala zgodnie z oczekiwaniami.
  To jest zreszta skrypt glplotter_funkcja.
}

{$apptype GUI}

uses
  SysUtils, OpenGLh, GLWindow, KambiUtils, KambiGLUtils, Math, Classes,
  KambiClassUtils, GLWinMessages, GLW_Demo, OpenGLBmpFonts,
  BFNT_BitstreamVeraSansMono_m16_Unit, ParseParametersUnit, VectorMath,
  KambiStringUtils, KambiFilesUtils;

{$define read_interface}
{$define read_implementation}

{ colors -------------------------------------------------------------------- }

type
  { pomiedzy ciGraph1 a ciGraphMax musza byc kolejne ciGraph* }
  TColorItem = (ciBG, ciOsieXY, ciCrosshair,
    ciGrid1,      ciPodzialka1,      ciLiczby1,
    ciGridPi,     ciPodzialkaPi,     ciLiczbyPi,
    ciGridCustom, ciPodzialkaCustom, ciLiczbyCustom,
    ciGraph1, ciGraph2, ciGraph3);
  TColorScheme = array[TColorItem]of PVector3f;
  PColorScheme = ^TColorScheme;

const
  ciGraphMax = ciGraph3;
  IloscGraphKol = Ord(ciGraphMax)-Ord(ciGraph1)+1;

  { zestawy kolorow : }
  ColorSchemeDark: TColorScheme=
  ( @Black3Single, @LightBlue3Single, @White3Single,
    @DarkGreen3Single, @DarkGreen3Single, @Green3Single,
    @DarkBrown3Single, @DarkBrown3Single, @Brown3Single,
    @Blue3Single, @Blue3Single, @LightBlue3Single,
    @Yellow3Single, @Red3Single, @LightGray3Single);
  ColorSchemeLight: TColorScheme=
  ( @White3Single, @Blue3Single, @Green3Single,
    @Yellow3Single, @Yellow3Single, @Green3Single,
    @Orange3Single, @Orange3Single, @Brown3Single,
    @Orange3Single, @Orange3Single, @Orange3Single,
    @Black3Single, @Green3Single, @LightGray3Single);

var
  ColorScheme: PColorScheme = @ColorSchemeDark;

{ TGraph --------------------------------------------------------------------- }

type
  TXY = record
    Break: boolean; { jezeli Break to pola x, y sa bez znaczenia }
    x, y: Float;     { these must be float to work with DeFormat %f }
  end;
  PXY = ^TXY;

  TDynArrayItem_1 = TXY;
  PDynArrayItem_1 = PXY;
  {$define DYNARRAY_1_IS_STRUCT}
  {$I DynArray_1.inc}
  type TDynXYArray = TDynArray_1;

type
  TGraph = class
  private
    FVisible: boolean;
    procedure SetVisible(const Value: boolean);
  public
    MenuItem: TMenuItemChecked;
    Points: TDynXYArray;
    Color: TVector3f;
    Name: string;
    property Visible: boolean read FVisible write SetVisible;

    { Initialize TGraph reading Points from file FileName.
      FileName = '-' means stdin.

      Graph name will be taken from FileName, or (if line "name=..." is
      present in PointsFile) then it will be used.

      @param AColorNumber color number (see TColorItem), counted from 0. }
    constructor Create(const FileName: string;
      AColorNumber: Integer); overload;
    destructor Destroy; override;
  end;

constructor TGraph.Create(const FileName: string; AColorNumber: Integer);

  procedure CreateFromReader(PointsFile: TTextReader; const AName: string;
    AColorNumber: Integer);
  var line: string;
      xy: TXY;
  const SNameLine = 'name=';
        SBreakLine = 'break';
  begin
   if AColorNumber < IloscGraphKol then
     Color := ColorScheme^[TColorItem(Ord(ciGraph1) + AColorNumber)]^ else
   repeat
     Color := Vector3f(Random, Random, Random);
     { Don't allow too dark colors, as they are not visible... }
   until BWColorValue(Color) >= 0.2;

   Points := TDynXYArray.Create;
   Points.AllowedCapacityOverflow := 100;

   Name := AName;
   FVisible := true;

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
     if not (xy.Break and ((Points.Count = 0) or Points.Items[Points.High].Break) ) then
     begin
      if not xy.Break then DeFormat(line, '%f %f', [@xy.x, @xy.y]);
      Points.AppendItem(xy);
     end;
    end;
   end;
  end;

var
  Reader: TTextReader;
begin
 inherited Create;

 if FileName = '-' then
  CreateFromReader(StdInReader, 'stdin', AColorNumber) else
 begin
  Reader := TTextReader.CreateFromFileStream(FileName);
  try
   CreateFromReader(Reader, FileName, AColorNumber);
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
  TObjectsListItem_1 = TGraph;
  {$I ObjectsList_1.inc}
  type TGraphsList = TObjectsList_1;

var Graphs: TGraphsList;

{ BoolOptions -------------------------------------------------------- }

{ TODO: good english names for osie, podzialka, liczby }

type
  TBoolOption=
  ( boCrosshair, boPointsCoords, boOsieXY, boMap,
    boGrid1,      boPodzialka1,      boLiczby1,
    boGridPi,     boPodzialkaPi,     boLiczbyPi,
    boGridCustom, boPodzialkaCustom, boLiczbyCustom,
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
  ( 'c', 'w', 'o', 'm',
    'g', 'p', 'l',
    'G', 'P', 'L',
    CtrlG, CtrlP, CtrlL,
    'q' );
  BoolOptionsParamsNames: array[TBoolOption]of string =
  ( 'crosshair', 'point-coords', 'osie-xy', 'map',
    'grid-1',      'podzialka-1',      'liczby-1',
    'grid-pi',     'podzialka-pi',     'liczby-pi',
    'grid-custom', 'podzialka-custom', 'liczby-custom',
    'only-points');
  BoolOptionsMenuNames: array[TBoolOption]of string =
  ( 'Crosshair', 'Point Coordinates', 'Osie XY', 'Map',
    'Grid 1',      'Podzialka 1',      'Liczby 1',
    'Grid Pi',     'Podzialka Pi',     'Liczby Pi',
    'Grid Custom', 'Podzialka Custom', 'LiczbyCustom',
    'Only Points');

{ inne zmienne globalne -------------------------------------------------- }

var
  WSize, HSize: TGLfloat; { rozmiary jakie ma okno we wspolrzednych OpenGL'a }

  CustomSize: TGLfloat = 2.5;

  { ponizej rzeczy ktore sa bezposrednio wykorzystywane w DrawGL.
    Ich wartosci poczatkowe sa ustalane w HomeState. }
  MoveX, MoveY: TGLfloat;
  ScaleX, ScaleY: TGLfloat;
  Rotate: TGLfloat;

  Font: TGLBitmapFont;

{ global funcs ---------------------------------------------------------- }

{ This resets some view properties to default, such that all Graphs are visible.
  This may ba called only after ResizeGL (it requires W/HSize set).

  Note that you shouldn't call this without user explicit request
  (e.g. you shouldn't call this from each EventResize, or even from each
  EventInit (because when we switch fullscreen on/off we're
  doing Close + Init again)). After all, user usually wants to preserve it's
  view properties. }
procedure HomeState;
var MinX, MaxX, MinY, MaxY: Float;
    MiddleX, MiddleY, SizeX, SizeY: Float;
    i, j: Integer;
begin
 Rotate := 0;

 MinX := MaxFloat;
 MinY := MaxFloat;
 MaxX := -MaxFloat;
 MaxY := -MaxFloat;
 for i := 0 to Graphs.Count-1 do
  for j := 0 to Graphs[i].Points.Count-1 do
  begin
   MinX := KambiUtils.Min(MinX, Graphs[i].Points.Items[j].x);
   MinY := KambiUtils.Min(MinY, Graphs[i].Points.Items[j].y);
   MaxX := KambiUtils.Max(MaxX, Graphs[i].Points.Items[j].x);
   MaxY := KambiUtils.Max(MaxY, Graphs[i].Points.Items[j].y);
  end;

 if MinX = MaxFloat then
 begin
  { This means that there was no points (with Break = false)
    in graphs (maybe there was no graphs at all) !
    So we set default MoveXY (look at the middle) and ScaleXY. }
  MoveX := WSize/2;
  MoveY := HSize/2;
  ScaleX := 1;
  ScaleY := 1;
 end else
 begin
  SizeX := MaxX - MinX;
  SizeY := MaxY - MinY;

  { When ScaleX/Y = 1, we see WSize and HSize points on graph.
    But we want to see SizeX and SizeY.
    Also we don't want ScaleX/Y to be too little (this makes a lot of trouble,
    and OpenGL rendering also has trouble and crawls surprisingly slowly).

    By default we set ScaleX and ScaleY to be equal, as this is most
    natural for user. }
  ScaleX := KambiUtils.Max(KambiUtils.Min(WSize / SizeX, HSize / SizeY), 0.01);
  ScaleY := ScaleX;

  MiddleX := (MinX + MaxX) / 2;
  MiddleY := (MinY + MaxY) / 2;

  { MoveX/Y to look at MiddleX/Y.
    I want XGLWinToUklad(WSize/2) = MiddleX.
    Solve equation (WSize/2-MoveX)/ScaleX = MiddleX for MoveX and you got
    what you need. }
  MoveX := WSize/2 - MiddleX * ScaleX;
  MoveY := HSize/2 - MiddleY * ScaleY;
 end;

 glw.PostRedisplay;
end;

{ funkcje XYGLWinToUklad pobieraja pozycje we wspolrzednej okna OpenGL'a
  (czyli 0..WSize lub HSize) i zwracaja jaka jest pozycja punktu na wykresie
  w tym miejscu okna. Nie biora pod uwage Rotate. }
function XGLWinToUklad(glWinX: TGLfloat): TGLfloat;
begin result:=(glWinX-MoveX)/ScaleX end;

function YGLWinToUklad(glWinY: TGLfloat): TGLfloat;
begin result:=(glWinY-MoveY)/ScaleY end;

{ X/YPixels : podajesz argumet = ilosc pixeli. Zwraca ile dla OpenGL'a (biorac
  pod uwage aktualne projection matrix) znaczy tyle pixeli (jaka to jest dlugosc
  dla OpenGL'a). Nie bierze pod uwage aktualnego modelview matrix (a wiec
  dziala jakby MoveX = MoveY = 0 i ScaleX/Y = 1). }
function XPixels(pixX: TGLfloat): TGLfloat;
begin result := pixX*WSize/glw.width end;

function YPixels(pixY: TGLfloat): TGLfloat;
begin result := pixY*HSize/glw.height end;

var
  GraphsListMenu: TMenu;

procedure UpdateGraphsMenu;
var
  I: Integer;
  CharKey: char;
begin
  while GraphsListMenu.EntriesCount > 3 do
    GraphsListMenu.EntryDelete(3);

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

{ This creates new TGraph instance and adds it to Graphs list.
  If loading graph from file fails, it will display nice MessageOK dialog.

  It also takes care of setting AColorNumer parameter for TGraph.Create
  as it should be. }
procedure GraphsAdd(const FileName: string);
var
  G: TGraph;
begin
  try
    G := TGraph.Create(FileName, Graphs.Count);
  except
    on E: Exception do
    begin
      MessageOK(Glw, Format('Error when opening graph from file "%s": %s',
        [FileName, E.Message]), taLeft);
      Exit;
    end;
  end;

  Graphs.Add(G);
end;

{ registered glw callbacks -------------------------------------------------- }

procedure Draw(glwin: TGLWindow);

  procedure ShowGridPodzialka(
    const krok: extended; const LiczbowyString: string;
    showGrid, showPodzialka, showPodzialkaLiczby: boolean;
    const gridKol, podzialkaKol, podzialkaLiczbyKol: TVector3f);
  { ShowGridPodzialka rysuje siatke, podzialke i podzialke liczbowa
    co krok. Odpowiednie parametry showXxx okreslaja co dokladnie narysowac -
    dowolna kombinacja trzech powyzszych elementow moze byc wiec narysowana.
    Parametry xxxKol okreslaja kolory odpowiednich elementow.
    Parametr LiczbowyString ma zastosowanie tylko do rysowania podzialki liczbowej.
    Przy kazdej kresce na osi bedzie pisany tekst Format(LiczbowyString, [i, i*krok])
     gdzie i to wielokrotnosc kroku o jaka dana kreska jest wysunieta.
     Przykladowe wartosci LiczbowyTekst to '%d', '%d*Pi', '%1: f'. }
  var i, minx, maxx, miny, maxy: integer;
  const podzialkaLength =10;
  begin
   if not (showGrid or showPodzialka or showPodzialkaLiczby) then exit;

   { minx to najmniejsza wartosc taka ze minx*krok miesci sie na ekranie.
     Podobnie maxx, miny, maxy. }
   minx := Ceil(XGLWinToUklad(0) / krok);
   miny := Ceil(YGLWinToUklad(0) / krok);
   maxx := Floor(XGLWinToUklad(WSize) / krok);
   maxy := Floor(YGLWinToUklad(HSize) / krok);

   if ShowGrid then
   begin
    glColorv(gridKol);
    glBegin(GL_LINES);
    { przesuwajac sie o wartosc integer unikamy przy okazji kumulacji bledow
      zaokraglen zmiennoprzecinkowych gdybysmy przesuwali sie o krok
      zmieniajac jakas wartosc o +krok w kazdym kroku petli. }
    for i := minx to maxx do
    begin
     glVertex2f(i*krok, YGLWinToUklad(0)); glVertex2f(i*krok, YGLWinToUklad(HSize));
    end;
    for i := miny to maxy do
    begin
     glVertex2f(XGLWinToUklad(0), i*krok); glVertex2f(XGLWinToUklad(WSize), i*krok);
    end;
    glEnd;
   end;

   if ShowPodzialka then
   begin
    glColorv(podzialkaKol);
    glBegin(GL_LINES);
    { przesuwajac sie o wartosc integer unikamy przy okazji kumulacji bledow
      zaokraglen zmiennoprzecinkowych gdybysmy przesuwali sie o krok
      zmieniajac jakas wartosc o +krok w kazdym kroku petli. }
    for i := minx to maxx do
    begin
     glVertex2f(i*krok, -YPixels(podzialkaLength)/ScaleY);
     glVertex2f(i*krok,+ YPixels(podzialkaLength)/ScaleY);
    end;
    for i := miny to maxy do
    begin
     glVertex2f(-XPixels(podzialkaLength)/ScaleX, i*krok);
     glVertex2f(+XPixels(podzialkaLength)/ScaleX, i*krok);
    end;
    glEnd;
   end;

   if ShowPodzialkaLiczby then
   begin
    glColorv(podzialkaLiczbyKol);
    for i := minx to maxx do
    begin
     glRasterPos2f(i*krok, 0); Font.PrintFmt(LiczbowyString, [i, i*krok]);
    end;
    for i := miny to maxy do
    begin
     glRasterPos2f(0, i*krok); Font.PrintFmt(LiczbowyString, [i, i*krok]);
    end;
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
      if not points.Items[i].break then
       glVertex2f(points.Items[i].x, points.Items[i].y);
     glEnd;
     glPointSize(1);
    end else
    begin
     glBegin(GL_LINE_STRIP);
      for i := 0 to Points.Count-1 do
       if points.Items[i].break then
       begin
        glEnd;
        glBegin(GL_LINE_STRIP);
       end else
        glVertex2f(points.Items[i].x, points.Items[i].y);
     glEnd;
    end;

    if BoolOptions[boPointsCoords] then
    begin
     for i := 0 to Points.Count-1 do
     if not points.Items[i].break then
     begin
      glRasterPos2f(points.Items[i].x, points.Items[i].y);
      Font.PrintFmt('(%f,%f)', [points.Items[i].x, points.Items[i].y]);
     end;
    end;
   end;
  end;

var i, j: integer;
    h: TGLfloat;
begin
 glClear(GL_COLOR_BUFFER_BIT);
 glLoadIdentity;

 {glRotate jest otoczone przez WSize/2, HSize/2 zeby obrot byl wzgledem srodka okienka}
 glTranslatef(WSize/2, HSize/2, 0);
 glRotatef(Rotate, 0, 0, 1);
 glTranslatef(-WSize/2, -HSize/2, 0);

 glTranslatef(MoveX, MoveY, 0);
 glScalef(ScaleX, ScaleY, 0);

 ShowGridPodzialka(1, '%d',
   BoolOptions[boGrid1],   BoolOptions[boPodzialka1],   BoolOptions[boLiczby1],
   ColorScheme^[ciGrid1]^,  ColorScheme^[ciPodzialka1]^,  ColorScheme^[ciLiczby1]^);
 ShowGridPodzialka(Pi, '%d*Pi',
   BoolOptions[boGridPi],  BoolOptions[boPodzialkaPi],  BoolOptions[boLiczbyPi],
   ColorScheme^[ciGridPi]^, ColorScheme^[ciPodzialkaPi]^, ColorScheme^[ciLiczbyPi]^);
 ShowGridPodzialka(CustomSize, '%1:f',
   BoolOptions[boGridCustom],  BoolOptions[boPodzialkaCustom],  BoolOptions[boLiczbyCustom],
   ColorScheme^[ciGridCustom]^, ColorScheme^[ciPodzialkaCustom]^, ColorScheme^[ciLiczbyCustom]^);

 if BoolOptions[boOsieXY] then
 begin
  glColorv(ColorScheme^[ciOsieXY]^);
  glBegin(GL_LINES);
   glVertex2f(0, YGLWinToUklad(0)); glVertex2f(0, YGLWinToUklad(HSize));
   glVertex2f(XGLWinToUklad(0), 0); glVertex2f(XGLWinToUklad(WSize), 0);
  glEnd;
 end;

 for j := 0 to Graphs.Count-1 do ShowGraph(Graphs[j]);

 if BoolOptions[boCrosshair] then
 begin
  glLoadIdentity;
  glColorv(ColorScheme^[ciCrosshair]^);

  glLineStipple(10, $AAAA);
  glEnable(GL_LINE_STIPPLE);
  glBegin(GL_LINES);
   glVertex2f(WSize/2, 0); glVertex2f(WSize/2, HSize);
   glVertex2f(0, HSize/2); glVertex2f(WSIze, HSize/2);
  glEnd;
  glDisable(GL_LINE_STIPPLE);

  glRasterPos2f(WSize/2, HSize/2);
  Font.PrintFmt('%f, %f', [XGLWinToUklad(WSize/2), YGLWinToUklad(HSize/2)]);
 end;

 if BoolOptions[boMap] then
 begin
  glLoadIdentity;
  glColorv(ColorScheme^[ciCrosshair]^);
  glTranslatef(XPixels(10), YPixels(5), 0);

  glRasterPos2i(0, 0);
  Font.PrintFmt('Scale %f, %f. Move %f, %f. Rotation %f. %d graphs.',
    [ScaleX, ScaleY, MoveX, MoveY, Rotate, Graphs.Count]);

  h := YPixels(Font.RowHeight);
  for i := 0 to Graphs.Count-1 do
  begin
   glColorv(Graphs[i].Color);
   glTranslatef(0, h, 0);

   glBegin(GL_LINES); glVertex2f(0, h/2); glVertex2f(5, h/2); glEnd;
   glRasterPos2f(6.5, 0);
   if Graphs[i].Visible then
    Font.PrintFmt('%d - %s', [i, Graphs[i].Name]) else
    Font.PrintFmt('{%d - %s}', [i, Graphs[i].Name]);
  end;
 end;
end;

var
  IdleFirst: boolean = true;

procedure Idle(glwin: TGLWindow);

  function SpeedFactor: TGLfloat;
  begin
   Result := glwin.FpsCompSpeed; { to make everything time-based }
   if glwin.KeysDown[K_Ctrl] then Result *= 10;
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
    MoveX:=(MoveX-WSize/2)*Multiplier + WSize/2;
    MoveY:=(MoveY-HSize/2)*Multiplier + HSize/2;

    glwin.PostRedisplay;
  end;

  procedure MultiplyGLScaleX(Multiplier: TGLfloat);
  begin
    Multiplier := Power(Multiplier, SpeedFactor);
    if ScaleX * Multiplier < 0.00001 then
      Exit;

    ScaleX *= Multiplier;

    MoveX := (MoveX-WSize/2) * Multiplier + WSize/2;

    glwin.PostRedisplay;
  end;

  procedure MultiplyGLScaleY(Multiplier: TGLfloat);
  begin
    Multiplier := Power(Multiplier, SpeedFactor);
    if ScaleY * Multiplier < 0.00001 then
      Exit;

    ScaleY *= Multiplier;

    MoveY := (MoveY-HSize/2) * Multiplier + HSize/2;

    glwin.PostRedisplay;
  end;

  procedure AddGL(var Value: TGLfloat; const Change: TGLfloat);
  begin
   Value += Change * SpeedFactor;
   glwin.PostRedisplay;
  end;

  { Interpretuje wszystkie pozostale ParStr(1) .. ParStr(ParCount) jako
    nazwy plikow (lub stdin jesli sa '-').
    Laduje z nich graphs, dodajac je do Graphs list. }
  procedure OpenGraphsFromParameters;
  var
    I: integer;
  begin
    for I := 1 to Parameters.High do
      GraphsAdd(Parameters[I]);
  end;

begin
  if IdleFirst then
  begin
    OpenGraphsFromParameters;
    UpdateGraphsMenu;
    HomeState;
    IdleFirst := false;
  end;

 with glwin do begin
  if KeysDown[K_Up] then AddGL(MoveY, -1);
  if KeysDown[K_Down] then AddGL(MoveY, +1);

  if KeysDown[K_Right] then AddGL(MoveX, -1);
  if KeysDown[K_Left] then AddGL(MoveX, +1);

  if KeysDown[K_Numpad_Minus] then MultiplyGLScale(1 / 1.1);
  if KeysDown[K_Numpad_Plus]  then MultiplyGLScale(1.1);

  if KeysDown[K_X] then
  begin
    if KeysDown[K_Shift] then
      MultiplyGLScaleX(1 / 1.1) else
      MultiplyGLScaleX(1.1);
  end;

  if KeysDown[K_Y] then
  begin
    if KeysDown[K_Shift] then
      MultiplyGLScaleY(1 / 1.1) else
      MultiplyGLScaleY(1.1);
  end;

  if KeysDown[K_PageUp] then AddGL(rotate, -1);
  if KeysDown[K_PageDown] then AddGL(rotate, +1);
 end;
end;

procedure Resize(glwin: TGLWindow);
begin
 glViewport(0, 0, glwin.Width, glwin.Height);
 WSize := 50;
 HSize := WSize * glwin.Height/glwin.Width;
 ProjectionGLOrtho(0, WSize, 0, HSize);
end;

procedure MouseMove(glwin: TGLWindow; newX, newY: integer);
begin
 if mbLeft in glwin.mousePressed then
 begin
  { zmien MoveX i MoveY o tyle o ile zmienila sie pozycja myszy od
    ostatniego MouseMove/Down }
  MoveX := MoveX + XPixels(newX-glwin.MouseX);
  MoveY := MoveY - YPixels(newY-glwin.MouseY); { y jest mierzone w przeciwna strone, stad minus }
  glw.PostRedisplay;
 end;
end;

procedure InitGL(glwin: TGLWindow);
begin
 glClearColorv(ColorScheme^[ciBG]^, 1);
 Font := TGLBitmapFont.Create(@BFNT_BitstreamVeraSansMono_m16);
end;

procedure CloseGL(glwin: TGLWindow);
begin
 FreeAndNil(Font);
end;

{ menu-related things -------------------------------------------------------- }

function GetMainMenu(): TMenu;
var
  M: TMenu;
  bo: TBoolOption;
begin
 Result := TMenu.Create('Main menu');
 M := TMenu.Create('_File');
   M.Append(TMenuItem.Create('_Open graph ...', 101, CtrlO));
   M.Append(TMenuItem.Create('_Add graph ...', 102, CtrlA));
   M.Append(TMenuSeparator.Create);
   M.Append(TMenuItem.Create('_Close all graphs', 103));
   M.Append(TMenuSeparator.Create);
   M.Append(TMenuItem.Create('_Exit',      10, CharEscape));
   Result.Append(M);
 M := TMenu.Create('_View');
   for bo := Low(bo) to High(bo) do
    M.Append(TMenuItemChecked.Create(
      BoolOptionsMenuNames[bo], 900+Ord(bo), BoolOptionsKeys[bo],
      BoolOptions[bo], true));
   Result.Append(M);
 M := TMenu.Create('_Graphs');
   M.Append(TMenuItem.Create('_Hide all graphs',      30));
   M.Append(TMenuItem.Create('_Show all graphs',      31));
   M.Append(TMenuSeparator.Create);
   GraphsListMenu := M;
   Result.Append(M);
 M := TMenu.Create('_Other');
   M.Append(TMenuItem.Create('_Restore default view',     21, K_Home));
   M.Append(TMenuItemChecked.Create('_FullScreen on/off', 22, K_F11,
     glw.FullScreen, true));
   M.Append(TMenuItem.Create('_Save screen to PNG',       23, K_F5));
   Result.Append(M);
 M := TMenu.Create('_Help');
   M.Append(TMenuItem.Create('Show _help', 5, K_F1));
   Result.Append(M);
end;

procedure MenuCommand(glwin: TGLWindow; Item: TMenuItem);

  procedure SetVisibleAll(Value: boolean);
  var i: Integer;
  begin
   for i := 0 to Graphs.Count - 1 do
    Graphs[i].Visible := Value;
   glwin.PostRedisplay;
  end;

  procedure OpenGraph;
  var
    FileName: string;
  begin
    FileName := '';
    if Glwin.FileDialog('Open graph from file', FileName, true) then
    begin
      Graphs.FreeContents;
      GraphsAdd(FileName);
      UpdateGraphsMenu;
      HomeState;
    end;
  end;

  procedure AddGraph;
  var
    FileName: string;
  begin
    FileName := '';
    if Glwin.FileDialog('Add graph from file', FileName, true) then
    begin
      GraphsAdd(FileName);
      UpdateGraphsMenu;
      { Calling HomeState is not desirable here, maybe user wants to keep
        previous view settings, to see already existing graphs as they were. }
    end;
  end;

  procedure CloseAllGraphs;
  begin
    Graphs.FreeContents;
    UpdateGraphsMenu;
    HomeState;
  end;

var bo: TBoolOption;
begin
 case Item.IntData of
  5:  MessageOK(glwin,
        ['Keys to change view (not available as menu items):',
         '  arrows      : move',
         '  +/-         : scale',
         '  x/X         : scale only horizontally',
         '  y/Y         : scale only vertically',
         '  PgUp/PgDown : rotate',
         'Hold down Ctrl to make these keys work 10x faster. ',
         '',
         'You can also move view by dragging while holding left mouse button.'],
         taLeft);
  10: glwin.Close;

  21: HomeState;
  22: glw.SwapFullScreen;
  23: glw.SaveScreenDialog(FnameAutoInc('glplotter_screen_%d.png'));

  30: SetVisibleAll(false);
  31: SetVisibleAll(true);

  101: OpenGraph;
  102: AddGraph;
  103: CloseAllGraphs;

  900..999:
    begin
     bo := TBoolOption(Item.IntData-900);
     BoolOptions[bo] := not BoolOptions[bo];
    end;
  1000..2000:
    begin
     with Graphs[Item.IntData-1000] do Visible := not Visible;
     glwin.PostRedisplay;
    end;
  else Exit;
 end;
 glwin.PostRedisplay;
end;

{ params parsing ------------------------------------------------------------ }

procedure BoolOptionsOptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
var bo: TBoolOption;
begin
 bo := TBoolOption(OptionNum div 2);
 BoolOptions[bo] := not Odd(OptionNum);
end;

procedure ParseParametersBoolOptions;
var
  bo: TBoolOption;
  Options: TDynOptionArray;
  Option: TOption;
begin
 Options := TDynOptionArray.Create;
 try
  Option.Short := #0;
  Option.Argument := oaNone;

  for bo := Low(bo) to High(bo) do
  begin
   Option.Long := BoolOptionsParamsNames[bo];
   Options.AppendItem(Option);
   Option.Long := 'no-' +BoolOptionsParamsNames[bo];
   Options.AppendItem(Option);
  end;

  ParseParameters(Options, @BoolOptionsOptionProc, nil, true);
 finally Options.Free end;
end;

const
  Version = '1.1.6';
  DisplayProgramName = 'glplotter';
  Options: array[0..4] of TOption = (
    (Short: 'h'; Long: 'help'; Argument: oaNone),
    (Short: 'c'; Long: 'custom-size'; Argument: oaRequired),
    (Short:  #0; Long: 'light'; Argument: oaNone),
    (Short:  #0; Long: 'dark'; Argument: oaNone),
    (Short: 'v'; Long: 'version'; Argument: oaNone)
  );

procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);

  function BoolOptionsParamsHelp: string;
  var bo: TBoolOption;
  begin
   Result := '';
   for bo := Low(bo) to High(bo) do
    Result += Format('  --%-20s --no-%0:s' +nl, [BoolOptionsParamsNames[bo]]);
  end;

var HelpTextParts: array[0..1]of string;
begin
 case OptionNum of
  0: begin
      HelpTextParts[0] :=
        'glplotter: plot lines.' +nl+
        nl+
        'Call as' +nl+
        '  glplotter [OPTION]... [FILE]...' +nl+
        'Each FILE is filename or "-" (meaning stdin).' +nl+
        nl+
        'Available options are:' +nl+
        HelpOptionHelp +nl+
        VersionOptionHelp +nl+
        '  --light               Set color scheme to light' +nl+
        '  --drak                Set color scheme to dark' +nl+
        '  --custom-size / -c SIZE' +nl+
        '                        Set size of custom grid etc.' +nl+
        nl+
        'Options that set initial visibility of various things:' +nl+
        BoolOptionsParamsHelp;
      HelpTextParts[1] :=
         TGLWindow.ParseParametersHelp(StandardParseOptions, true) +nl+
         nl+
         SVrmlEngineProgramHelpSuffix(DisplayProgramName, Version, true);
      { I can't simply construct HelpTextParts "on the fly" when
        calling InfoWriteParts due to bugs in FPC 1.9.6, already fixed
        in FPC 1.9.7 from 2005-02-01. }
      InfoWriteParts('glplotter help (page %d / %d)', HelpTextParts);
      ProgramBreak;
     end;
  1: CustomSize := StrToFloat(Argument);
  2: ColorScheme := @ColorSchemeLight;
  3: ColorScheme := @ColorSchemeDark;
  4: begin
      WritelnStr(Version);
      ProgramBreak;
     end;
  else raise EInternalError.Create('OptionProc');
 end;
end;

{ main ------------------------------------------------------------ }

begin
 Graphs := TGraphsList.Create;
 try
  { parse parameters }
  ParseParametersBoolOptions;
  glw.ParseParameters;
  ParseParameters(Options, @OptionProc, nil);

  { basic glw callbacks }
  glw.OnIdle := @Idle;
  glw.OnResize := @Resize;
  glw.OnInit := @InitGL;
  glw.OnClose := @CloseGL;
  glw.OnMouseMove := @MouseMove;
  glw.OnDraw := @Draw;

  { setup menu }
  glw.MainMenu := GetMainMenu;
  glw.OnMenuCommand := @MenuCommand;

  { other glw options }
  glw.FpsActive := true;
  glw.SetDemoOptions(K_None, #0, false);
  glw.Caption := 'glplotter';

  glw.Init;

  glwm.Loop;
 finally Graphs.FreeWithContents end;
end.

{
  Local Variables:
  kam-compile-release-command-unix:  "./compile.sh && mv -fv glplotter      ~/bin/"
  kam-compile-release-command-win32: "./compile.sh && mv -fv glplotter.exe c:/bin/"
  End:
}
